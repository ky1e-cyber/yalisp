#include <assert.h>
#include <ctype.h>
#include <errno.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdnoreturn.h>
#include <string.h>
#include "allocator.h"
#include "arena.h"
#include "array.h"
#include "error.h"
#include "macros.h"
#include "parser.h"
#include "token.h"
#include "utils.h"
#include "vector.h"

typedef enum { HASH_TRUE = 0, HASH_FALSE, HASH_COUNT__ } hash_literal_t;

const char* const UNEXP_LEX_ERR = "Unexpected character encountered at %lu:%lu";
const char* const NUM_LEX_ERR = "Invalid numeric literal at %lu:%lu";
const char* const FLOAT_LEX_ERR = "Invalid float literal at %lu:%lu";
const char* const FLOAT_LEX_OUTOFRANGE =
    "Float literal's value is too big at %lu:%lu";
const char* const INT_LEX_OUTOFRANGE =
    "Int literal's value is too big at %lu:%lu";
const char* const ATOM_LEX_ERR = "Invalid atom at %lu:%lu";
const char* const STR_LEX_ERR = "Invalid string literal at %lu:%lu";
const char* const STR_LEX_INVALID_ESCAPE =
    "Invalid escape in string literal at %lu:%lu";

m_macro_like_const char* const ded_true = "t";
m_macro_like_const char* const ded_false = "f";

m_macro_like_const char* const hash_literals[] =
    {[HASH_TRUE] = ded_true, [HASH_FALSE] = ded_false};

static const char* const special_define = "define";
static const char* const special_let = "let";
static const char* const special_lambda = "lambda";
static const char* const special_plus = "+";
static const char* const special_minus = "-";
static const char* const special_star = "*";
static const char* const special_slash = "/";
static const char* const special_tilde = "~";
static const char* const special_if = "if";
static const char* const special_eq = "=";

static const char* const special_neq = "/=";
static const char* const special_lt = "<";
static const char* const special_le = "<=";
static const char* const special_gt = ">";
static const char* const special_ge = ">=";

static const char* const special_not = "not";
static const char* const special_and = "and";
static const char* const special_or = "or";

static const char* const special_atoms[] = {
    [SPEC_DEFINE] = special_define, [SPEC_LET] = special_let,
    [SPEC_PLUS] = special_plus,     [SPEC_MINUS] = special_minus,
    [SPEC_STAR] = special_star,     [SPEC_SLASH] = special_slash,
    [SPEC_TILDE] = special_tilde,   [SPEC_IF] = special_if,
    [SPEC_EQ] = special_eq,         [SPEC_NEQ] = special_neq,
    [SPEC_LT] = special_lt,         [SPEC_LE] = special_le,
    [SPEC_GT] = special_gt,         [SPEC_GE] = special_ge,
    [SPEC_NOT] = special_not,       [SPEC_AND] = special_and,
    [SPEC_OR] = special_or,         [SPEC_LAMBDA] = special_lambda};

m_macro_like bool is_whitespace(int c) {
  return (c == ' ' || c == '\t' || c == '\n');
}

m_macro_like bool is_bracket(int c) {
  static const int brackets[] = {'(', ')', '{', '}', '[', ']'};
  return m_contains(c, brackets, sizeof(brackets) / sizeof(int));
}

// checks if `c` correctly separates tokens
// e.g. in 'foo(bar)' open parenthesis correctly
//          separates `foo` and should be tokenized as
//          [ ATOM(foo), ROUND_PAREN_O, ATOM(bar), ROUND_PAREN_C ]
// while in 'foo#bar' `#` can't be part of an atom
//           therefore all expression must be tokenized as INVALID
m_macro_like bool is_correcty_separating(int c) {
  return (c == EOF) || is_whitespace(c) || is_bracket(c);
}

// checks if c is a start of an atom
m_macro_like bool is_atomstart(int c) {
  static const int puncts[] = {'!', '$',  '%', '&', '*', '+', ',', '-',
                               '.', '/',  ':', ';', '<', '=', '>', '?',
                               '@', '\\', '^', '_', '`', '|', '~'};
  return isalpha(c) || (m_contains(c, puncts, sizeof(puncts) / sizeof(int)));
}

m_macro_like bool is_atompart(int c) {
  return isdigit(c) || is_atomstart(c);
}

m_macro_like bool is_ded_literal_part(int c) {
  return isalpha(c) || (c == '-');
}

static lexer_state_t g_lexer = {.file_desc = NULL,
                                .current_char = EOF,
                                .line_no = 1,
                                .line_pos = 1,
                                .eof = true};

static arena_ptr_t g_str_arena = NULL;

static noreturn void lexer_buff_error__() {
  error("Could not allocate buffer for lexer\n");
}

static void lexer_init(const char* const src_path, arena_ptr_t str_arena) {
  FILE* srcfile = fopen(src_path, "r");
  if (srcfile == NULL)
    error("Could not open file %s: %s\n", src_path, strerror(errno));

  int c = fgetc(srcfile);

  g_lexer = (lexer_state_t){.file_desc = srcfile,
                            .current_char = c,
                            .line_no = 1,
                            .line_pos = 1,
                            .eof = false};
  g_str_arena = str_arena;
}

static void lexer_deinit() {
  fclose(g_lexer.file_desc);
}

static loc_t lexer_getloc() {
  return (loc_t){.line_no = g_lexer.line_no, .line_pos = g_lexer.line_pos};
}

static token_t noval_token_at(loc_t loc, token_kind_t kind) {
  return (token_t){.kind = kind, .loc = loc};
}

static token_t invalid_at(loc_t loc, const char* const err_fmt) {
  size_t len = strlen(err_fmt);

  char* err_msg = (char*)arena_alloc(g_str_arena, sizeof(char) * (len + 24));

// :)
#pragma clang diagnostic ignored "-Wformat-security"
  snprintf(err_msg, len + 24, err_fmt, loc.line_no, loc.line_pos);

  token_t tk = noval_token_at(loc, TK_INVALID);
  tk.err_msg = err_fmt;
  return tk;
}

static int lexer_next_char_peek() {
  return g_lexer.current_char;
}

static int lexer_next_char_consume() {
  assert(!g_lexer.eof &&
         "lexer_next_char_consume called after EOF encountered");

  int c = g_lexer.current_char;
  g_lexer.line_pos++;

  if (c == EOF) {
    g_lexer.eof = true;
    return c;
  }

  if (c == '\n') {
    g_lexer.line_no++;
    g_lexer.line_pos = 1;
  }

  g_lexer.current_char = fgetc(g_lexer.file_desc);

  return c;
}

static token_t lexer_tokenize_atom_tail__(loc_t loc, vector_ptr_t buf) {
  for (size_t i = 0; i < SPEC_COUNT__; i++) {
    char* s = vector_data(char, buf);
    assert(special_atoms[i] != NULL && "special_atoms map has gaps");
    if (strcmp(s, special_atoms[i]) == 0) {
      return (token_t){.kind = TK_SPECIAL_ATOM,
                       .loc = loc,
                       .value.as_special_atom = (special_atom_t)i};
    }
  }

  char* const atom_val = (char*)arena_alloc(g_str_arena, vector_size(buf));

  size_t dst_ind = 0;
  for (char* src = vector_begin(char, buf); src < vector_end(char, buf);
       src++) {
    atom_val[dst_ind] = *src;
    dst_ind++;
  }

  return (token_t){.kind = TK_ATOM, .loc = loc, .value.as_cstr = atom_val};
}

static token_t lexer_tokenize_atom() {
  loc_t loc = lexer_getloc();

  vector_ptr_t m_cleanup(vector_cleanup) buf =
      vector_make(char, alloc_default, lexer_buff_error__);

  int c = lexer_next_char_peek();
  while (is_atompart(c)) {
    buf = vector_push_back(char, buf, (char)lexer_next_char_consume());
    c = lexer_next_char_peek();
  }

  if (!is_correcty_separating(c)) {
    lexer_next_char_consume();
    return invalid_at(loc, ATOM_LEX_ERR);
  }

  buf = vector_push_back(char, buf, '\0');

  return lexer_tokenize_atom_tail__(loc, buf);
}

m_macro_like token_t lexer_tokenize_float_e_tail__(char* buf,
                                                   size_t buf_pos,
                                                   loc_t loc) {
  int c = lexer_next_char_peek();
  while (isdigit(c)) {
    buf[buf_pos++] = (char)lexer_next_char_consume();
    c = lexer_next_char_peek();
  }

  if (!is_correcty_separating(c)) {
    lexer_next_char_consume();
    return invalid_at(loc, FLOAT_LEX_ERR);
  }

  buf[buf_pos] = '\0';

  char* end__;
  errno = 0;
  double val = strtod(buf, &end__);

  if (errno == ERANGE)
    return invalid_at(loc, FLOAT_LEX_OUTOFRANGE);

  return (token_t){.kind = TK_FLOAT_LITERAL, .loc = loc, .value.as_f64 = val};
}

m_macro_like token_t lexer_tokenize_float_point_tail__(char* buf,
                                                       size_t buf_pos,
                                                       loc_t loc) {
  int c = lexer_next_char_peek();
  while (isdigit(c)) {
    buf[buf_pos++] = (char)lexer_next_char_consume();
    c = lexer_next_char_peek();
  }

  if (c == 'e') {
    buf[buf_pos++] = (char)lexer_next_char_consume();
    char nxt = lexer_next_char_peek();
    if (!isdigit(nxt))
      return invalid_at(loc, FLOAT_LEX_ERR);

    return lexer_tokenize_float_e_tail__(buf, buf_pos, loc);
  }

  if (!is_correcty_separating(c)) {
    lexer_next_char_consume();
    return invalid_at(loc, FLOAT_LEX_ERR);
  }

  buf[buf_pos] = '\0';

  char* end__;
  errno = 0;
  double val = strtod(buf, &end__);

  if (errno == ERANGE) {
    return invalid_at(loc, FLOAT_LEX_OUTOFRANGE);
  }

  return (token_t){.kind = TK_FLOAT_LITERAL, .loc = loc, .value.as_f64 = val};
}

static token_t lexer_tokenize_num() {
  loc_t loc = lexer_getloc();

  char buf[64];  // to be safe
  size_t buf_pos = 0;

  int c = lexer_next_char_peek();
  while (isdigit(c)) {
    buf[buf_pos++] = (char)lexer_next_char_consume();
    c = lexer_next_char_peek();
  }

  if (c == '.') {
    buf[buf_pos++] = (char)lexer_next_char_consume();
    int nxt = lexer_next_char_peek();
    if (!isdigit(nxt))
      return invalid_at(loc, FLOAT_LEX_ERR);

    return lexer_tokenize_float_point_tail__(buf, buf_pos, loc);
  }

  if (!is_correcty_separating(c)) {
    lexer_next_char_consume();
    return invalid_at(loc, NUM_LEX_ERR);
  }

  buf[buf_pos] = '\0';

  char* end__;
  errno = 0;
  int64_t val = (int64_t)strtoll(buf, &end__, 10);

  if (errno == ERANGE)
    return invalid_at(loc, INT_LEX_OUTOFRANGE);

  return (token_t){.kind = TK_INT_LITERAL, .loc = loc, .value.as_i64 = val};
}

m_macro_like token_t
lexer_tokenize_str_readrest_invalid_tail__(loc_t loc,
                                           const char* const err_msg) {
  int c = lexer_next_char_peek();
  while (c != '\"' && c != EOF) {
    lexer_next_char_consume();
    c = lexer_next_char_peek();
  }

  return invalid_at(loc, err_msg);
}

static token_t lexer_tokenize_str() {
  static const int valid_escapes[] = {'e', 'r', 't',  'a',  'f', 'v',
                                      'b', 'n', '\\', '\"', '\''};
  static const char escapes_map[] = {
      ['e'] = '\e',  ['r'] = '\r',  ['t'] = '\t', ['a'] = '\a',
      ['f'] = '\f',  ['v'] = '\v',  ['b'] = '\b', ['n'] = '\n',
      ['\\'] = '\\', ['\"'] = '\"', ['\''] = '\''};

  loc_t loc = lexer_getloc();

  vector_ptr_t m_cleanup(vector_cleanup) buf =
      vector_make(char, alloc_default, lexer_buff_error__);

  lexer_next_char_consume();  // consume '\"'
  int c = lexer_next_char_peek();
  while (true) {
    if (c == '\"') {
      lexer_next_char_consume();
      break;
    }

    if (c == EOF)
      return invalid_at(loc, STR_LEX_ERR);

    if (c == '\\') {
      lexer_next_char_consume();
      c = lexer_next_char_peek();

      if (m_contains(c, valid_escapes, sizeof(valid_escapes) / sizeof(int))) {
        c = escapes_map[c];
      } else {
        return lexer_tokenize_str_readrest_invalid_tail__(
            loc, STR_LEX_INVALID_ESCAPE);
      }
    }

    buf = vector_push_back(char, buf, (char)c);

    lexer_next_char_consume();
    c = lexer_next_char_peek();
  }

  buf = vector_push_back(char, buf, '\0');

  char* const strval = (char*)arena_alloc(g_str_arena, vector_size(buf));
  char* dst = strval;

  for (char* src = vector_begin(char, buf); src < vector_end(char, buf);
       src++) {
    *(dst++) = *src;
  }

  return (token_t){.kind = TK_STR_LITERAL, .loc = loc, .value.as_cstr = strval};
}

static token_t lexer_tokenize_hash_literal() {
  loc_t loc = lexer_getloc();

  vector_ptr_t buf m_cleanup(vector_cleanup) =
      vector_make(char, alloc_default, lexer_buff_error__);

  lexer_next_char_consume();
  int c = lexer_next_char_peek();
  while (is_ded_literal_part(c)) {
    buf = vector_push_back(char, buf, (char)lexer_next_char_consume());
    c = lexer_next_char_peek();
  }

  buf = vector_push_back(char, buf, '\0');

  for (size_t i = 0; i < HASH_COUNT__; i++) {
    assert(hash_literals[i] != NULL && "hash_literal map has gaps");
    if (strcmp(vector_data(char, buf), hash_literals[i]) == 0) {
      switch ((hash_literal_t)i) {
        case HASH_TRUE:
          return (token_t){
              .kind = TK_BOOL_LITERAL, .value.as_bool = true, .loc = loc};
        case HASH_FALSE:
          return (token_t){
              .kind = TK_BOOL_LITERAL, .value.as_bool = false, .loc = loc};
        case HASH_COUNT__:
          m_unreachable;
          break;
      }
    }
  }

  return invalid_at(loc, "Invalid hash-literal");
}

static token_t lexer_next_token() {
  assert(!g_lexer.eof &&
         "lexer_next_token_consume called after EOF encountered");

  static const token_kind_t bracket_map[] = {
      ['('] = TK_ROUND_PAREN_O, [')'] = TK_ROUND_PAREN_C,
      ['{'] = TK_CURLY_PAREN_O, ['}'] = TK_CURLY_PAREN_C,
      ['['] = TK_SQ_PAREN_O,    [']'] = TK_SQ_PAREN_C};

  int c = lexer_next_char_peek();
  while (is_whitespace(c)) {
    lexer_next_char_consume();
    c = lexer_next_char_peek();
  }

  loc_t loc = lexer_getloc();

  if (is_atomstart(c))
    return lexer_tokenize_atom();

  if (c == EOF) {
    lexer_next_char_consume();
    return noval_token_at(loc, TK_EOS);
  }

  if (c == '#')
    return lexer_tokenize_hash_literal();

  if (c == '\'') {
    lexer_next_char_consume();
    return noval_token_at(loc, TK_QUOTE);
  }

  if (is_bracket(c))
    return noval_token_at(loc, bracket_map[lexer_next_char_consume()]);

  if (isdigit(c))
    return lexer_tokenize_num();

  if (c == '\"')
    return lexer_tokenize_str();

  token_t invalid = invalid_at(lexer_getloc(), UNEXP_LEX_ERR);
  lexer_next_char_consume();

  return invalid;
}

static void fpprint_token(FILE* stream, token_t tk) {
  static const char* const kinds[] = {[TK_INVALID] = "INVALID",
                                      [TK_EOS] = "EOS",
                                      [TK_ROUND_PAREN_O] = "ROUND_PAREN_O",
                                      [TK_ROUND_PAREN_C] = "ROUND_PAREN_C",
                                      [TK_SQ_PAREN_O] = "SQ_PAREN_O",
                                      [TK_SQ_PAREN_C] = "SQ_PAREN_C",
                                      [TK_CURLY_PAREN_O] = "CURLY_PAREN_O",
                                      [TK_CURLY_PAREN_C] = "CURLY_PAREN_C",
                                      [TK_ATOM] = "ATOM",
                                      [TK_SPECIAL_ATOM] = "SPECIAL_ATOM",
                                      [TK_QUOTE] = "QUOTE",
                                      [TK_INT_LITERAL] = "INT_LITERAL",
                                      [TK_FLOAT_LITERAL] = "FLOAT_LITERAL",
                                      [TK_STR_LITERAL] = "STR_LITERAL",
                                      [TK_BOOL_LITERAL] = "BOOL_LITERAL"

  };

  static_assert(sizeof(kinds) / sizeof(char*) == TK_COUNT__,
                "Missing token kind in fpprint token");

  fprintf(stream, "Kind: %s\n", kinds[tk.kind]);
  fprintf(stream, "Location: line_no = %lu, line_pos = %lu\n", tk.loc.line_no,
          tk.loc.line_pos);

  static const char* const format_f64_val = "Value: %lf\n";
  static const char* const format_i64_val = "Value: %lli\n";
  static const char* const format_cstr_val = "Value: %s\n";
  static const char* const format_cstr_val_str = "Value: \"%s\"\n";

  switch (tk.kind) {
    case TK_ATOM:
      fprintf(stream, format_cstr_val, tk.value.as_cstr);
      break;
    case TK_STR_LITERAL:
      fprintf(stream, format_cstr_val_str, tk.value.as_cstr);
      break;
    case TK_INT_LITERAL:
      fprintf(stream, format_i64_val, tk.value.as_i64);
      break;
    case TK_FLOAT_LITERAL:
      fprintf(stream, format_f64_val, tk.value.as_f64);
      break;
    case TK_BOOL_LITERAL:
      fprintf(stream, format_cstr_val, tk.value.as_bool ? "#t" : "#f");
      break;
    case TK_SPECIAL_ATOM:
      fprintf(stream, format_cstr_val, special_atoms[tk.value.as_special_atom]);
      break;
    case TK_INVALID:
      fprintf(stream, "Error message: %s\n", tk.err_msg);
    default:
      break;
  }

  fprintf(stream, "\n");
}

static void lexer_deinit_defer__([[maybe_unused]] int* blank) {
  lexer_deinit();
}

void fdump_tokens(FILE* stream,
                  const char* const src_path,
                  arena_ptr_t str_arena) {
  lexer_init(src_path, str_arena);
  m_defer(lexer_deinit_defer__);

  token_t tk = lexer_next_token();
  while (tk.kind != TK_EOS) {
    fpprint_token(stream, tk);
    tk = lexer_next_token();
  }

  fpprint_token(stream, tk);
  fprintf(stream, "\n");
}

static parse_tree_t* pt_make_node(arena_ptr_t arena,
                                  loc_t loc,
                                  pt_node_kind_t kind,
                                  pt_value_t value,
                                  const char* err_msg) {
  parse_tree_t* pt = (parse_tree_t*)arena_alloc(arena, sizeof(parse_tree_t));

  pt->loc = loc;
  pt->kind = kind;
  pt->value = value;
  pt->err_msg = err_msg;

  return pt;
}

static parse_tree_t* pt_make_toplevel_node(
    arena_ptr_t arena,
    loc_t loc,
    array_ptr_t /* [parse_tree_t*] */ toplevel_list) {
  return pt_make_node(arena, loc, PT_TOPLEVEL,
                      (pt_value_t){.as_list = toplevel_list}, NULL);
}

static parse_tree_t* pt_make_list_node(arena_ptr_t arena,
                                       loc_t loc,
                                       array_ptr_t /* [parse_tree_t*] */ list) {
  return pt_make_node(arena, loc, PT_LIST, (pt_value_t){.as_list = list}, NULL);
}

static parse_tree_t* pt_make_quoted_node(arena_ptr_t arena,
                                         loc_t loc,
                                         parse_tree_t* subtree) {
  return pt_make_node(arena, loc, PT_QUOTED,
                      (pt_value_t){.as_subtree = subtree}, NULL);
}

static parse_tree_t* pt_make_atom_node(arena_ptr_t arena,
                                       loc_t loc,
                                       token_t atom_token) {
  assert(atom_token.kind == TK_ATOM || atom_token.kind == TK_SPECIAL_ATOM);
  return pt_make_node(arena, loc, PT_ATOM, (pt_value_t){.as_token = atom_token},
                      NULL);
}

static parse_tree_t* pt_make_literal_node(arena_ptr_t arena,
                                          loc_t loc,
                                          token_t literal_token) {
  const token_kind_t literal_kinds[] = {TK_BOOL_LITERAL, TK_FLOAT_LITERAL,
                                        TK_INT_LITERAL, TK_STR_LITERAL};
  assert(m_contains(literal_token.kind, literal_kinds,
                    sizeof(literal_kinds) / sizeof(token_kind_t)));
  return pt_make_node(arena, loc, PT_LITERAL,
                      (pt_value_t){.as_token = literal_token}, NULL);
}

static parse_tree_t* pt_make_error_node(arena_ptr_t arena,
                                        loc_t loc,
                                        const char* err_msg) {
  return pt_make_node(arena, loc, PT_ERROR, (pt_value_t){}, err_msg);
}

static parser_state_t g_parser = {
    .current_token = {.kind = TK_EOS, .loc = {.line_no = 0, .line_pos = 0}},
    .eos = true};

static arena_ptr_t g_pt_arena = NULL;

static void parser_init(const char* const src_path,
                        arena_ptr_t str_arena,
                        arena_ptr_t pt_arena) {
  lexer_init(src_path, str_arena);
  g_pt_arena = pt_arena;

  token_t tk = lexer_next_token();
  g_parser = (parser_state_t){.current_token = tk, .eos = false};
}

static void parser_deinit() {
  lexer_deinit();
}

static token_t parser_next_token_peek() {
  return g_parser.current_token;
}

static token_t parser_next_token_consume() {
  token_t tk = parser_next_token_peek();

  if (tk.kind == TK_EOS) {
    g_parser.eos = true;
    return tk;
  };

  g_parser.current_token = lexer_next_token();

  return tk;
}

static parse_tree_t* parse_expr();
static parse_tree_t* parse_list();
static parse_tree_t* parse_toplevel();

static noreturn void parser_buf_error__() {
  error("Couldn't allocate buffer for parser\n");
}

static parse_tree_t* parse_seq(const bool is_toplevel) {
  token_t tk = parser_next_token_peek();

  loc_t loc = tk.loc;

  if (!is_toplevel) {
    assert(tk.kind == TK_ROUND_PAREN_O);
    parser_next_token_consume();
    tk = parser_next_token_peek();
  }

  vector_ptr_t buf m_cleanup(vector_cleanup) =
      vector_make(parse_tree_t*, alloc_default, parser_buf_error__);

  const token_kind_t close = is_toplevel ? TK_EOS : TK_ROUND_PAREN_C;

  while (tk.kind != close) {
    if (!is_toplevel && tk.kind == TK_EOS) {
      static char err_fmt[] = "Unclosed list encountered at %lu:%lu";
      char* err_msg =
          (char*)arena_alloc(g_str_arena, sizeof(err_fmt) + 24 * sizeof(char));
      snprintf(err_msg, sizeof(err_fmt) / sizeof(char) + 24, err_fmt,
               loc.line_no, loc.line_pos);
      return pt_make_error_node(g_pt_arena, loc, err_msg);
    }

    parse_tree_t* nxt = parse_expr();
    buf = vector_push_back(parse_tree_t*, buf, nxt);

    tk = parser_next_token_peek();
  }

  parser_next_token_consume();

  size_t lst_sz = vector_size(buf);

  array_ptr_t lst =
      array_init((array_ptr_t)arena_alloc(
                     g_pt_arena, array_bytesize(parse_tree_t*, lst_sz)),
                 lst_sz);
  array_fill_from(parse_tree_t*, lst, vector_data(parse_tree_t*, buf), lst_sz);

  return is_toplevel
             ? pt_make_toplevel_node(g_pt_arena,
                                     (loc_t){.line_no = 1, .line_pos = 1}, lst)
             : pt_make_list_node(g_pt_arena, loc, lst);
}

static parse_tree_t* parse_list() {
  return parse_seq(false);
}

static parse_tree_t* parse_expr() {
  token_t tk = parser_next_token_peek();
  loc_t loc = tk.loc;

  if (tk.kind == TK_ROUND_PAREN_O)
    return parse_list();

  if (tk.kind == TK_INT_LITERAL || tk.kind == TK_BOOL_LITERAL ||
      tk.kind == TK_FLOAT_LITERAL || tk.kind == TK_STR_LITERAL) {
    parser_next_token_consume();
    return pt_make_literal_node(g_pt_arena, loc, tk);
  }

  if (tk.kind == TK_ATOM || tk.kind == TK_SPECIAL_ATOM) {
    parser_next_token_consume();
    return pt_make_atom_node(g_pt_arena, loc, tk);
  }

  if (tk.kind == TK_QUOTE) {
    parser_next_token_consume();
    return pt_make_quoted_node(g_pt_arena, loc, parse_expr());
  }

  if (tk.kind == TK_INVALID) {
    parser_next_token_consume();
    return pt_make_error_node(g_pt_arena, loc, tk.err_msg);
  }

  parser_next_token_consume();

  static const char err_fmt[] =
      "Unexpected token encountered while parsing expression at: %lu:%lu";
  char* err_msg =
      (char*)arena_alloc(g_str_arena, sizeof(err_fmt) + 24 * sizeof(char));

  snprintf(err_msg, sizeof(err_fmt) / sizeof(char) + 24, err_fmt, loc.line_no,
           loc.line_pos);

  return pt_make_error_node(g_pt_arena, loc, err_msg);
}

static parse_tree_t* parse_toplevel() {
  return parse_seq(true);
}

static void fprint_literal_token(FILE* stream, token_t tk) {
  assert(tk.kind == TK_BOOL_LITERAL || tk.kind == TK_INT_LITERAL ||
         tk.kind == TK_FLOAT_LITERAL || tk.kind == TK_STR_LITERAL);

  switch (tk.kind) {
    case TK_BOOL_LITERAL:
      fprintf(stream, tk.value.as_bool ? "#t" : "#f");
      break;
    case TK_INT_LITERAL:
      fprintf(stream, "%lli", tk.value.as_i64);
      break;
    case TK_FLOAT_LITERAL:
      fprintf(stream, "%lf", tk.value.as_f64);
      break;

    case TK_STR_LITERAL:
      fprintf(stream, "\"%s\"", tk.value.as_cstr);
      break;

    default:
      m_unreachable;
      break;
  }
}

static void fpprint_parse_tree(FILE* stream, parse_tree_t* pt) {
  fprintf(stream, "{%lu:%lu}", pt->loc.line_no, pt->loc.line_pos);

  switch (pt->kind) {
    case PT_TOPLEVEL:;
      array_ptr_t toplvl = pt->value.as_list;
      for (parse_tree_t** it = array_begin(parse_tree_t*, toplvl);
           it < array_end(parse_tree_t*, toplvl); it++) {
        fpprint_parse_tree(stream, *it);
        fprintf(stream, "\n");
      }
      break;

    case PT_ERROR:
      fprintf(stream, "ERROR(%s)", pt->err_msg);
      break;
    case PT_ATOM:;
      token_t atom = pt->value.as_token;
      assert(atom.kind == TK_SPECIAL_ATOM || atom.kind == TK_ATOM);
      if (atom.kind == TK_SPECIAL_ATOM) {
        fprintf(stream, "$%s", special_atoms[atom.value.as_special_atom]);
      } else {
        fprintf(stream, "%s", atom.value.as_cstr);
      }
      break;
    case PT_LIST:
      fprintf(stream, "(\n");
      array_ptr_t lst = pt->value.as_list;

      for (parse_tree_t** it = array_begin(parse_tree_t*, lst);
           it < array_end(parse_tree_t*, lst); it++) {
        fpprint_parse_tree(stream, *it);
        fprintf(stream, "\n");
      }

      fprintf(stream, ")\n");

      break;
    case PT_QUOTED:
      fprintf(stream, "\' ");
      fpprint_parse_tree(stream, pt->value.as_subtree);
      break;

    case PT_LITERAL:;
      token_t lit = pt->value.as_token;
      fprint_literal_token(stream, lit);
      break;
  }
}

void parser_deinit_defer__([[maybe_unused]] int* blank) {
  parser_deinit();
}

void fdump_parse_tree(FILE* stream,
                      const char* const src_path,
                      arena_ptr_t str_arena,
                      arena_ptr_t pt_arena) {
  parse_tree_t* pt = parse(src_path, str_arena, pt_arena);
  fpprint_parse_tree(stream, pt);
}

parse_tree_t* parse(const char* const path,
                    arena_ptr_t str_arena,
                    arena_ptr_t pt_arena) {
  static_assert((sizeof(special_atoms) / sizeof(char*)) == SPEC_COUNT__,
                "special_atoms map doesn't contain all possible special atoms");
  static_assert(
      (sizeof(hash_literals) / sizeof(char*)) == HASH_COUNT__,
      "ded_literals map doesn't contain all possible dedicated literals");

  parser_init(path, str_arena, pt_arena);
  m_defer(parser_deinit_defer__);

  return parse_toplevel();
}
