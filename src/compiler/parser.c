#include "defs.h"
#pragma clang diagnostic ignored "-Wformat-security"

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
#include "program_tree.h"
#include "error.h"
#include "macros.h"
#include "parser.h"
#include "shared.h"
#include "table.h"
#include "token.h"
#include "vector.h"

typedef enum { HASH_TRUE = 0, HASH_FALSE, HASH_COUNT__ } hash_literal_t;

static const char* const UNEXP_LEX_ERR_FMT =
    "Unexpected character encountered at %lu:%lu";
static const char* const NUM_LEX_ERR_FMT = "Invalid numeric literal at %lu:%lu";
static const char* const INT_LEX_OUTOFRANGE_FMT =
    "Int literal's value is too big at %lu:%lu";
static const char* const ATOM_LEX_ERR_FMT = "Invalid atom at %lu:%lu";
static const char* const STR_LEX_ERR_FMT = "Invalid string literal at %lu:%lu";
static const char* const STR_LEX_INVALID_ESCAPE_FMT =
    "Invalid escape in string literal at %lu:%lu";

static char UNCLOSED_LIST_ERR_FMT[] = "Unclosed list encountered at %lu:%lu";

static const char* const UNEXP_FMT = "Unexpected token at %lu:%lu";
static const char* const UNEXP_SPECIAL_ATOM_FMT =
    "Unexpected special atom encountered at %lu:%lu";

static const char* const MALFORMED_FORM_FMT = "Malformed form at %lu:%lu";
static const char* const UNDEFINED_NAME_FMT = "Undefined name at %lu:%lu";

static const char* const hash_true = "t";
static const char* const hash_false = "f";

static const char* const hash_literals[] =
    {[HASH_TRUE] = hash_true, [HASH_FALSE] = hash_false};

static const char* const special_define = "define";
static const char* const special_let = "let";
static const char* const special_lambda = "lambda";
static const char* const special_plus = "+";
static const char* const special_minus = "-";
static const char* const special_star = "*";
static const char* const special_slash = "/";
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

static const char* const special_do = "do";

static const char* const special_atoms[] = {
    [SPEC_DEFINE] = special_define, [SPEC_LET] = special_let,
    [SPEC_PLUS] = special_plus,     [SPEC_MINUS] = special_minus,
    [SPEC_STAR] = special_star,     [SPEC_SLASH] = special_slash,
    [SPEC_IF] = special_if,         [SPEC_EQ] = special_eq,
    [SPEC_NEQ] = special_neq,       [SPEC_LT] = special_lt,
    [SPEC_LE] = special_le,         [SPEC_GT] = special_gt,
    [SPEC_GE] = special_ge,         [SPEC_NOT] = special_not,
    [SPEC_AND] = special_and,       [SPEC_OR] = special_or,
    [SPEC_LAMBDA] = special_lambda, [SPEC_DO] = special_do,
    [SPEC_VECTOR] = special_do};

static const special_atom_t spec_binops[] = {
    SPEC_PLUS, SPEC_MINUS, SPEC_STAR, SPEC_SLASH, SPEC_LE,
    SPEC_LT,   SPEC_GE,    SPEC_GT,   SPEC_EQ,    SPEC_NEQ};

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
  static const int puncts[] = {'!', '$', '%', '&',  '*', '+', ',',
                               '-', '.', '/', ':',  ';', '<', '=',
                               '>', '?', '@', '\\', '^', '`', '|'};
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

static arena_ptr_t g_lexer_str_arena = NULL;

static noreturn void error_lexer_buf() {
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
  g_lexer_str_arena = str_arena;
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

static token_t invalid_token_at(loc_t loc, const char* const err_fmt) {
  size_t len = strlen(err_fmt);

  char* err_msg =
      (char*)arena_alloc(g_lexer_str_arena, sizeof(char) * (len + 24));

  snprintf(err_msg, len + 24, err_fmt, loc.line_no, loc.line_pos);

  token_t tk = noval_token_at(loc, TK_INVALID);
  tk.err_msg = err_msg;
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

  char* const atom_val =
      (char*)arena_alloc(g_lexer_str_arena, vector_size(buf));

  vector_copy_data(char, buf, (void*)atom_val);

  return (token_t){.kind = TK_ATOM, .loc = loc, .value.as_cstr = atom_val};
}

static token_t lexer_tokenize_atom() {
  loc_t loc = lexer_getloc();

  vector_ptr_t m_cleanup(vector_cleanup) buf =
      vector_make(char, alloc_default, error_lexer_buf);

  int c = lexer_next_char_peek();
  while (is_atompart(c)) {
    buf = vector_push_back(char, buf, (char)lexer_next_char_consume());
    c = lexer_next_char_peek();
  }

  if (!is_correcty_separating(c)) {
    lexer_next_char_consume();
    return invalid_token_at(loc, ATOM_LEX_ERR_FMT);
  }

  buf = vector_push_back(char, buf, '\0');

  return lexer_tokenize_atom_tail__(loc, buf);
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

  if (!is_correcty_separating(c)) {
    lexer_next_char_consume();
    return invalid_token_at(loc, NUM_LEX_ERR_FMT);
  }

  buf[buf_pos] = '\0';

  char* end__;
  errno = 0;
  int64_t val = (int64_t)strtoll(buf, &end__, 10);

  if (errno == ERANGE)
    return invalid_token_at(loc, INT_LEX_OUTOFRANGE_FMT);

  return (token_t){.kind = TK_INT_LITERAL, .loc = loc, .value.as_i64 = val};
}

static token_t lexer_tokenize_str_readrest_invalid_tail__(
    loc_t loc,
    const char* const err_msg) {
  int c = lexer_next_char_peek();
  while (c != '\"' && c != EOF) {
    lexer_next_char_consume();
    c = lexer_next_char_peek();
  }

  return invalid_token_at(loc, err_msg);
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
      vector_make(char, alloc_default, error_lexer_buf);

  lexer_next_char_consume();  // consume '\"'
  int c = lexer_next_char_peek();
  while (true) {
    if (c == '\"') {
      lexer_next_char_consume();
      break;
    }

    if (c == EOF)
      return invalid_token_at(loc, STR_LEX_ERR_FMT);

    if (c == '\\') {
      lexer_next_char_consume();
      c = lexer_next_char_peek();

      if (m_contains(c, valid_escapes, sizeof(valid_escapes) / sizeof(int))) {
        c = escapes_map[c];
      } else {
        return lexer_tokenize_str_readrest_invalid_tail__(
            loc, STR_LEX_INVALID_ESCAPE_FMT);
      }
    }

    buf = vector_push_back(char, buf, (char)c);

    lexer_next_char_consume();
    c = lexer_next_char_peek();
  }

  buf = vector_push_back(char, buf, '\0');

  char* const strval = (char*)arena_alloc(g_lexer_str_arena, vector_size(buf));
  vector_copy_data(char, buf, (void*)strval);

  return (token_t){.kind = TK_STR_LITERAL, .loc = loc, .value.as_cstr = strval};
}

static token_t lexer_tokenize_hash_literal() {
  loc_t loc = lexer_getloc();

  vector_ptr_t buf m_cleanup(vector_cleanup) =
      vector_make(char, alloc_default, error_lexer_buf);

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

  return invalid_token_at(loc, "Invalid hash-literal");
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

  if (isdigit(c)) {
    token_t tk = lexer_tokenize_num();

    if (tk.kind == TK_INT_LITERAL) {
      if (tk.value.as_i64 > (((int64_t)1 << 60) - 1))
        return invalid_token_at(tk.loc, INT_LEX_OUTOFRANGE_FMT);
    }

    return tk;
  }

  if (c == '~') {
    lexer_next_char_consume();

    if (!isdigit(lexer_next_char_peek()))
      return invalid_token_at(loc, NUM_LEX_ERR_FMT);

    token_t tk = lexer_tokenize_num();

    if (tk.kind == TK_INT_LITERAL) {
      if (tk.value.as_i64 > ((int64_t)1 << 60))
        return invalid_token_at(tk.loc, INT_LEX_OUTOFRANGE_FMT);
      tk.value.as_i64 = -(tk.value.as_i64);
    }

    return tk;
  }

  if (c == '\"')
    return lexer_tokenize_str();

  token_t invalid = invalid_token_at(lexer_getloc(), UNEXP_LEX_ERR_FMT);
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
                                      [TK_STR_LITERAL] = "STR_LITERAL",
                                      [TK_BOOL_LITERAL] = "BOOL_LITERAL"

  };

  fprintf(stream, "Kind: %s\n", kinds[tk.kind]);
  fprintf(stream, "Location: line_no = %lu, line_pos = %lu\n", tk.loc.line_no,
          tk.loc.line_pos);

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

static parser_state_t g_parser = {
    .current_token = {.kind = TK_EOS, .loc = {.line_no = 0, .line_pos = 0}},
    .eos = true};

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

static noreturn void error_parser_buf() {
  error("Couldn't allocate buffer for parser\n");
}

static noreturn void error_parser_env() {
  error("Couldn't allocate env buffer for parser\n");
}

void parser_deinit_defer__([[maybe_unused]] int* blank) {
  parser_deinit();
}

static void parser_init(const char* const src_path, arena_ptr_t str_arena) {
  lexer_init(src_path, str_arena);

  token_t tk = lexer_next_token();
  g_parser = (parser_state_t){.current_token = tk, .eos = false};
}

static program_tree_t* parse_expr(arena_ptr_t pt_arena,
                                  arena_ptr_t str_arena,
                                  table_t env);

static program_tree_t* parse_binop(arena_ptr_t pt_arena,
                                   arena_ptr_t str_arena,
                                   table_t env) {
  token_t op_tk = parser_next_token_consume();
  loc_t loc = op_tk.loc;

  assert(op_tk.kind == TK_SPECIAL_ATOM);
  assert(m_contains(op_tk.value.as_special_atom, spec_binops,
                    sizeof(spec_binops) / sizeof(special_atom_t)));

  static const binop_t spec_to_binop[] = {
      [SPEC_PLUS] = BINOP_SUM, [SPEC_MINUS] = BINOP_SUB,
      [SPEC_STAR] = BINOP_MUL, [SPEC_SLASH] = BINOP_DIV,
      [SPEC_LE] = BINOP_LE,    [SPEC_LT] = BINOP_LT,
      [SPEC_GE] = BINOP_GE,    [SPEC_GT] = BINOP_GT,
      [SPEC_EQ] = BINOP_EQ,    [SPEC_NEQ] = BINOP_NEQ};

  program_tree_t* lhs = parse_expr(pt_arena, str_arena, env);
  program_tree_t* rhs = parse_expr(pt_arena, str_arena, env);

  token_t paren_c = parser_next_token_consume();
  if (paren_c.kind != TK_ROUND_PAREN_C)
    return pt_error_at(pt_arena, str_arena, loc, MALFORMED_FORM_FMT);

  return pt_make_binop(pt_arena, loc,
                        spec_to_binop[op_tk.value.as_special_atom], lhs, rhs);
}

static program_tree_t* parse_if(arena_ptr_t ast_arena,
                                arena_ptr_t str_arena,
                                table_t env) {
  token_t if_tk = parser_next_token_consume();
  loc_t loc = if_tk.loc;

  assert(if_tk.kind == TK_SPECIAL_ATOM &&
         if_tk.value.as_special_atom == SPEC_IF);

  program_tree_t* cond = parse_expr(ast_arena, str_arena, env);
  program_tree_t* t_branch = parse_expr(ast_arena, str_arena, env);
  program_tree_t* f_branch = parse_expr(ast_arena, str_arena, env);

  token_t paren_c = parser_next_token_consume();
  if (paren_c.kind != TK_ROUND_PAREN_C)
    return pt_error_at(ast_arena, str_arena, loc, MALFORMED_FORM_FMT);

  return pt_make_if(ast_arena, loc, cond, t_branch, f_branch);
}

static program_tree_t* parse_let(arena_ptr_t ast_arena,
                                 arena_ptr_t str_arena,
                                 table_t env) {
  token_t let = parser_next_token_consume();
  assert(let.kind == TK_SPECIAL_ATOM && let.value.as_special_atom == SPEC_LET);

  loc_t loc = let.loc;

  token_t paren_o = parser_next_token_peek();

  if (paren_o.kind != TK_ROUND_PAREN_O)
    return pt_error_at(ast_arena, str_arena, loc, MALFORMED_FORM_FMT);

  parser_next_token_consume();
  token_t name_atom = parser_next_token_consume();

  if (name_atom.kind != TK_ATOM)
    return pt_error_at(ast_arena, str_arena, loc, MALFORMED_FORM_FMT);

  name_id_t id = g_names_cnt++;
  table_t local_env = table_add(env, name_atom.value.as_cstr, id);

  program_tree_t* bind_value = parse_expr(ast_arena, str_arena, local_env);

  token_t paren_c = parser_next_token_consume();
  if (paren_c.kind != TK_ROUND_PAREN_C)
    return pt_error_at(ast_arena, str_arena, loc, MALFORMED_FORM_FMT);

  program_tree_t* expr = parse_expr(ast_arena, str_arena, local_env);

  token_t form_close = parser_next_token_consume();
  if (form_close.kind != TK_ROUND_PAREN_C)
    return pt_error_at(ast_arena, str_arena, loc, MALFORMED_FORM_FMT);

  return pt_make_let(ast_arena, loc,
                      (bind_pair_t){.name_id = id, .value_subtree = bind_value},
                      expr);
}

static vector_ptr_t collect_free(const program_tree_t* expr,
                                 table_t env,
                                 vector_ptr_t acc) {
  switch (expr->kind) {
    case PT_BOOL_LITERAL:
    case PT_INT_LITERAL:
    case PT_STR_LITERAL:
    case PT_ERROR:
      return acc;
    case PT_LET:;
      pt_let_form_t let = expr->value.as_let_form;
      env = table_add(env, NULL, let.bind.name_id);
      acc = collect_free(expr->value.as_let_form.bind.value_subtree, env, acc);
      return collect_free(expr->value.as_let_form.expr_subtree, env, acc);
    case PT_LAMBDA:;
      array_ptr_t captured = expr->value.as_lambda.captured;
      for (size_t i = 0; i < array_size(captured); i++) {
        name_id_t id = array_data(name_id_t, captured)[i];
        if (!table_contains(env, id))
          acc = vector_push_back(name_id_t, acc, id);
      }

      return acc;
    case PT_IF:
      acc = collect_free(expr->value.as_if_form.cond_subtree, env, acc);
      acc = collect_free(expr->value.as_if_form.t_branch_subtree, env, acc);
      return collect_free(expr->value.as_if_form.f_branch_subtree, env, acc);
    case PT_BINOP:
      acc = collect_free(expr->value.as_binop.lhs, env, acc);
      return collect_free(expr->value.as_binop.rhs, env, acc);
    case PT_CALL:
      acc = collect_free(expr->value.as_call.fn_subtree, env, acc);
      array_ptr_t args = expr->value.as_call.args_subtrees;
      for (size_t i = 0; i < array_size(args); i++)
        acc = collect_free(array_data(program_tree_t*, args)[i], env, acc);
      return acc;
    case PT_NAME:;
      name_id_t id = expr->value.as_name_id;
      if (!table_contains(env, id))
        acc = vector_push_back(name_id_t, acc, id);
      return acc;
    case PT_TOPLEVEL:
      assert(false);
      m_unreachable;
  }

  m_unreachable;
}

static program_tree_t* parse_lambda(arena_ptr_t ast_arena,
                                    arena_ptr_t str_arena,
                                    table_t env) {
  token_t lambd_tk = parser_next_token_consume();
  loc_t loc = lambd_tk.loc;
  assert(lambd_tk.kind == TK_SPECIAL_ATOM &&
         lambd_tk.value.as_special_atom == SPEC_LAMBDA);

  // vector_ptr_t local_env m_cleanup(vector_cleanup) =
  // vector_copy(assoc_pair_t, env);

  vector_ptr_t params_buf m_cleanup(vector_cleanup) =
      vector_make(name_id_t, alloc_default, error_parser_buf);

  token_t paren_o = parser_next_token_consume();
  if (paren_o.kind != TK_ROUND_PAREN_O)
    return pt_error_at(ast_arena, str_arena, loc, MALFORMED_FORM_FMT);

  token_t nxt = parser_next_token_consume();
  while (nxt.kind != TK_ROUND_PAREN_C) {
    if (nxt.kind != TK_ATOM)
      return pt_error_at(ast_arena, str_arena, loc, MALFORMED_FORM_FMT);

    name_id_t id = g_names_cnt++;
    env = table_add(env, nxt.value.as_cstr, id);
    params_buf = vector_push_back(name_id_t, params_buf, id);

    nxt = parser_next_token_consume();
  }

  size_t buf_sz = vector_size(params_buf);

  array_ptr_t params = array_make_arena(name_id_t, ast_arena, buf_sz);
  vector_copy_data(name_id_t, params_buf, array_baseptr(params));

  program_tree_t* body = parse_expr(ast_arena, str_arena, env);

  token_t paren_c = parser_next_token_consume();
  if (paren_c.kind != TK_ROUND_PAREN_C)
    return pt_error_at(ast_arena, str_arena, loc, MALFORMED_FORM_FMT);

  vector_ptr_t frees_buf m_cleanup(vector_cleanup) =
      vector_make(name_id_t, alloc_default, error_parser_buf);

  arena_ptr_t lambda_env_arena m_cleanup(arena_cleanup) =
      arena_make(arena_size(env.arena), alloc_default, error_parser_env);
  table_t lambda_env = table_make(lambda_env_arena);
  for (size_t i = 0; i < array_size(params); i++)
    lambda_env = table_add(lambda_env, NULL, array_data(name_id_t, params)[i]);

  frees_buf = collect_free(body, lambda_env, frees_buf);

  array_ptr_t captured =
      array_make_arena(name_id_t, ast_arena, vector_size(frees_buf));
  vector_copy_data(name_id_t, frees_buf, array_baseptr(captured));

  return pt_make_lambda(ast_arena, loc, params, captured, body);
}

static program_tree_t* parse_list(arena_ptr_t ast_arena,
                                  arena_ptr_t str_arena,
                                  table_t env) {
  token_t paren_o = parser_next_token_consume();
  loc_t loc = paren_o.loc;
  assert(paren_o.kind == TK_ROUND_PAREN_O);

  token_t fst = parser_next_token_peek();

  if (fst.kind == TK_SPECIAL_ATOM) {
    special_atom_t spec = fst.value.as_special_atom;

    if (spec == SPEC_LET)
      return parse_let(ast_arena, str_arena, env);

    if (spec == SPEC_IF)
      return parse_if(ast_arena, str_arena, env);

    if (spec == SPEC_LAMBDA)
      return parse_lambda(ast_arena, str_arena, env);

    if (m_contains(spec, spec_binops,
                   sizeof(spec_binops) / sizeof(special_atom_t))) {
      return parse_binop(ast_arena, str_arena, env);
    }

    parser_next_token_consume();
    return pt_error_at(ast_arena, str_arena, loc, UNEXP_FMT);
  }

  program_tree_t* fn = parse_expr(ast_arena, str_arena, env);

  vector_ptr_t buf m_cleanup(vector_cleanup) =
      vector_make(program_tree_t*, alloc_default, error_parser_buf);

  token_t nxt_tk = parser_next_token_peek();
  while (nxt_tk.kind != TK_ROUND_PAREN_C) {
    if (nxt_tk.kind == TK_EOS)
      return pt_error_at(ast_arena, str_arena, loc, UNCLOSED_LIST_ERR_FMT);

    program_tree_t* nxt = parse_expr(ast_arena, str_arena, env);
    buf = vector_push_back(program_tree_t*, buf, nxt);

    nxt_tk = parser_next_token_peek();
  }
  parser_next_token_consume();

  size_t args_sz = vector_size(buf);

  array_ptr_t args = array_make_arena(program_tree_t*, ast_arena, args_sz);
  vector_copy_data(program_tree_t*, buf, array_baseptr(args));

  return pt_make_call(ast_arena, loc, fn, args);
}

static program_tree_t* parse_expr(arena_ptr_t ast_arena,
                                  arena_ptr_t str_arena,
                                  table_t env) {
  token_t tk = parser_next_token_peek();
  loc_t loc = tk.loc;

  if (tk.kind == TK_INVALID) {
    parser_next_token_consume();
    return pt_make_error(ast_arena, loc, tk.err_msg);
  }

  if (tk.kind == TK_ATOM) {
    token_t atom_name = parser_next_token_consume();
    name_id_t id = table_lookup(env, atom_name.value.as_cstr);
    if (id == -1)
      return pt_error_at(ast_arena, str_arena, loc, UNDEFINED_NAME_FMT);

    return pt_make_name(ast_arena, loc, id);
  }

  if (tk.kind == TK_ROUND_PAREN_O) {
    return parse_list(ast_arena, str_arena, env);
  }

  if (tk.kind == TK_BOOL_LITERAL) {
    parser_next_token_consume();
    return pt_make_bool_literal(ast_arena, tk.loc, tk.value.as_bool);
  }

  if (tk.kind == TK_INT_LITERAL) {
    parser_next_token_consume();
    return pt_make_i64_literal(ast_arena, tk.loc, tk.value.as_i64);
  }

  if (tk.kind == TK_STR_LITERAL) {
    parser_next_token_consume();
    return pt_make_string_literal(ast_arena, tk.loc, tk.value.as_cstr);
  }

  if (tk.kind == TK_SPECIAL_ATOM) {
    parser_next_token_consume();
    return pt_error_at(ast_arena, str_arena, tk.loc, UNEXP_SPECIAL_ATOM_FMT);
  }

  parser_next_token_consume();
  return pt_error_at(ast_arena, str_arena, tk.loc, UNEXP_LEX_ERR_FMT);
}

static program_tree_t* parse_toplevel(arena_ptr_t ast_arena,
                                      arena_ptr_t str_arena) {
  vector_ptr_t buf m_cleanup(vector_cleanup) =
      vector_make(program_tree_t*, alloc_default, error_parser_buf);

  arena_ptr_t env_arena m_cleanup(arena_cleanup) =
      arena_make(PARSER_ENV_ARENA_SIZE, alloc_default, error_parser_env);
  table_t env = table_make(env_arena);

  token_t nxt = parser_next_token_peek();

  while (nxt.kind != TK_EOS) {
    program_tree_t* expr = parse_expr(ast_arena, str_arena, env);
    buf = vector_push_back(program_tree_t*, buf, expr);
    nxt = parser_next_token_peek();
  }

  size_t buf_sz = vector_size(buf);

  array_ptr_t toplvl_lst = array_make_arena(program_tree_t*, ast_arena, buf_sz);
  vector_copy_data(program_tree_t*, buf, array_baseptr(toplvl_lst));

  return pt_make_toplevel(ast_arena, toplvl_lst);
}

program_tree_t* parse(const char* const path,
                      arena_ptr_t pt_arena,
                      arena_ptr_t str_arena) {
  parser_init(path, str_arena);
  m_defer(parser_deinit_defer__);

  return parse_toplevel(pt_arena, str_arena);
}

static void fpprint_ast(FILE* stream, program_tree_t* ast);

static void fpprint_ast_seq(FILE* stream,
                            array_ptr_t /* [ast_t*] */ lst,
                            char delim) {
  size_t seq_sz = array_size(lst);
  for (size_t i = 0; i < seq_sz - 1; i++) {
    fpprint_ast(stream, array_data(program_tree_t*, lst)[i]);
    fprintf(stream, "%c", delim);
  }

  if (seq_sz > 0)
    fpprint_ast(stream, array_data(program_tree_t*, lst)[seq_sz - 1]);
}

static void fpprint_ast_if(FILE* stream, pt_if_form_t if_form) {
  fprintf(stream, "if ");
  fpprint_ast(stream, if_form.cond_subtree);
  fprintf(stream, " then ");
  fpprint_ast(stream, if_form.t_branch_subtree);
  fprintf(stream, " else ");
  fpprint_ast(stream, if_form.f_branch_subtree);
}

static void fpprint_ast_call(FILE* stream, pt_call_t call) {
  fprintf(stream, "(");
  fpprint_ast(stream, call.fn_subtree);
  fprintf(stream, " ");
  fpprint_ast_seq(stream, call.args_subtrees, ' ');
  fprintf(stream, ")");
}

static const char* binop_repr(binop_t op) {
  static const char* const sum = "+";
  static const char* const sub = "-";
  static const char* const mul = "*";
  static const char* const div = "/";
  static const char* const le = "<=";
  static const char* const lt = "<";
  static const char* const ge = ">=";
  static const char* const gt = ">";
  static const char* const eq = "=";
  static const char* const neq = "/=";

  switch (op) {
    case BINOP_SUM:
      return sum;
    case BINOP_SUB:
      return sub;
    case BINOP_MUL:
      return mul;
    case BINOP_DIV:
      return div;
    case BINOP_LE:
      return le;
    case BINOP_LT:
      return lt;
    case BINOP_GE:
      return ge;
    case BINOP_GT:
      return gt;
    case BINOP_EQ:
      return eq;
    case BINOP_NEQ:
      return neq;
    case BINOP_COUNT__:
      assert(false);
      m_unreachable;
  }
}

static void fpprint_ast_binop(FILE* stream, pt_binop_t binop) {
  fprintf(stream, "(OP(%s) ", binop_repr(binop.op));
  fpprint_ast(stream, binop.lhs);
  fprintf(stream, " ");
  fpprint_ast(stream, binop.rhs);
  fprintf(stream, ")");
}

static void fpprint_ast_let(FILE* stream, pt_let_form_t let) {
  fprintf(stream, "let $var%d := ", let.bind.name_id);
  fpprint_ast(stream, let.bind.value_subtree);
  fprintf(stream, " in ");
  fpprint_ast(stream, let.expr_subtree);
}

static void fpprint_ast_lambda(FILE* stream, pt_lambda_t lambda) {
  fprintf(stream, "[");
  size_t captured_sz = array_size(lambda.captured);
  for (size_t i = 0; i < captured_sz - 1; i++) {
    fprintf(stream, "$var%d ", array_data(name_id_t, lambda.captured)[i]);
  }
  if (captured_sz > 0)
    fprintf(stream, "$var%d",
            array_data(name_id_t, lambda.captured)[captured_sz - 1]);
  fprintf(stream, "]");
  fprintf(stream, "\\");
  array_ptr_t params = lambda.params;
  size_t params_sz = array_size(params);

  for (size_t i = 0; i < params_sz; i++) {
    fprintf(stream, "$var%d ", array_data(name_id_t, params)[i]);
  }

  fprintf(stream, "-> ");
  fpprint_ast(stream, lambda.body_subtree);
}

static void fpprint_ast(FILE* stream, program_tree_t* ast) {
  switch (ast->kind) {
    case PT_ERROR:
      fprintf(stream, "ERROR(%s)", ast->err_msg);
      break;
    case PT_TOPLEVEL:
      fpprint_ast_seq(stream, ast->value.as_subtree_list, '\n');
      break;
    case PT_BOOL_LITERAL:
      fprintf(stream, "BOOL(%s)", ast->value.as_bool ? "true" : "false");
      break;
    case PT_INT_LITERAL:
      fprintf(stream, "INT(%lli)", ast->value.as_i64);
      break;
    case PT_STR_LITERAL:
      fprintf(stream, "STRING(%s)", ast->value.as_cstr);
      break;
    case PT_NAME:
      fprintf(stream, "$var%d", ast->value.as_name_id);
      break;
    case PT_BINOP:
      fpprint_ast_binop(stream, ast->value.as_binop);
      break;
    case PT_IF:
      fpprint_ast_if(stream, ast->value.as_if_form);
      break;
    case PT_CALL:
      fpprint_ast_call(stream, ast->value.as_call);
      break;
    case PT_LET:
      fpprint_ast_let(stream, ast->value.as_let_form);
      break;
    case PT_LAMBDA:
      fpprint_ast_lambda(stream, ast->value.as_lambda);
      break;
  }
}

void fdump_ast(FILE* stream,
               const char* const src_path,
               arena_ptr_t ast_arena,
               arena_ptr_t str_arena) {
  program_tree_t* ast = parse(src_path, ast_arena, str_arena);
  fpprint_ast(stream, ast);
}
