#pragma clang diagnostic ignored "-Wformat-security"

#include <assert.h>
#include <ctype.h>
#include <errno.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdnoreturn.h>
#include <string.h>
#include "arena.h"
#include "array.h"
#include "env_table.h"
#include "error.h"
#include "globals_table.h"
#include "macros.h"
#include "parser.h"
#include "program_tree.h"
#include "shared.h"
#include "token.h"
#include "vector.h"

typedef enum { HASH_TRUE = 0, HASH_FALSE, HASH_COUNT__ } hash_literal_t;

static const char* const hash_literals[] =
    {[HASH_TRUE] = "t", [HASH_FALSE] = "f"};

static const char* const special_atoms[] = {
    [SPEC_DEFINE] = "define", [SPEC_LET] = "let",
    [SPEC_PLUS] = "+",        [SPEC_MINUS] = "-",
    [SPEC_STAR] = "*",        [SPEC_SLASH] = "/",
    [SPEC_IF] = "if",         [SPEC_EQ] = "=",
    [SPEC_NEQ] = "/=",        [SPEC_LT] = "<",
    [SPEC_LE] = "<=",         [SPEC_GT] = ">",
    [SPEC_GE] = ">=",         [SPEC_NOT] = "not",
    [SPEC_AND] = "and",       [SPEC_OR] = "or",
    [SPEC_LAMBDA] = "lambda", [SPEC_VECTOR] = "vector",
    [SPEC_EXTERN] = "extern"};

static const special_atom_t spec_binops[] = {
    SPEC_PLUS, SPEC_MINUS, SPEC_STAR, SPEC_SLASH, SPEC_LE,
    SPEC_LT,   SPEC_GE,    SPEC_GT,   SPEC_EQ,    SPEC_NEQ};

static const char UNEXP_LEX_ERR_FMT[] =
    "Unexpected character encountered at %lu:%lu";
static const char NUM_LEX_ERR_FMT[] = "Invalid numeric literal at %lu:%lu";
static const char INT_LEX_OUTOFRANGE_FMT[] =
    "Int literal's value is too big at %lu:%lu";
static const char ATOM_LEX_ERR_FMT[] = "Invalid atom at %lu:%lu";
static const char STR_LEX_ERR_FMT[] = "Invalid string literal at %lu:%lu";
static const char STR_LEX_INVALID_ESCAPE_FMT[] =
    "Invalid escape in string literal at %lu:%lu";

static char UNCLOSED_LIST_ERR_FMT[] = "Unclosed list encountered at %lu:%lu";

static const char UNEXP_FMT[] = "Unexpected token at %lu:%lu";
static const char UNEXP_SPECIAL_ATOM_FMT[] =
    "Unexpected special atom encountered at %lu:%lu";

static const char MALFORMED_FORM_FMT[] = "Malformed form at %lu:%lu";
static const char UNDEFINED_NAME_FMT[] = "Undefined name at %lu:%lu";
static const char UNEXP_TOPLEVEL_DEFINITON_FMT[] =
    "Unexpected top-level definition at %lu:%lu";

static bool is_whitespace(int c) {
  return (c == ' ' || c == '\t' || c == '\n');
}

static bool is_bracket(int c) {
  static const int brackets[] = {'(', ')', '[', ']'};
  return m_contains(c, brackets, sizeof(brackets) / sizeof(int));
}

// checks if `c` correctly separates tokens
// e.g. in 'foo(bar)' open parenthesis correctly
//          separates `foo` and should be tokenized as
//          [ ATOM(foo), ROUND_PAREN_O, ATOM(bar), ROUND_PAREN_C ]
// while in 'foo#bar' `#` can't be part of an atom
//           therefore all expression must be tokenized as INVALID
static bool is_correcty_separating(int c) {
  return (c == EOF) || is_whitespace(c) || is_bracket(c);
}

// checks if c is a start of an atom
static bool is_atomstart(int c) {
  static const int puncts[] = {'!', '$',  '%', '&', '*', '+', ',', '-',
                               '.', '/',  ':', ';', '<', '=', '>', '?',
                               '@', '\\', '^', '`', '|', '{', '}'};
  return isalpha(c) || (m_contains(c, puncts, sizeof(puncts) / sizeof(int)));
}

static bool is_atompart(int c) {
  return isdigit(c) || is_atomstart(c);
}

static bool is_ded_literal_part(int c) {
  return isalpha(c) || (c == '-');
}

static lexer_state_t g_lexer = {.file_desc = NULL,
                                .current_char = EOF,
                                .line_no = 1,
                                .line_pos = 1,
                                .eof = true};

static noreturn void error_lexer_buf() {
  error("Could not allocate buffer for lexer\n");
}

static void lexer_init(const char* const src_path) {
  FILE* srcfile = fopen(src_path, "r");
  if (srcfile == NULL)
    error("Could not open file %s: %s\n", src_path, strerror(errno));

  int c = fgetc(srcfile);
  g_lexer = (lexer_state_t){.file_desc = srcfile,
                            .current_char = c,
                            .line_no = 1,
                            .line_pos = 1,
                            .eof = false};
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

  char* err_msg = (char*)arena_alloc(g_str_arena, sizeof(char) * (len + 24));

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

  char* const atom_val = (char*)arena_alloc(g_str_arena, vector_size(buf));

  vector_copy_data(char, buf, (void*)atom_val);

  return (token_t){.kind = TK_ATOM, .loc = loc, .value.as_cstr = atom_val};
}

static token_t lexer_tokenize_atom() {
  loc_t loc = lexer_getloc();

  vector_ptr_t m_cleanup(vector_cleanup) buf =
      vector_make(char, error_lexer_buf);

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
      vector_make(char, error_lexer_buf);

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

  char* const strval = (char*)arena_alloc(g_str_arena, vector_size(buf));
  vector_copy_data(char, buf, (void*)strval);

  return (token_t){.kind = TK_STR_LITERAL, .loc = loc, .value.as_cstr = strval};
}

static token_t lexer_tokenize_hash_literal() {
  loc_t loc = lexer_getloc();

  vector_ptr_t buf m_cleanup(vector_cleanup) =
      vector_make(char, error_lexer_buf);

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

  static const token_kind_t bracket_map[] = {['('] = TK_ROUND_PAREN_O,
                                             [')'] = TK_ROUND_PAREN_C,
                                             ['['] = TK_SQ_PAREN_O,
                                             [']'] = TK_SQ_PAREN_C};

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

static bool tk_is_paren_open(token_kind_t kind) {
  return kind == TK_ROUND_PAREN_O || kind == TK_SQ_PAREN_O;
}

static token_kind_t tk_closing_bracket(token_kind_t open) {
  assert(tk_is_paren_open(open));
  return open == TK_ROUND_PAREN_O ? TK_ROUND_PAREN_C : TK_SQ_PAREN_C;
}

void parser_deinit_defer__([[maybe_unused]] int* blank) {
  parser_deinit();
}

static void parser_init(const char* const src_path) {
  lexer_init(src_path);

  token_t tk = lexer_next_token();
  g_parser = (parser_state_t){.current_token = tk, .eos = false};
}

static program_tree_t* parse_expr(env_table_t env, bool is_toplevel);

static array_ptr_t /* [program_tree_t*] */
parse_list_tail(env_table_t env, token_kind_t closing) {
  vector_ptr_t buf m_cleanup(vector_cleanup) =
      vector_make(program_tree_t*, error_parser_buf);

  token_t nxt_tk = parser_next_token_peek();
  while (nxt_tk.kind != closing) {
    if (nxt_tk.kind == TK_EOS)
      return NULL;

    program_tree_t* nxt = parse_expr(env, false);
    buf = vector_push_back(program_tree_t*, buf, nxt);

    nxt_tk = parser_next_token_peek();
  }

  token_t paren_c = parser_next_token_consume();
  assert(paren_c.kind == closing);

  size_t args_sz = vector_size(buf);

  array_ptr_t lst = array_make_arena(program_tree_t*, g_pt_arena, args_sz);
  vector_copy_data(program_tree_t*, buf, array_baseptr(lst));

  return lst;
}

static program_tree_t* parse_binop(env_table_t env, token_kind_t closing) {
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

  program_tree_t* lhs = parse_expr(env, false);
  program_tree_t* rhs = parse_expr(env, false);

  token_t paren_c = parser_next_token_consume();
  if (paren_c.kind != closing)
    return pt_error_at(g_pt_arena, g_str_arena, loc, MALFORMED_FORM_FMT);

  return pt_make_binop(g_pt_arena, loc,
                       spec_to_binop[op_tk.value.as_special_atom], lhs, rhs);
}

static program_tree_t* parse_if(env_table_t env, token_kind_t closing) {
  token_t if_tk = parser_next_token_consume();
  loc_t loc = if_tk.loc;

  assert(if_tk.kind == TK_SPECIAL_ATOM &&
         if_tk.value.as_special_atom == SPEC_IF);

  program_tree_t* cond = parse_expr(env, false);
  program_tree_t* t_branch = parse_expr(env, false);
  program_tree_t* f_branch = parse_expr(env, false);

  token_t paren_c = parser_next_token_consume();
  if (paren_c.kind != closing)
    return pt_error_at(g_pt_arena, g_str_arena, loc, MALFORMED_FORM_FMT);

  return pt_make_if(g_pt_arena, loc, cond, t_branch, f_branch);
}

static program_tree_t* parse_let(env_table_t env, token_kind_t closing) {
  token_t let = parser_next_token_consume();
  assert(let.kind == TK_SPECIAL_ATOM && let.value.as_special_atom == SPEC_LET);

  loc_t loc = let.loc;

  token_t paren_o = parser_next_token_peek();

  if (!tk_is_paren_open(paren_o.kind))
    return pt_error_at(g_pt_arena, g_str_arena, loc, MALFORMED_FORM_FMT);

  parser_next_token_consume();
  token_t name_atom = parser_next_token_consume();

  if (name_atom.kind != TK_ATOM)
    return pt_error_at(g_pt_arena, g_str_arena, loc, MALFORMED_FORM_FMT);

  int id = g_names_cnt++;
  env_table_t local_env = env_table_add(env, name_atom.value.as_cstr, id);

  program_tree_t* bind_value = parse_expr(local_env, false);

  token_t paren_c = parser_next_token_consume();
  if (paren_c.kind != tk_closing_bracket(paren_o.kind))
    return pt_error_at(g_pt_arena, g_str_arena, loc, MALFORMED_FORM_FMT);

  program_tree_t* expr = parse_expr(local_env, false);

  token_t form_close = parser_next_token_consume();
  if (form_close.kind != closing)
    return pt_error_at(g_pt_arena, g_str_arena, loc, MALFORMED_FORM_FMT);

  return pt_make_let(g_pt_arena, loc,
                     (bind_pair_t){.name_id = id, .value_subtree = bind_value},
                     expr);
}

static vector_ptr_t collect_free(const program_tree_t* expr,
                                 env_table_t env,
                                 vector_ptr_t acc);

static vector_ptr_t collect_free_seq(array_ptr_t /* [program_tree_t*] */ exprs,
                                     env_table_t env,
                                     vector_ptr_t acc) {
  for (size_t i = 0; i < array_size(exprs); i++)
    acc = collect_free(array_data(program_tree_t*, exprs)[i], env, acc);
  return acc;
}

static vector_ptr_t collect_free(const program_tree_t* expr,
                                 env_table_t env,
                                 vector_ptr_t acc) {
  switch (expr->kind) {
    case PT_BOOL_LITERAL:
    case PT_INT_LITERAL:
    case PT_STR_LITERAL:
    case PT_GLOBAL_SYMBOL:
    case PT_ERROR:
      return acc;
    case PT_LET:;
      {
        pt_let_form_t let = expr->value.as_let_form;
        env = env_table_add(env, NULL, let.bind.name_id);
        acc =
            collect_free(expr->value.as_let_form.bind.value_subtree, env, acc);
        return collect_free(expr->value.as_let_form.expr_subtree, env, acc);
      }
    case PT_LAMBDA:;
      {
        array_ptr_t captured = expr->value.as_lambda.captured;
        for (size_t i = 0; i < array_size(captured); i++) {
          int id = array_data(int, captured)[i];
          if (!env_table_contains(env, id))
            acc = vector_push_back(int, acc, id);
        }

        return acc;
      }
    case PT_IF:;
      {
        acc = collect_free(expr->value.as_if_form.cond_subtree, env, acc);
        acc = collect_free(expr->value.as_if_form.t_branch_subtree, env, acc);
        return collect_free(expr->value.as_if_form.f_branch_subtree, env, acc);
      }
    case PT_BINOP:;
      {
        acc = collect_free(expr->value.as_binop.lhs, env, acc);
        return collect_free(expr->value.as_binop.rhs, env, acc);
      }
    case PT_CALL:;
      {
        acc = collect_free(expr->value.as_call.fn_subtree, env, acc);
        return collect_free_seq(expr->value.as_call.args_subtrees, env, acc);
      }
    case PT_NAME:;
      {
        int id = expr->value.as_name_id;
        if (!env_table_contains(env, id))
          acc = vector_push_back(int, acc, id);
        return acc;
      }
    case PT_VECTOR:
      return collect_free_seq(expr->value.as_subtree_list, env, acc);
    case PT_TOPLEVEL:;
      {
        assert(false);
        m_unreachable;
      }
  }
}

static program_tree_t* parse_lambda(env_table_t env, token_kind_t closing) {
  token_t lambd_tk = parser_next_token_consume();
  loc_t loc = lambd_tk.loc;
  assert(lambd_tk.kind == TK_SPECIAL_ATOM &&
         lambd_tk.value.as_special_atom == SPEC_LAMBDA);

  vector_ptr_t params_buf m_cleanup(vector_cleanup) =
      vector_make(int, error_parser_buf);

  const token_t params_paren_o = parser_next_token_consume();
  if (!tk_is_paren_open(params_paren_o.kind))
    return pt_error_at(g_pt_arena, g_str_arena, loc, MALFORMED_FORM_FMT);

  token_t nxt = parser_next_token_consume();
  while (nxt.kind != tk_closing_bracket(params_paren_o.kind)) {
    if (nxt.kind != TK_ATOM)
      return pt_error_at(g_pt_arena, g_str_arena, loc, MALFORMED_FORM_FMT);

    int id = g_names_cnt++;
    env = env_table_add(env, nxt.value.as_cstr, id);
    params_buf = vector_push_back(int, params_buf, id);

    nxt = parser_next_token_consume();
  }

  size_t buf_sz = vector_size(params_buf);

  array_ptr_t params = array_make_arena(int, g_pt_arena, buf_sz);
  vector_copy_data(int, params_buf, array_baseptr(params));

  program_tree_t* body = parse_expr(env, false);

  token_t paren_c = parser_next_token_consume();
  if (paren_c.kind != closing)
    return pt_error_at(g_pt_arena, g_str_arena, loc, MALFORMED_FORM_FMT);

  vector_ptr_t frees_buf m_cleanup(vector_cleanup) =
      vector_make(int, error_parser_buf);

  arena_ptr_t lambda_env_arena m_cleanup(arena_cleanup) =
      arena_make(arena_size(env.arena), error_parser_env);
  env_table_t lambda_env = env_table_make(lambda_env_arena);
  for (size_t i = 0; i < array_size(params); i++)
    lambda_env = env_table_add(lambda_env, NULL, array_data(int, params)[i]);

  frees_buf = collect_free(body, lambda_env, frees_buf);

  array_ptr_t captured =
      array_make_arena(int, g_pt_arena, vector_size(frees_buf));
  vector_copy_data(int, frees_buf, array_baseptr(captured));

  return pt_make_lambda(g_pt_arena, loc, params, captured, body);
}

static program_tree_t* parse_vector(

    env_table_t env,
    token_kind_t closing) {
  token_t vec_tk = parser_next_token_consume();
  loc_t loc = vec_tk.loc;

  assert(vec_tk.kind == TK_SPECIAL_ATOM &&
         vec_tk.value.as_special_atom == SPEC_VECTOR);

  array_ptr_t elems = parse_list_tail(env, closing);
  if (elems == NULL)
    return pt_error_at(g_pt_arena, g_str_arena, loc, MALFORMED_FORM_FMT);

  return pt_make_vector(g_pt_arena, loc, elems);
}

static program_tree_t* parse_extern(

    token_kind_t closing,
    bool is_toplevel) {
  token_t ext_tk = parser_next_token_consume();
  loc_t loc = ext_tk.loc;

  assert(ext_tk.kind == TK_SPECIAL_ATOM &&
         ext_tk.value.as_special_atom == SPEC_EXTERN);
  do {
    token_t symbol_tk = parser_next_token_consume();
    if (symbol_tk.kind != TK_ATOM)
      break;

    token_t paren_c = parser_next_token_consume();
    if (paren_c.kind != closing)
      break;

    if (!is_toplevel)
      return pt_error_at(g_pt_arena, g_str_arena, loc,
                         UNEXP_TOPLEVEL_DEFINITON_FMT);

    g_globals_table =
        globals_table_add(g_globals_table, symbol_tk.value.as_cstr);

    return pt_make_global(g_pt_arena, loc, symbol_tk.value.as_cstr);

  } while (false);

  return pt_error_at(g_pt_arena, g_str_arena, loc, MALFORMED_FORM_FMT);
}

static program_tree_t* parse_list(env_table_t env, bool is_toplevel) {
  token_t paren_o = parser_next_token_consume();
  loc_t loc = paren_o.loc;
  assert(tk_is_paren_open(paren_o.kind));

  token_kind_t closing = tk_closing_bracket(paren_o.kind);

  token_t fst = parser_next_token_peek();

  if (fst.kind == TK_SPECIAL_ATOM) {
    special_atom_t spec = fst.value.as_special_atom;

    if (spec == SPEC_LET)
      return parse_let(env, closing);

    if (spec == SPEC_IF)
      return parse_if(env, closing);

    if (spec == SPEC_LAMBDA)
      return parse_lambda(env, closing);

    if (spec == SPEC_VECTOR)
      return parse_vector(env, closing);

    if (spec == SPEC_EXTERN)
      return parse_extern(closing, is_toplevel);

    if (m_contains(spec, spec_binops,
                   sizeof(spec_binops) / sizeof(special_atom_t))) {
      return parse_binop(env, closing);
    }

    parser_next_token_consume();
    return pt_error_at(g_pt_arena, g_str_arena, loc, UNEXP_FMT);
  }

  program_tree_t* fn = parse_expr(env, false);

  array_ptr_t args = parse_list_tail(env, closing);
  if (args == NULL)
    return pt_error_at(g_pt_arena, g_str_arena, loc, UNCLOSED_LIST_ERR_FMT);

  return pt_make_call(g_pt_arena, loc, fn, args);
}

static program_tree_t* parse_expr(env_table_t env, bool is_toplevel) {
  token_t tk = parser_next_token_peek();
  loc_t loc = tk.loc;

  if (tk.kind == TK_INVALID) {
    parser_next_token_consume();
    return pt_make_error(g_pt_arena, loc, tk.err_msg);
  }

  if (tk.kind == TK_ATOM) {
    token_t atom_name = parser_next_token_consume();

    int id = env_table_lookup(env, atom_name.value.as_cstr);
    if (id == -1) {
      if (globals_table_contains(g_globals_table, atom_name.value.as_cstr))
        return pt_make_global(g_pt_arena, loc, atom_name.value.as_cstr);

      return pt_error_at(g_pt_arena, g_str_arena, loc, UNDEFINED_NAME_FMT);
    }
    return pt_make_name(g_pt_arena, loc, id);
  }

  if (tk_is_paren_open(tk.kind))
    return parse_list(env, is_toplevel);

  if (tk.kind == TK_BOOL_LITERAL) {
    parser_next_token_consume();
    return pt_make_bool_literal(g_pt_arena, tk.loc, tk.value.as_bool);
  }

  if (tk.kind == TK_INT_LITERAL) {
    parser_next_token_consume();
    return pt_make_i64_literal(g_pt_arena, tk.loc, tk.value.as_i64);
  }

  if (tk.kind == TK_STR_LITERAL) {
    parser_next_token_consume();
    return pt_make_string_literal(g_pt_arena, tk.loc, tk.value.as_cstr);
  }

  if (tk.kind == TK_SPECIAL_ATOM) {
    parser_next_token_consume();
    return pt_error_at(g_pt_arena, g_str_arena, tk.loc, UNEXP_SPECIAL_ATOM_FMT);
  }

  parser_next_token_consume();
  return pt_error_at(g_pt_arena, g_str_arena, tk.loc, UNEXP_LEX_ERR_FMT);
}

static program_tree_t* parse_toplevel() {
  vector_ptr_t buf m_cleanup(vector_cleanup) =
      vector_make(program_tree_t*, error_parser_buf);

  env_table_t env = env_table_make(g_env_arena);

  token_t nxt = parser_next_token_peek();

  while (nxt.kind != TK_EOS) {
    program_tree_t* expr = parse_expr(env, true);
    buf = vector_push_back(program_tree_t*, buf, expr);
    nxt = parser_next_token_peek();
  }

  size_t buf_sz = vector_size(buf);

  array_ptr_t toplvl_lst =
      array_make_arena(program_tree_t*, g_pt_arena, buf_sz);
  vector_copy_data(program_tree_t*, buf, array_baseptr(toplvl_lst));

  return pt_make_toplevel(g_pt_arena, toplvl_lst);
}

program_tree_t* parse(const char* const path) {
  parser_init(path);
  m_defer(parser_deinit_defer__);

  return parse_toplevel();
}
