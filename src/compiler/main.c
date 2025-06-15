#include <assert.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include "array.h"
#include "defs.h"
#include "error.h"
#include "parser.h"
#include "passes.h"
#include "program_tree.h"
#include "shared.h"

static int dump_parser_errors_and_die_tail(program_tree_t* pt, int cnt);

static int dump_parser_errors_and_die_seq(
    array_ptr_t /* [program_tree_t*] */ subtrees,
    int cnt) {
  for (size_t i = 0; i < array_size(subtrees); i++)
    cnt += dump_parser_errors_and_die_tail(
        array_data(program_tree_t*, subtrees)[i], 0);
  return cnt;
}

static int dump_parser_errors_and_die_tail(program_tree_t* pt, int cnt) {
  switch (pt->kind) {
    case PT_TOPLEVEL:
    case PT_VECTOR:
      return dump_parser_errors_and_die_seq(pt->value.as_subtree_list, cnt);
    case PT_BOOL_LITERAL:
    case PT_INT_LITERAL:
    case PT_STR_LITERAL:
    case PT_NAME:
    case PT_GLOBAL_SYMBOL:
      return cnt;
    case PT_LAMBDA:
      return dump_parser_errors_and_die_tail(pt->value.as_lambda.body_subtree,
                                             cnt);
    case PT_UOP:
      return dump_parser_errors_and_die_tail(pt->value.as_uop.operand, cnt);
    case PT_LET:
      return cnt +
             dump_parser_errors_and_die_tail(
                 pt->value.as_let_form.bind.value_subtree, 0) +
             dump_parser_errors_and_die_tail(pt->value.as_let_form.expr_subtree,
                                             0);
    case PT_BINOP:
      return cnt + dump_parser_errors_and_die_tail(pt->value.as_binop.lhs, 0) +
             dump_parser_errors_and_die_tail(pt->value.as_binop.rhs, 0);

    case PT_CALL:
      return cnt +
             dump_parser_errors_and_die_tail(pt->value.as_call.fn_subtree, 0) +
             dump_parser_errors_and_die_seq(pt->value.as_call.args_subtrees, 0);
    case PT_IF:;
      {
        return cnt +
               dump_parser_errors_and_die_tail(
                   pt->value.as_if_form.cond_subtree, 0) +
               dump_parser_errors_and_die_tail(
                   pt->value.as_if_form.t_branch_subtree, 0) +
               dump_parser_errors_and_die_tail(
                   pt->value.as_if_form.f_branch_subtree, 0);
      }
    case PT_ERROR:;
      {
        fprintf(stderr, "%s\n", pt->err_msg);
        return cnt + 1;
      }
  }
}

static bool dump_parser_errors_and_die_check(program_tree_t* pt) {
  int cnt = dump_parser_errors_and_die_tail(pt, 0);
  if (cnt > 0) {
    fprintf(stderr, "%d error generated.\n", cnt);
    return true;
  }

  return false;
}

void error_arena_alloc() {
  error("Arena allocation error\n");
}

int main(int argc, char* argv[]) {
  assert(argc >= 2);

  const char* const src_path = argv[1];

  shared_init(STR_ARENA_SIZE, PT_ARENA_SIZE, GLOBALS_ARENA_SIZE,
              ENV_ARENA_SIZE);
  program_tree_t* pt = parse(src_path);
  if (dump_parser_errors_and_die_check(pt))
    return 1;

  program_tree_t* transformed =
      to_mnf_pass(shrink_logic_operators_pass(register_pass(pt)));
  pprint_pt(transformed);

  shared_deinit();

  return 0;
}
