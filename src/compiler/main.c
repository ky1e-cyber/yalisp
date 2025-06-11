#include <assert.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include "allocator.h"
#include "arena.h"
#include "array.h"
#include "error.h"
#include "macros.h"
#include "parser.h"
#include "passes.h"
#include "program_tree.h"

static int dump_parser_errors_and_die_tail(program_tree_t* pt, int cnt) {
  switch (pt->kind) {
    case PT_TOPLEVEL:;
      {
        array_ptr_t exprs = pt->value.as_subtree_list;
        for (size_t i = 0; i < array_size(exprs); i++)
          cnt += dump_parser_errors_and_die_tail(
              array_data(program_tree_t*, exprs)[i], 0);
        return cnt;
      }
    case PT_BOOL_LITERAL:
    case PT_INT_LITERAL:
    case PT_STR_LITERAL:
    case PT_NAME:
      return cnt;
    case PT_LAMBDA:
      return dump_parser_errors_and_die_tail(pt->value.as_lambda.body_subtree,
                                             cnt);
    case PT_LET:;
      {
        return cnt +
               dump_parser_errors_and_die_tail(
                   pt->value.as_let_form.bind.value_subtree, 0) +
               dump_parser_errors_and_die_tail(
                   pt->value.as_let_form.expr_subtree, 0);
      }
    case PT_BINOP:;
      {
        return cnt +
               dump_parser_errors_and_die_tail(pt->value.as_binop.lhs, cnt) +
               dump_parser_errors_and_die_tail(pt->value.as_binop.rhs, 0);
      }
    case PT_CALL:;
      {
        cnt += dump_parser_errors_and_die_tail(pt->value.as_call.fn_subtree, 0);

        array_ptr_t args = pt->value.as_call.args_subtrees;
        for (size_t i = 0; i < array_size(args); i++)
          cnt += dump_parser_errors_and_die_tail(
              array_data(program_tree_t*, args)[i], 0);

        return cnt;
      }
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

  arena_ptr_t str_arena m_cleanup(arena_cleanup) =
      arena_make(1 << 20, alloc_default, error_arena_alloc);

  arena_ptr_t pt_arena m_cleanup(arena_cleanup) =
      arena_make(1 << 20, alloc_default, error_arena_alloc);

  program_tree_t* pt = parse(src_path, str_arena, pt_arena);

  if (dump_parser_errors_and_die_check(pt))
    return 1;

  fpprint_pt(stdout, pt);

  program_tree_t* mnf = to_mnf(pt, pt_arena);

  fpprint_pt(stdout, mnf);

  return 0;
}
