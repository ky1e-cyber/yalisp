#include "arena.h"
#include "array.h"
#include "ast.h"
#include "passes.h"

// static ast_t* copy_ast(ast_t* rt, arena_ptr_t arena) {
//   switch (rt->kind) {
//     case AST_BOOL_LITERAL:;
//       return ast_make_bool_literal(arena, rt->loc, rt->value.as_bool);
//     case AST_INT_LITERAL:
//       return ast_make_i64_literal(arena, rt->loc, rt->value.as_i64);
//     case AST_STR_LITERAL:
//       return ast_make_string_literal(arena, rt->loc, rt->value.as_cstr);
//     case AST_NAME:
//       return ast_make_name(arena, rt->loc, rt->value.as_name_id);
//     case AST_TOPLEVEL:;
//       array_ptr_t old_subtrees = rt->value.as_subtree_list;
//       size_t sz = array_size(old_subtrees);
//       array_ptr_t new_subtrees = array_make_arena(ast_t*, arena, sz);
//       for (size_t i = 0; i < sz; i++) {
//         array_data(ast_t*, new_subtrees)[i] =
//             copy_ast(array_data(ast_t*, old_subtrees)[i], arena);
//       }

//       return ast_make_toplevel(arena, new_subtrees);
//     case AST_LET:;
//       ast_t* bind_expr =
//           copy_ast(rt->value.as_let_form.bind.value_subtree, arena);
//       ast_t* expr = copy_ast(rt->value.as_let_form.expr_subtree, arena);

//       return ast_make_let(
//           arena, rt->loc,
//           (bind_pair_t){.name_id = rt->value.as_let_form.bind.name_id,
//                         .value_subtree = bind_expr},
//           expr);
//     case AST_IF:;
//       ast_t* cond = copy_ast(rt->value.as_if_form.cond_subtree, arena);
//       ast_t* t_branch = copy_ast(rt->value.as_if_form.t_branch_subtree, arena);
//       ast_t* f_branch = copy_ast(rt->value.as_if_form.f_branch_subtree, arena);
//       return ast_make_if(arena, rt->loc, cond, t_branch, f_branch);

//     case AST_BINOP:;
//       ast_t* lhs = copy_ast(rt->value.as_binop.lhs, arena);
//       ast_t* rhs = copy_ast(rt->value.as_binop.rhs, arena);
//       return ast_make_binop(arena, rt->loc, rt->value.as_binop.op, lhs, rhs);
//     case AST_LAMBDA:;
//       ast_t* body = copy_ast(rt->value.as_lambda.body_subtree, arena);
//       array_ptr_t old_params = rt->value.as_lambda.params;
//       array_ptr_t old_captured = rt->value.as_lambda.captured;
//       array_ptr_t new_params =
//           array_make_arena(name_id_t, arena, array_size(old_params));
//       array_ptr_t new_captured =
//           array_make_arena(name_id_t, arena, array_size(old_captured));

//       array_copy_data(name_id_t, old_params, array_baseptr(new_params));
//       array_copy_data(name_id_t, old_captured, array_baseptr(new_captured));

//       return ast_make_lambda(arena, rt->loc, new_params, new_captured, body);
//   }
// }

ast_t* to_mnf(ast_t* ast, arena_ptr_t arena) {}

