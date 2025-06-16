#include <assert.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include "array.h"
#include "codegen.h"
#include "macros.h"
#include "program_tree.h"
#include "shared.h"

static int g_basicblocks_cnt = 0;

static int next_bb_id() {
  return (g_basicblocks_cnt++) + 1;
}

static uint64_t reinterpret_i64(int64_t x) {
  uint64_t res;
  memcpy(&res, &x, sizeof(uint64_t));
  return res;
}

typedef enum {
  type_ptr = 0b001u,
  type_bool = 0b010u,
  type_int = 0b100u,
  type_void = 0b110u
} type_t;

static const uint64_t true_const = (1 << 3) | type_bool;
static const uint64_t false_const = type_bool;

static uint64_t flag_i64(int64_t val) {
  uint64_t u = reinterpret_i64(val);
  return (u << 3) | type_int;
}

static char* make_asign_dest(int dest) {
  static const char tmpvar_fmt[] = "%%v%d =l ";
  static char ret[] = "%r =l ";
  static char scrap[] = "%scrp =l ";

#define tmpvar_sz sizeof(tmpvar_fmt) / sizeof(char) + 32

  static char tmpvar[tmpvar_sz];

  if (dest == -1)
    return scrap;
  if (dest == 0)
    return ret;

  snprintf(tmpvar, tmpvar_sz, tmpvar_fmt, dest);
  return tmpvar;

#undef tmpvar_sz
}

static char* make_repr(program_tree_t* pt_atomic) {
#define repr_buf_sz 64

  static char repr_buf[repr_buf_sz];

  switch (pt_atomic->kind) {
    case PT_BOOL_LITERAL:;
      {
        snprintf(repr_buf, repr_buf_sz, "%llu",
                 pt_atomic->value.as_bool ? true_const : false_const);
        break;
      }
    case PT_INT_LITERAL:;
      {
        snprintf(repr_buf, repr_buf_sz, "%llu",
                 flag_i64(pt_atomic->value.as_i64));
        break;
      }
    case PT_NAME:;
      {
        snprintf(repr_buf, repr_buf_sz, "%%v%d", pt_atomic->value.as_name_id);
        break;
      }
    case PT_GLOBAL_SYMBOL:;
      {
        snprintf(repr_buf, repr_buf_sz, "$%s", pt_atomic->value.as_symbol);
        break;
      }
    default:;
      {
        assert(false);
        m_unreachable;
      }
  }

  return repr_buf;
#undef repr_buf_sz
}

static void dump_qbe_typecheck(program_tree_t* operand, type_t t) {
  static const type_t kinds_map[] = {
      [PT_BOOL_LITERAL] = type_bool, [PT_INT_LITERAL] = type_int};

  switch (operand->kind) {
    case PT_BOOL_LITERAL:
    case PT_INT_LITERAL:;
      {
        if (kinds_map[operand->kind] != t) {
          printf(
              "  call $yalisp_rt_panic(l $yalisp_rt_tymismatch_s) "
              "# typecheck failed\n");
        }
        break;
      }
    case PT_NAME:
    case PT_GLOBAL_SYMBOL:;
      {
        printf("  call $yalisp_rt_typcheck(l %s, l %u)\n", make_repr(operand),
               t);
        break;
      }
    default:;
      {
        assert(false);
        m_unreachable;
      }
  }
}

static void dump_qbe_expr(program_tree_t* pt_expr, int dest);

static void dump_qbe_prim_literals(program_tree_t* pt_lit, int dest) {
  assert(is_primitive_literal(pt_lit));
  printf("  %s", make_asign_dest(dest));
  printf("add 0, %s # ", make_repr(pt_lit));
  pt_pprint(pt_lit);
  printf("\n");
}

static void dump_qbe_name(int id, int dest) {
  printf("  %s", make_asign_dest(dest));
  printf("add 0, %%v%d\n", id);
}

static void dump_qbe_let(pt_let_form_t let, int dest) {
  dump_qbe_expr(let.bind.value_subtree, let.bind.name_id);
  dump_qbe_expr(let.expr_subtree, dest);
}

static void dump_qbe_uop(pt_uop_t uop, int dest) {
  switch (uop.op) {
    case UNARY_NOT:;
      {
        dump_qbe_typecheck(uop.operand, type_bool);
        printf("  %s", make_asign_dest(dest));
        printf("xor %s, %u # OP(not)\n", make_repr(uop.operand), 1 << 3);
        break;
      }
    default:;
      {
        assert(false);
        m_unreachable;
      }
  }
}

static void dump_qbe_deflag(program_tree_t* operand, int dest) {
  printf("  %s", make_asign_dest(dest));
  printf("shr %s, 3 # detag\n", make_repr(operand));
}

static void dump_qbe_flag(int dest, int src, type_t t) {
  printf("  %s", make_asign_dest(dest));
  printf("shl %%v%d, 3\n", src);

  printf("  %s", make_asign_dest(dest));
  printf("or %%v%d, %u # tag\n", dest, t);
}

static void dump_qbe_binop(pt_binop_t binop, int dest) {
  static const char* ops_to_builtin[] = {
      [BINOP_SUM] = "yalisp_builtin_add",
      [BINOP_SUB] = "yalisp_builtin_sub",
      [BINOP_MUL] = "yalisp_builtin_mul",
      [BINOP_DIV] = "yalisp_builtin_div",
  };

  static const char* ops_to_instrs[] = {
      [BINOP_EQ] = "ceql",  [BINOP_NEQ] = "cnel", [BINOP_GE] = "csgel",
      [BINOP_GT] = "csgtl", [BINOP_LE] = "cslel", [BINOP_LT] = "csltl"};

  switch (binop.op) {
    case BINOP_EQ:
    case BINOP_NEQ:
    case BINOP_GE:
    case BINOP_GT:
    case BINOP_LE:
    case BINOP_LT:;
      {
        dump_qbe_typecheck(binop.lhs, type_int);
        dump_qbe_typecheck(binop.rhs, type_int);

        const int tmp_lhs_id = next_name_id();
        dump_qbe_deflag(binop.lhs, tmp_lhs_id);
        const int tmp_rhs_id = next_name_id();
        dump_qbe_deflag(binop.rhs, tmp_rhs_id);

        const int tmp_res_id = next_name_id();
        printf("  %s%s %%v%d, %%v%d\n", make_asign_dest(tmp_res_id),
               ops_to_instrs[binop.op], tmp_lhs_id, tmp_rhs_id);
        dump_qbe_flag(tmp_res_id, tmp_res_id, type_bool);
        printf("  %sadd 0, %%v%d\n", make_asign_dest(dest), tmp_res_id);

        return;
      }
    case BINOP_COUNT__:
    case BINOP_AND:
    case BINOP_OR:;
      {
        assert(false);
        m_unreachable;
      }
    default:
      break;
  }

  dump_qbe_typecheck(binop.lhs, type_int);
  dump_qbe_typecheck(binop.rhs, type_int);
  printf("  %scall $%s", make_asign_dest(dest), ops_to_builtin[binop.op]);
  printf("(l %s, ", make_repr(binop.lhs));
  printf("l %s)\n", make_repr(binop.rhs));
}

static void dump_qbe_if(pt_if_form_t if_form, int dest) {
  dump_qbe_typecheck(if_form.cond_subtree, type_bool);
  const int cond_tmp_id = next_name_id();
  dump_qbe_deflag(if_form.cond_subtree, cond_tmp_id);

  const int bb_id = next_bb_id();

  printf("  jnz %%v%d, @bb_true%d, @bb_false%d\n", cond_tmp_id, bb_id, bb_id);
  printf("@bb_true%d\n", bb_id);
  dump_qbe_expr(if_form.t_branch_subtree, dest);
  printf("  jmp @bb_end%d\n", bb_id);
  printf("@bb_false%d\n", bb_id);
  dump_qbe_expr(if_form.f_branch_subtree, dest);
  printf("@bb_end%d\n", bb_id);
}

static void dump_qbe_vector(array_ptr_t elems, int dest) {
  size_t elems_sz = array_size(elems);
  printf("  %s", make_asign_dest(dest));
  printf("call $yalisp_builtin_make_vector(l %lu", elems_sz);

  for (size_t i = 0; i < elems_sz; i++)
    printf(", l %s", make_repr(array_data(program_tree_t*, elems)[i]));

  printf(")\n");
}

static void dump_qbe_str(int id, int dest) {
  printf("  %s", make_asign_dest(dest));
  printf("call $yalisp_builtin_make_str(l $s%d)\n", id);
}

static void dump_qbe_names_vector(array_ptr_t ids, int dest) {
  size_t ids_sz = array_size(ids);

  printf("  %s", make_asign_dest(dest));
  printf("call $yalisp_builtin_make_vector(l %lu", ids_sz);

  for (size_t i = 0; i < ids_sz; i++)
    printf(", l %%v%d", array_data(int, ids)[i]);

  printf(")\n");
}

static void dump_qbe_lambda(pt_lambda_t lambda, int dest) {
  int env_tmp_id = next_name_id();

  dump_qbe_names_vector(lambda.captured, env_tmp_id);

  printf("  %s", make_asign_dest(dest));
  printf("call $yalisp_builtin_make_lambda(l %d, l %%v%d)\n", lambda.id,
         env_tmp_id);
}

static void rename_global(char* symbol) {
  size_t l = strlen(symbol);

  for (size_t i = 0; i < l; i++) {
    if (symbol[i] == '-')
      symbol[i] = '_';
  }
}

static void dump_qbe_call(pt_call_t call, int dest) {
  int args_id = next_name_id();

  dump_qbe_vector(call.args_subtrees, args_id);

  dump_qbe_typecheck(call.fn_subtree, type_ptr);
  printf("  %s", make_asign_dest(dest));
  printf("call $yalisp_builtin_call(l %s, l %%v%d)\n",
         make_repr(call.fn_subtree), args_id);
}

static void dump_qbe_global(char* symbol, int dest) {
  rename_global(symbol);
  printf("  %s", make_asign_dest(dest));
  printf("add 0, $%s\n", symbol);
}

static void dump_qbe_lambda_impl(pt_lambda_t lambda) {
  printf(
      "function l $lambda%d(l %%env, l %%args) {\n"
      "@start\n"
      "  %%r =l add 0, %u\n",
      lambda.id, type_void);

  for (size_t i = 0; i < array_size(lambda.captured); i++)
    printf("  %%v%d =l call $yalisp_builtin_vector_get(l %%env, l %lu)\n",
           array_data(int, lambda.captured)[i], i);

  for (size_t i = 0; i < array_size(lambda.params); i++)
    printf("  %%v%d =l call $yalisp_builtin_vector_get(l %%args, l %lu)\n",
           array_data(int, lambda.params)[i], i);

  dump_qbe_expr(lambda.body_subtree, 0);

  printf("  ret %%r\n}\n");
}

static void dump_qbe_expr(program_tree_t* pt_expr, int dest) {
  switch (pt_expr->kind) {
    case PT_BOOL_LITERAL:
    case PT_INT_LITERAL:;
      {
        dump_qbe_prim_literals(pt_expr, dest);
        break;
      }
    case PT_NAME:;
      {
        dump_qbe_name(pt_expr->value.as_name_id, dest);
        break;
      }
    case PT_LET:;
      {
        dump_qbe_let(pt_expr->value.as_let_form, dest);
        break;
      }
    case PT_UOP:;
      {
        dump_qbe_uop(pt_expr->value.as_uop, dest);
        break;
      }
    case PT_BINOP:;
      {
        dump_qbe_binop(pt_expr->value.as_binop, dest);
        break;
      }
    case PT_IF:;
      {
        dump_qbe_if(pt_expr->value.as_if_form, dest);
        break;
      }
    case PT_STR_LITERAL:;
      {
        dump_qbe_str(pt_expr->value.as_str_literal.id, dest);
        break;
      }
    case PT_VECTOR:;
      {
        dump_qbe_vector(pt_expr->value.as_subtree_list, dest);
        break;
      }
    case PT_LAMBDA:;
      {
        dump_qbe_lambda(pt_expr->value.as_lambda, dest);
        break;
      }
    case PT_CALL:;
      {
        dump_qbe_call(pt_expr->value.as_call, dest);
        break;
      }
    case PT_GLOBAL_SYMBOL:;
      {
        dump_qbe_global(pt_expr->value.as_symbol, dest);
        break;
      }
    case PT_ERROR:
    case PT_TOPLEVEL:;
      {
        assert(false);
        m_unreachable;
      }
  }
}

static void dump_qbe_main_body(program_tree_t* pt_toplevel) {
  static char* main_header =
      "export function w $main() {\n"
      "@start\n"
      "  call $yalisp_rt_init()\n"
      "  %r =l add 0, 0\n";

  static char* main_end =
      "  %r =l shr %r, 3\n"
      "  ret %r\n"
      "}\n";

  array_ptr_t pts = pt_toplevel->value.as_subtree_list;

  size_t pts_sz = array_size(pts);

  printf("%s", main_header);

  if (pts_sz > 0) {
    for (size_t i = 0; i < pts_sz - 1; i++) {
      program_tree_t* pt = array_data(program_tree_t*, pts)[i];
      if (pt->kind == PT_GLOBAL_SYMBOL)
        continue;
      dump_qbe_expr(pt, -1);
    }

    program_tree_t* ret = array_data(program_tree_t*, pts)[pts_sz - 1];
    if (ret->kind != PT_GLOBAL_SYMBOL)
      dump_qbe_expr(ret, 0);
  }

  printf("%s", main_end);
}

void dump_qbe(program_tree_t* pt_toplevel) {
  for (int i = 0; i < g_str_cnt; i++)
    printf("data $s%d = { b \"%s\", b 0}\n\n", g_str_literals[i].id,
           g_str_literals[i].cstr);

  for (int i = 0; i < g_lambdas_cnt; i++) {
    dump_qbe_lambda_impl(g_lambdas[i]);
    printf("\n");
  }

  dump_qbe_main_body(pt_toplevel);
}
