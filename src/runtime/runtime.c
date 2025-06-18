#include <assert.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdnoreturn.h>
#include <string.h>
#include "runtime.h"

static type_t get_type(intptr_t p) {
  return (type_t)(p & 0b111);
}

static intptr_t detag(intptr_t p) {
  return p >> 3;
}

static uint64_t* detag_ptr(intptr_t p) {
  return (uint64_t*)(p ^ 1);
}

static intptr_t tag_ptr(uint64_t* ptr) {
  return (intptr_t)ptr | 1;
}

char* yalisp_rt_tymismatch_s = "Type error";

void noreturn yalisp_rt_panic(char* msg) {
  fprintf(stderr, "PANIC: %s\n", msg);
  exit(1);
}

void yalisp_rt_typcheck(intptr_t p, uint64_t t) {
  if (get_type(p) != (type_t)t)
    yalisp_rt_panic(yalisp_rt_tymismatch_s);
}

static void rc_decr(object_t* obj);

static void rc_vector_release(vector_t vec) {
  for (size_t i = 0; i < vec.sz; i++)
    yalisp_rt_rc_decr(vec.data[i]);
  rt_release(vec.data);
}

static void rc_incr(object_t* obj) {
  if (obj->rc < 0)
    return;
  obj->rc++;
}

static void rc_decr(object_t* obj) {
  if (obj->rc < 0)
    return;

  if ((--obj->rc) > 0)
    return;

  switch (obj->kind) {
    case OBJ_VEC: {
      rc_vector_release(obj->value.as_vector);
      break;
    }
    case OBJ_LAMBDA: {
      yalisp_rt_rc_decr(obj->value.as_lambda.env_vector_ptr);
      break;
    }
    case OBJ_STR:
      break;
  }

  rt_release(obj);
}

void yalisp_rt_rc_incr(intptr_t p) {
  if (get_type(p) != type_ptr)
    return;

  rc_incr((object_t*)detag_ptr(p));
}

void yalisp_rt_rc_decr(intptr_t p) {
  if (get_type(p) != type_ptr)
    return;

  rc_decr((object_t*)detag_ptr(p));
}

void* rt_alloc(size_t sz) {
  void* mem = malloc(sz);
  if (!mem)
    yalisp_rt_panic("Out of memory");

  return mem;
}

void rt_release(void* p) {
  free(p);
}

static object_t* rt_alloc_object() {
  return (object_t*)rt_alloc(sizeof(object_t));
}

intptr_t yalisp_rt_make_vector(uint64_t sz, intptr_t* data) {
  object_t* vec_obj = rt_alloc_object();
  intptr_t* vec_data = (intptr_t*)rt_alloc(sizeof(intptr_t) * sz);

  memmove(vec_data, data, sz * sizeof(intptr_t));

  for (size_t i = 0; i < sz; i++) {
    if (get_type(vec_data[i]) == type_ptr)
      rc_incr((object_t*)detag_ptr(vec_data[i]));
  }

  *vec_obj = (object_t){.kind = OBJ_VEC,
                        .rc = 1,
                        .value.as_vector = {.sz = sz, .data = vec_data}};

  return tag_ptr((uint64_t*)vec_obj);
}

intptr_t yalisp_rt_make_str(char* s) {
  object_t* str_obj = rt_alloc_object();
  *str_obj = (object_t){.kind = OBJ_STR, .rc = 1, .value.as_cstr = s};

  return tag_ptr((uint64_t*)str_obj);
}

intptr_t yalisp_rt_make_lambda(lambda_impl_ptr_t lp, intptr_t env) {
  object_t* lambda_obj = rt_alloc_object();

  *lambda_obj =
      (object_t){.kind = OBJ_LAMBDA,
                 .rc = 1,
                 .value.as_lambda = {.lambda_impl = lp, .env_vector_ptr = env}};
  return tag_ptr((uint64_t*)lambda_obj);
}

intptr_t yalisp_rt_call(intptr_t fn, intptr_t args_vec) {
  object_t* fn_obj = (object_t*)detag_ptr(fn);
  if (fn_obj->kind != OBJ_LAMBDA)
    yalisp_rt_panic("Trying to apply non-lambda object");

  lambda_impl_ptr_t fn_impl = fn_obj->value.as_lambda.lambda_impl;

  return fn_impl(fn_obj->value.as_lambda.env_vector_ptr, args_vec);
}

intptr_t yalisp_rt_vector_get(intptr_t vec, uint64_t i) {
  object_t* vec_obj = (object_t*)detag_ptr(vec);
  return vec_obj->value.as_vector.data[i];
}

intptr_t print_impl(intptr_t __attribute__((unused)) env_vec,
                    intptr_t args_vec) {
  object_t* args_obj = (object_t*)detag_ptr(args_vec);
  if (args_obj->value.as_vector.sz != 1)
    yalisp_rt_panic("print must be applied to 1 argument");

  intptr_t arg = args_obj->value.as_vector.data[0];

  switch (get_type(arg)) {
    case type_int: {
      printf("%li\n", detag(arg));
      break;
    }
    case type_bool: {
      printf("%s\n", detag(arg) ? "#t" : "#f");
      break;
    }
    case type_void: {
      printf("#void\n");
      break;
    }
    case type_ptr: {
      printf("%p\n", (void*)detag_ptr(arg));
      break;
    }
    default:
      assert(0 && "Unexpected type tag");
  }

  return (intptr_t)type_void;
}

object_t print_lambda = {
    .kind = OBJ_LAMBDA,
    .rc = -1,
    .value.as_lambda = {.lambda_impl = print_impl, .env_vector_ptr = 0}};

intptr_t vector_ref_impl(intptr_t __attribute__((unused)) env_vec,
                         intptr_t args_vec) {
  object_t* args_obj = (object_t*)detag_ptr(args_vec);
  if (args_obj->value.as_vector.sz != 2)
    yalisp_rt_panic("vector-ref must be applied to 2 arguments");

  intptr_t vec = args_obj->value.as_vector.data[0];
  intptr_t pos = args_obj->value.as_vector.data[1];

  yalisp_rt_typcheck(vec, type_ptr);
  yalisp_rt_typcheck(pos, type_int);

  object_t* vec_obj = (object_t*)detag_ptr(vec);
  if (vec_obj->kind != OBJ_VEC)
    yalisp_rt_panic("vector-ref applied to non-vec object");

  int64_t i = (int64_t)detag(pos);
  int64_t vec_sz = vec_obj->value.as_vector.sz;

  if (i < 0 || i >= vec_sz)
    yalisp_rt_panic("vector-ref out of bounds");

  return vec_obj->value.as_vector.data[i];
}

object_t vector_ref_lambda = {
    .kind = OBJ_LAMBDA,
    .rc = -1,
    .value.as_lambda = {.lambda_impl = vector_ref_impl, .env_vector_ptr = 0}};

intptr_t print_string_impl(intptr_t __attribute__((unused)) env_vec,
                           intptr_t args_vec) {
  object_t* args_obj = (object_t*)detag_ptr(args_vec);
  if (args_obj->value.as_vector.sz != 1)
    yalisp_rt_panic("print-string must be applied to 1 argument");

  intptr_t arg = args_obj->value.as_vector.data[0];

  yalisp_rt_typcheck(arg, type_ptr);

  object_t* arg_obj = (object_t*)detag_ptr(arg);

  if (arg_obj->kind != OBJ_STR)
    yalisp_rt_panic("print-string applied to non-string argument");

  printf("%s\n", arg_obj->value.as_cstr);

  return (intptr_t)type_void;
}

object_t print_string_lambda = {
    .kind = OBJ_LAMBDA,
    .rc = -1,
    .value.as_lambda = {.lambda_impl = print_string_impl, .env_vector_ptr = 0}};

intptr_t print;
intptr_t read_int;
intptr_t vector_ref;
intptr_t vector_length;
intptr_t print_string;

void yalisp_rt_init() {
  print = (intptr_t)(&print_lambda) | type_ptr;
  vector_ref = (intptr_t)(&vector_ref_lambda) | type_ptr;
  print_string = (intptr_t)(&print_string_lambda) | type_ptr;
}
