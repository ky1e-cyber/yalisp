#if !defined(H_RUNTIME)
#define H_RUNTIME

#include <stddef.h>
#include <stdint.h>

typedef enum {
  type_ptr = 0b001u,
  type_bool = 0b010u,
  type_int = 0b100u,
  type_void = 0b110u
} type_t;

typedef struct {
  uint64_t sz;
  intptr_t* data;
} vector_t;

typedef intptr_t (*lambda_impl_ptr_t)(intptr_t, intptr_t);

typedef struct {
  intptr_t env_vector_ptr;
  lambda_impl_ptr_t lambda_impl;
} lambda_t;

typedef enum { OBJ_VEC, OBJ_LAMBDA, OBJ_STR } object_kind_t;

typedef struct {
  int rc;
  object_kind_t kind;
  union {
    vector_t as_vector;
    lambda_t as_lambda;
    char* as_cstr;
  } value;
} object_t;

extern char* yalisp_rt_tymismatch_s;

void* rt_alloc(size_t sz);

void rt_release(void* p);

void yalisp_rt_panic(char* msg);

void yalisp_rt_typcheck(intptr_t p, uint64_t t);

void yalisp_rt_rc_decr(intptr_t p);

void yalisp_rt_init();

intptr_t yalisp_rt_make_vector(uint64_t sz, intptr_t* data);

intptr_t yalisp_rt_make_str(char* s);

intptr_t yalisp_rt_make_lambda(lambda_impl_ptr_t lp, intptr_t env);

intptr_t yalisp_rt_call(intptr_t fn, intptr_t args_vec);

intptr_t yalisp_rt_vector_get(intptr_t vec, uint64_t i);

extern intptr_t print;

extern intptr_t println;

extern intptr_t read_int;

extern intptr_t vector_ref;

extern intptr_t vector_length;

#endif
