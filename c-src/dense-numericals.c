#include <math.h>
#include <string.h>
#include <stdint.h>
#include "sleef/sleefinline_purec_scalar.h"
#include "purec.c"

#define sleefify_f32_u10(base_name) Sleef_##base_name##f1_u10purec
#define sleefify_f64_u10(base_name) Sleef_##base_name##d1_u10purec

#define sleefify_f32(base_name) Sleef_##base_name##f1_purec
#define sleefify_f64(base_name) Sleef_##base_name##d1_purec

#if defined(__x86_64)
  #include "x86_64.c"
#elif defined(__aarch64__)
  #include "aarch64.c"
#endif


vecf32 static inline vecf32_make(float* ptr, const long stride){
  vecf32 v;
  for(int i=0; i<SIMD_SINGLE_STRIDE; i++) v[i] = ptr[i*stride];
  return v;
}
void static inline vecf32_store_multi(vecf32 v, float* ptr, const long stride){
  // TODO: Optimize this
  for(int i=0; i<SIMD_SINGLE_STRIDE; i++) (ptr+i*stride)[0] = v[i];
}
void static inline vecf32_store_bool(vecf32 v, _Bool* ptr, const long stride){
  for(int i=0; i<SIMD_SINGLE_STRIDE; i++) (ptr+i*stride)[0] = v[i];
}

vecf64 static inline vecf64_make(double* ptr, const long stride){
  vecf64 v;
  for(int i=0; i<SIMD_DOUBLE_STRIDE; i++) v[i] = ptr[i*stride];
  return v;
}
void static inline vecf64_store_multi(vecf64 v, double* ptr, const long stride){
  for(int i=0; i<SIMD_DOUBLE_STRIDE; i++) (ptr+i*stride)[0] = v[i];
}
void static inline vecf64_store_bool(vecf64 v, _Bool* ptr, const long stride){
  for(int i=0; i<SIMD_DOUBLE_STRIDE; i++) (ptr+i*stride)[0] = v[i];
}


#include "one_arg_fn_body.c"
#include "two_arg_fn_body.c"
#include "comparison.c"

one_arg_fn_body_u10(ssin, sin, SIMD_SINGLE_STRIDE, float, vecf32, f32, float, vecf32);
one_arg_fn_body_u10(scos, cos, SIMD_SINGLE_STRIDE, float, vecf32, f32, float, vecf32);
one_arg_fn_body_u10(stan, tan, SIMD_SINGLE_STRIDE, float, vecf32, f32, float, vecf32);

one_arg_fn_body_u10(sasin, asin, SIMD_SINGLE_STRIDE, float, vecf32, f32, float, vecf32);
one_arg_fn_body_u10(sacos, acos, SIMD_SINGLE_STRIDE, float, vecf32, f32, float, vecf32);
one_arg_fn_body_u10(satan, atan, SIMD_SINGLE_STRIDE, float, vecf32, f32, float, vecf32);

one_arg_fn_body_u10(ssinh, sinh, SIMD_SINGLE_STRIDE, float, vecf32, f32, float, vecf32);
one_arg_fn_body_u10(scosh, cosh, SIMD_SINGLE_STRIDE, float, vecf32, f32, float, vecf32);
one_arg_fn_body_u10(stanh, tanh, SIMD_SINGLE_STRIDE, float, vecf32, f32, float, vecf32);

one_arg_fn_body_u10(sasinh, asinh, SIMD_SINGLE_STRIDE, float, vecf32, f32, float, vecf32);
one_arg_fn_body_u10(sacosh, acosh, SIMD_SINGLE_STRIDE, float, vecf32, f32, float, vecf32);
one_arg_fn_body_u10(satanh, atanh, SIMD_SINGLE_STRIDE, float, vecf32, f32, float, vecf32);

one_arg_fn_body_u10(dsin, sin, SIMD_DOUBLE_STRIDE, double, vecf64, f64, double, vecf64);
one_arg_fn_body_u10(dcos, cos, SIMD_DOUBLE_STRIDE, double, vecf64, f64, double, vecf64);
one_arg_fn_body_u10(dtan, tan, SIMD_DOUBLE_STRIDE, double, vecf64, f64, double, vecf64);

one_arg_fn_body_u10(dasin, asin, SIMD_DOUBLE_STRIDE, double, vecf64, f64, double, vecf64);
one_arg_fn_body_u10(dacos, acos, SIMD_DOUBLE_STRIDE, double, vecf64, f64, double, vecf64);
one_arg_fn_body_u10(datan, atan, SIMD_DOUBLE_STRIDE, double, vecf64, f64, double, vecf64);

one_arg_fn_body_u10(dsinh, sinh, SIMD_DOUBLE_STRIDE, double, vecf64, f64, double, vecf64);
one_arg_fn_body_u10(dcosh, cosh, SIMD_DOUBLE_STRIDE, double, vecf64, f64, double, vecf64);
one_arg_fn_body_u10(dtanh, tanh, SIMD_DOUBLE_STRIDE, double, vecf64, f64, double, vecf64);

one_arg_fn_body_u10(dasinh, asinh, SIMD_DOUBLE_STRIDE, double, vecf64, f64, double, vecf64);
one_arg_fn_body_u10(dacosh, acosh, SIMD_DOUBLE_STRIDE, double, vecf64, f64, double, vecf64);
one_arg_fn_body_u10(datanh, atanh, SIMD_DOUBLE_STRIDE, double, vecf64, f64, double, vecf64);



one_arg_fn_body_u10(slog,   log,   SIMD_SINGLE_STRIDE, float, vecf32, f32, float, vecf32);
one_arg_fn_body_u10(slog10, log10, SIMD_SINGLE_STRIDE, float, vecf32, f32, float, vecf32);
one_arg_fn_body_u10(slog2,  log2,  SIMD_SINGLE_STRIDE, float, vecf32, f32, float, vecf32);
one_arg_fn_body_u10(slog1p, log1p, SIMD_SINGLE_STRIDE, float, vecf32, f32, float, vecf32);

one_arg_fn_body_u10(dlog,   log,   SIMD_DOUBLE_STRIDE, double, vecf64, f64, double, vecf64);
one_arg_fn_body_u10(dlog10, log10, SIMD_DOUBLE_STRIDE, double, vecf64, f64, double, vecf64);
one_arg_fn_body_u10(dlog2,  log2,  SIMD_DOUBLE_STRIDE, double, vecf64, f64, double, vecf64);
one_arg_fn_body_u10(dlog1p, log1p, SIMD_DOUBLE_STRIDE, double, vecf64, f64, double, vecf64);



one_arg_fn_body_u10(sexp,   exp,   SIMD_SINGLE_STRIDE, float, vecf32, f32, float, vecf32);
one_arg_fn_body_u10(sexp10, exp10, SIMD_SINGLE_STRIDE, float, vecf32, f32, float, vecf32);
one_arg_fn_body_u10(sexp2,  exp2,  SIMD_SINGLE_STRIDE, float, vecf32, f32, float, vecf32);
one_arg_fn_body_u10(sexpm1, expm1, SIMD_SINGLE_STRIDE, float, vecf32, f32, float, vecf32);

one_arg_fn_body_u10(dexp,   exp,   SIMD_DOUBLE_STRIDE, double, vecf64, f64, double, vecf64);
one_arg_fn_body_u10(dexp10, exp10, SIMD_DOUBLE_STRIDE, double, vecf64, f64, double, vecf64);
one_arg_fn_body_u10(dexp2,  exp2,  SIMD_DOUBLE_STRIDE, double, vecf64, f64, double, vecf64);
one_arg_fn_body_u10(dexpm1, expm1, SIMD_DOUBLE_STRIDE, double, vecf64, f64, double, vecf64);




two_arg_fn_body_u10(spow,   pow,   SIMD_SINGLE_STRIDE, float, vecf32, f32, float, vecf32);
two_arg_fn_body_u10(satan2, atan2, SIMD_SINGLE_STRIDE, float, vecf32, f32, float, vecf32);
two_arg_fn_body_u10(sadd,   add,   SIMD_SINGLE_STRIDE, float, vecf32, f32, float, vecf32);
two_arg_fn_body_u10(ssub,   sub,   SIMD_SINGLE_STRIDE, float, vecf32, f32, float, vecf32);
two_arg_fn_body_u10(smul,   mul,   SIMD_SINGLE_STRIDE, float, vecf32, f32, float, vecf32);
two_arg_fn_body_u10(sdiv,   div,   SIMD_SINGLE_STRIDE, float, vecf32, f32, float, vecf32);

two_arg_fn_body_u10(dpow,   pow,   SIMD_DOUBLE_STRIDE, double, vecf64, f64, double, vecf64);
two_arg_fn_body_u10(datan2, atan2, SIMD_DOUBLE_STRIDE, double, vecf64, f64, double, vecf64);
two_arg_fn_body_u10(dadd,   add,   SIMD_DOUBLE_STRIDE, double, vecf64, f64, double, vecf64);
two_arg_fn_body_u10(dsub,   sub,   SIMD_DOUBLE_STRIDE, double, vecf64, f64, double, vecf64);
two_arg_fn_body_u10(dmul,   mul,   SIMD_DOUBLE_STRIDE, double, vecf64, f64, double, vecf64);
two_arg_fn_body_u10(ddiv,   div,   SIMD_DOUBLE_STRIDE, double, vecf64, f64, double, vecf64);


two_arg_fn_body_comparison(slt,  lt,  SIMD_SINGLE_STRIDE, float, vecf32, f32);
two_arg_fn_body_comparison(sle,  le,  SIMD_SINGLE_STRIDE, float, vecf32, f32);
two_arg_fn_body_comparison(seq,  eq,  SIMD_SINGLE_STRIDE, float, vecf32, f32);
two_arg_fn_body_comparison(sneq, neq, SIMD_SINGLE_STRIDE, float, vecf32, f32);
two_arg_fn_body_comparison(sge,  ge,  SIMD_SINGLE_STRIDE, float, vecf32, f32);
two_arg_fn_body_comparison(sgt,  gt,  SIMD_SINGLE_STRIDE, float, vecf32, f32);

two_arg_fn_body_comparison(dlt,  lt,  SIMD_DOUBLE_STRIDE, double, vecf64, f64);
two_arg_fn_body_comparison(dle,  le,  SIMD_DOUBLE_STRIDE, double, vecf64, f64);
two_arg_fn_body_comparison(deq,  eq,  SIMD_DOUBLE_STRIDE, double, vecf64, f64);
two_arg_fn_body_comparison(dneq, neq, SIMD_DOUBLE_STRIDE, double, vecf64, f64);
two_arg_fn_body_comparison(dge,  ge,  SIMD_DOUBLE_STRIDE, double, vecf64, f64);
two_arg_fn_body_comparison(dgt,  gt,  SIMD_DOUBLE_STRIDE, double, vecf64, f64);


/* // two_arg_fn_body(copysign); */
/* // two_arg_fn_body(fmax); */
/* // two_arg_fn_body(fmin); */
/* // two_arg_fn_body(fdim); */

/* // two_arg_fn_body(hypot); */
