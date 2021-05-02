#include <math.h>
#include <string.h>
#include <stdint.h>
#include "sleef/sleefinline_purec_scalar.h"
#include "purec.c"

#define sleefify_f32(base_name) Sleef_##base_name##f1_u10purec
#define sleefify_f64(base_name) Sleef_##base_name##d1_u10purec

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
  for(int i=0; i< SIMD_SINGLE_STRIDE; i++) (ptr+i*stride)[0] = v[i];
}

vecf64 static inline vecf64_make(double* ptr, const long stride){
  vecf64 v;
  for(int i=0; i<SIMD_DOUBLE_STRIDE; i++) v[i] = ptr[i*stride];
  return v;
}
void static inline vecf64_store_multi(vecf64 v, double* ptr, const long stride){
  // TODO: Optimize this
  for(int i=0; i< SIMD_DOUBLE_STRIDE; i++) (ptr+i*stride)[0] = v[i];
}

#include "float_float.c"
#include "two_arg_fn_body.c"

one_arg_fn_body_ff(sin);
one_arg_fn_body_ff(cos);
one_arg_fn_body_ff(tan);

one_arg_fn_body_ff(asin);
one_arg_fn_body_ff(acos);
one_arg_fn_body_ff(atan);

one_arg_fn_body_ff(sinh);
one_arg_fn_body_ff(cosh);
one_arg_fn_body_ff(tanh);

one_arg_fn_body_ff(asinh);
one_arg_fn_body_ff(acosh);
one_arg_fn_body_ff(atanh);

one_arg_fn_body_ff(log);
one_arg_fn_body_ff(log10);
one_arg_fn_body_ff(log2);
one_arg_fn_body_ff(log1p);

one_arg_fn_body_ff(exp);
one_arg_fn_body_ff(exp2);
one_arg_fn_body_ff(exp10);
one_arg_fn_body_ff(expm1);

two_arg_fn_body(spow,   pow,   SIMD_SINGLE_STRIDE, float, vecf32, f32, float, vecf32);
two_arg_fn_body(satan2, atan2, SIMD_SINGLE_STRIDE, float, vecf32, f32, float, vecf32);
two_arg_fn_body(sadd,   add,   SIMD_SINGLE_STRIDE, float, vecf32, f32, float, vecf32);
two_arg_fn_body(ssub,   sub,   SIMD_SINGLE_STRIDE, float, vecf32, f32, float, vecf32);
two_arg_fn_body(smul,   mul,   SIMD_SINGLE_STRIDE, float, vecf32, f32, float, vecf32);
two_arg_fn_body(sdiv,   div,   SIMD_SINGLE_STRIDE, float, vecf32, f32, float, vecf32);


two_arg_fn_body(dpow,   pow,   SIMD_DOUBLE_STRIDE, double, vecf64, f64, double, vecf64);
two_arg_fn_body(datan2, atan2, SIMD_DOUBLE_STRIDE, double, vecf64, f64, double, vecf64);
two_arg_fn_body(dadd,   add,   SIMD_DOUBLE_STRIDE, double, vecf64, f64, double, vecf64);
two_arg_fn_body(dsub,   sub,   SIMD_DOUBLE_STRIDE, double, vecf64, f64, double, vecf64);
two_arg_fn_body(dmul,   mul,   SIMD_DOUBLE_STRIDE, double, vecf64, f64, double, vecf64);
two_arg_fn_body(ddiv,   div,   SIMD_DOUBLE_STRIDE, double, vecf64, f64, double, vecf64);

/* // two_arg_fn_body(copysign); */
/* // two_arg_fn_body(fmax); */
/* // two_arg_fn_body(fmin); */
/* // two_arg_fn_body(fdim); */

/* // two_arg_fn_body(hypot); */
