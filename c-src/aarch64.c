#if defined(__aarch64__)

#include <arm_neon.h>
#include "sleef/sleefinline_advsimd.h"

typedef float32x4_t vecf32;
typedef float64x2_t vecf64;

typedef uint32x4_t boolf32;
typedef uint64x2_t boolf64;

#define sleefify_vecf32_u10(base_name) Sleef_##base_name##f4_u10advsimd
#define sleefify_vecf64_u10(base_name) Sleef_##base_name##d2_u10advsimd

#define sleefify_vecf32(base_name) Sleef_##base_name##f4_advsimd
#define sleefify_vecf64(base_name) Sleef_##base_name##d2_advsimd

const int SIMD_SINGLE_STRIDE = 4;
const int SIMD_DOUBLE_STRIDE = 2;

vecf32 static inline vecf32_load(float* ptr){ return vld1q_f32(ptr); }
void static inline vecf32_store(float* ptr, vecf32 v){ return vst1q_f32(ptr, v); }

vecf64 static inline vecf64_load(double* ptr){ return vld1q_f64(ptr); }
void static inline vecf64_store(double* ptr, vecf64 v){ return vst1q_f64(ptr, v); }

void static inline vecf32_store_bool(boolf32 v, _Bool* ptr, const long stride){
  for(int i=0; i<SIMD_SINGLE_STRIDE; i++) (ptr+i*stride)[0] = v[i];
}
void static inline vecf64_store_bool(boolf64 v, _Bool* ptr, const long stride){
  for(int i=0; i<SIMD_DOUBLE_STRIDE; i++) (ptr+i*stride)[0] = v[i];
}

vecf32 static inline Sleef_addf4_u10advsimd(vecf32 a, vecf32 b){return vaddq_f32(a, b);}
vecf32 static inline Sleef_subf4_u10advsimd(vecf32 a, vecf32 b){return vsubq_f32(a, b);}
vecf32 static inline Sleef_mulf4_u10advsimd(vecf32 a, vecf32 b){return vmulq_f32(a, b);}
vecf32 static inline Sleef_divf4_u10advsimd(vecf32 a, vecf32 b){return vdivq_f32(a, b);}

boolf32 static inline Sleef_ltf4_advsimd(vecf32 a, vecf32 b){return vcltq_f32(a, b);}
boolf32 static inline Sleef_lef4_advsimd(vecf32 a, vecf32 b){return vcleq_f32(a, b);}
boolf32 static inline Sleef_eqf4_advsimd(vecf32 a, vecf32 b){return vceqq_f32(a, b);}
boolf32 static inline Sleef_neqf4_advsimd(vecf32 a, vecf32 b){return vceqzq_u32(vceqq_f32(a, b));}
boolf32 static inline Sleef_gtf4_advsimd(vecf32 a, vecf32 b){return vcgtq_f32(a, b);}
boolf32 static inline Sleef_gef4_advsimd(vecf32 a, vecf32 b){return vcgeq_f32(a, b);}


vecf64 static inline Sleef_addd2_u10advsimd(vecf64 a, vecf64 b){return vaddq_f64(a, b);}
vecf64 static inline Sleef_subd2_u10advsimd(vecf64 a, vecf64 b){return vsubq_f64(a, b);}
vecf64 static inline Sleef_muld2_u10advsimd(vecf64 a, vecf64 b){return vmulq_f64(a, b);}
vecf64 static inline Sleef_divd2_u10advsimd(vecf64 a, vecf64 b){return vdivq_f64(a, b);}

boolf64 static inline Sleef_ltd2_advsimd(vecf64 a, vecf64 b){return vcltq_f64(a, b);}
boolf64 static inline Sleef_led2_advsimd(vecf64 a, vecf64 b){return vcleq_f64(a, b);}
boolf64 static inline Sleef_eqd2_advsimd(vecf64 a, vecf64 b){return vceqq_f64(a, b);}
boolf64 static inline Sleef_neqd2_advsimd(vecf64 a, vecf64 b){return vceqzq_u64(vceqq_f64(a, b));}
boolf64 static inline Sleef_gtd2_advsimd(vecf64 a, vecf64 b){return vcgtq_f64(a, b);}
boolf64 static inline Sleef_ged2_advsimd(vecf64 a, vecf64 b){return vcgeq_f64(a, b);}



#endif
