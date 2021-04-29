#if defined(__aarch64__)

#include<arm_neon.h>

#define sleefify_vecf32(base_name) Sleef_##base_name##f4_u10
#define sleefify_vecf64(base_name) Sleef_##base_name##d2_u10

typedef float32x4_t vecf32;
const int SIMD_SINGLE_STRIDE = 4;
vecf32 static inline vecf32_load(float* ptr){ return vld1q_f32(ptr); }
void static inline vecf32_store(float* ptr, vecf32 v){ return vst1q_f32(ptr, v); }

typedef float64x2_t vecf64;
const int SIMD_DOUBLE_STRIDE = 2;
vecf64 static inline vecf64_load(double* ptr){ return vld1q_f64(ptr); }
void static inline vecf64_store(double* ptr, vecf64 v){ return vst1q_f64(ptr, v); }

#endif
