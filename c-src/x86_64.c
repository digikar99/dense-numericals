#if defined(__x86_64)

#include <immintrin.h>

#if defined(__AVX512F__)




#include "sleef/sleefinline_avx512f.h"

#define sleefify_vecf32_u10(base_name) Sleef_##base_name##f16_u10avx512f
#define sleefify_vecf64_u10(base_name) Sleef_##base_name##d8_u10avx512f

#define sleefify_vecf32(base_name) Sleef_##base_name##f8_avx512f
#define sleefify_vecf64(base_name) Sleef_##base_name##d4_avx512f

typedef __m512 vecf32;
typedef __m512d vecf64;
const int SIMD_SINGLE_STRIDE = 16;
const int SIMD_DOUBLE_STRIDE = 8;

vecf32 static inline vecf32_load(float* ptr){ return _mm512_loadu_ps(ptr); }
void static inline vecf32_store(float* ptr, vecf32 v){ return _mm512_storeu_ps(ptr, v); }

vecf64 static inline vecf64_load(double* ptr){ return _mm512_loadu_pd(ptr); }
void static inline vecf64_store(double* ptr, vecf64 v){ return _mm512_storeu_pd(ptr, v); }


vecf32 static inline Sleef_addf16_u10avx2(vecf32 a, vecf32 b){return _mm512_add_ps(a, b);}
vecf32 static inline Sleef_subf16_u10avx2(vecf32 a, vecf32 b){return _mm512_sub_ps(a, b);}
vecf32 static inline Sleef_mulf16_u10avx2(vecf32 a, vecf32 b){return _mm512_mul_ps(a, b);}
vecf32 static inline Sleef_divf16_u10avx2(vecf32 a, vecf32 b){return _mm512_div_ps(a, b);}

vecf64 static inline Sleef_addd8_u10avx2(vecf64 a, vecf64 b){return _mm512_add_pd(a, b);}
vecf64 static inline Sleef_subd8_u10avx2(vecf64 a, vecf64 b){return _mm512_sub_pd(a, b);}
vecf64 static inline Sleef_muld8_u10avx2(vecf64 a, vecf64 b){return _mm512_mul_pd(a, b);}
vecf64 static inline Sleef_divd8_u10avx2(vecf64 a, vecf64 b){return _mm512_div_pd(a, b);}




#elif defined(__AVX2__)





#include "sleef/sleefinline_avx2.h"

#define sleefify_vecf32_u10(base_name) Sleef_##base_name##f8_u10avx2
#define sleefify_vecf64_u10(base_name) Sleef_##base_name##d4_u10avx2

#define sleefify_vecf32(base_name) Sleef_##base_name##f8_avx2
#define sleefify_vecf64(base_name) Sleef_##base_name##d4_avx2

typedef __m256 vecf32;
typedef __m256d vecf64;
const int SIMD_SINGLE_STRIDE = 8;
const int SIMD_DOUBLE_STRIDE = 4;

vecf32 static inline vecf32_load(float* ptr){ return _mm256_loadu_ps(ptr); }
void static inline vecf32_store(float* ptr, vecf32 v){ return _mm256_storeu_ps(ptr, v); }

vecf64 static inline vecf64_load(double* ptr){ return _mm256_loadu_pd(ptr); }
void static inline vecf64_store(double* ptr, vecf64 v){ return _mm256_storeu_pd(ptr, v); }


vecf32 static inline Sleef_addf8_u10avx2(vecf32 a, vecf32 b){return _mm256_add_ps(a, b);}
vecf32 static inline Sleef_subf8_u10avx2(vecf32 a, vecf32 b){return _mm256_sub_ps(a, b);}
vecf32 static inline Sleef_mulf8_u10avx2(vecf32 a, vecf32 b){return _mm256_mul_ps(a, b);}
vecf32 static inline Sleef_divf8_u10avx2(vecf32 a, vecf32 b){return _mm256_div_ps(a, b);}

// A quick test on numpy will suggest that numpy uses the "ordered" "non-signalling"
// comparisons
// - https://www.felixcloutier.com/x86/cmppd#tbl-3-1
vecf32 static inline Sleef_ltf8_avx2(vecf32 a, vecf32 b){
  return _mm256_cmp_ps(a, b, _CMP_LT_OQ);
}
vecf32 static inline Sleef_lef8_avx2(vecf32 a, vecf32 b){
  return _mm256_cmp_ps(a, b, _CMP_LE_OQ);
}
vecf32 static inline Sleef_eqf8_avx2(vecf32 a, vecf32 b){
  return _mm256_cmp_ps(a, b, _CMP_EQ_OQ);
}
vecf32 static inline Sleef_neqf8_avx2(vecf32 a, vecf32 b){
  return _mm256_cmp_ps(a, b, _CMP_NEQ_OQ);
}
vecf32 static inline Sleef_gtf8_avx2(vecf32 a, vecf32 b){
  return _mm256_cmp_ps(a, b, _CMP_GT_OQ);
}
vecf32 static inline Sleef_gef8_avx2(vecf32 a, vecf32 b){
  return _mm256_cmp_ps(a, b, _CMP_GE_OQ);
}


vecf64 static inline Sleef_addd4_u10avx2(vecf64 a, vecf64 b){return _mm256_add_pd(a, b);}
vecf64 static inline Sleef_subd4_u10avx2(vecf64 a, vecf64 b){return _mm256_sub_pd(a, b);}
vecf64 static inline Sleef_muld4_u10avx2(vecf64 a, vecf64 b){return _mm256_mul_pd(a, b);}
vecf64 static inline Sleef_divd4_u10avx2(vecf64 a, vecf64 b){return _mm256_div_pd(a, b);}
vecf64 static inline Sleef_ltd4_avx2(vecf64 a, vecf64 b){
  return _mm256_cmp_pd(a, b, _CMP_LT_OQ);
}
vecf64 static inline Sleef_led4_avx2(vecf64 a, vecf64 b){
  return _mm256_cmp_pd(a, b, _CMP_LE_OQ);
}
vecf64 static inline Sleef_eqd4_avx2(vecf64 a, vecf64 b){
  return _mm256_cmp_pd(a, b, _CMP_EQ_OQ);
}
vecf64 static inline Sleef_neqd4_avx2(vecf64 a, vecf64 b){
  return _mm256_cmp_pd(a, b, _CMP_NEQ_OQ);
}
vecf64 static inline Sleef_gtd4_avx2(vecf64 a, vecf64 b){
  return _mm256_cmp_pd(a, b, _CMP_GT_OQ);
}
vecf64 static inline Sleef_ged4_avx2(vecf64 a, vecf64 b){
  return _mm256_cmp_pd(a, b, _CMP_GE_OQ);
}



#else




#include "sleef/sleefinline_sse4.h"

#define sleefify_vecf32_u10(base_name) Sleef_##base_name##f4_u10sse4
#define sleefify_vecf64_u10(base_name) Sleef_##base_name##d2_u10sse4

#define sleefify_vecf32(base_name) Sleef_##base_name##f8_sse4
#define sleefify_vecf64(base_name) Sleef_##base_name##d4_sse4

typedef __m128 vecf32;
typedef __m128d vecf64;
const int SIMD_SINGLE_STRIDE = 4;
const int SIMD_DOUBLE_STRIDE = 2;

vecf32 static inline vecf32_load(float* ptr){ return _mm_loadu_ps(ptr); }
void static inline vecf32_store(float* ptr, vecf32 v){ return _mm_storeu_ps(ptr, v); }

vecf64 static inline vecf64_load(double* ptr){ return _mm_loadu_pd(ptr); }
void static inline vecf64_store(double* ptr, vecf64 v){ return _mm_storeu_pd(ptr, v); }


vecf32 static inline Sleef_addf4_u10avx2(vecf32 a, vecf32 b){return _mm_add_ps(a, b);}
vecf32 static inline Sleef_subf4_u10avx2(vecf32 a, vecf32 b){return _mm_sub_ps(a, b);}
vecf32 static inline Sleef_mulf4_u10avx2(vecf32 a, vecf32 b){return _mm_mul_ps(a, b);}
vecf32 static inline Sleef_divf4_u10avx2(vecf32 a, vecf32 b){return _mm_div_ps(a, b);}

vecf64 static inline Sleef_addd2_u10avx2(vecf64 a, vecf64 b){return _mm_add_pd(a, b);}
vecf64 static inline Sleef_subd2_u10avx2(vecf64 a, vecf64 b){return _mm_sub_pd(a, b);}
vecf64 static inline Sleef_muld2_u10avx2(vecf64 a, vecf64 b){return _mm_mul_pd(a, b);}
vecf64 static inline Sleef_divd2_u10avx2(vecf64 a, vecf64 b){return _mm_div_pd(a, b);}




#endif





#endif // __x64_64
