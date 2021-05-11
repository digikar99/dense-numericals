#if defined(__x86_64)

#include <immintrin.h>

#if defined(__AVX512F__)




#include "sleef/sleefinline_avx512f.h"

#define sleefify_vecf32_u10(base_name) Sleef_##base_name##f16_u10avx512f
#define sleefify_vecf64_u10(base_name) Sleef_##base_name##d8_u10avx512f

#define sleefify_vecf32(base_name) Sleef_##base_name##f16_avx512f
#define sleefify_vecf64(base_name) Sleef_##base_name##d8_avx512f

typedef __m512 vecf32;
typedef __m512d vecf64;
typedef __mmask8 boolf64;
typedef __mmask16 boolf32;

const int SIMD_SINGLE_STRIDE = 16;
const int SIMD_DOUBLE_STRIDE = 8;

vecf32 static inline vecf32_load(float* ptr){ return _mm512_loadu_ps(ptr); }
void static inline vecf32_store(float* ptr, vecf32 v){ return _mm512_storeu_ps(ptr, v); }

vecf64 static inline vecf64_load(double* ptr){ return _mm512_loadu_pd(ptr); }
void static inline vecf64_store(double* ptr, vecf64 v){ return _mm512_storeu_pd(ptr, v); }


vecf32 static inline Sleef_addf16_u10avx512f(vecf32 a, vecf32 b){return _mm512_add_ps(a, b);}
vecf32 static inline Sleef_subf16_u10avx512f(vecf32 a, vecf32 b){return _mm512_sub_ps(a, b);}
vecf32 static inline Sleef_mulf16_u10avx512f(vecf32 a, vecf32 b){return _mm512_mul_ps(a, b);}
vecf32 static inline Sleef_divf16_u10avx512f(vecf32 a, vecf32 b){return _mm512_div_ps(a, b);}


// FIXME: Prolly not the most efficient way to do things via bitshifting
void static inline vecf32_store_bool(boolf32 v, _Bool* ptr, const long stride){
  for(int i=0; i<SIMD_SINGLE_STRIDE; i++) (ptr+i*stride)[0] = (v>>i)&1;
}
void static inline vecf64_store_bool(boolf64 v, _Bool* ptr, const long stride){
  for(int i=0; i<SIMD_DOUBLE_STRIDE; i++) (ptr+i*stride)[0] = (v>>i)&i;
}


boolf32 static inline Sleef_ltf16_avx512f(vecf32 a, vecf32 b){
  return _mm512_cmp_ps_mask(a, b, _CMP_LT_OQ);
}
boolf32 static inline Sleef_lef16_avx512f(vecf32 a, vecf32 b){
  return _mm512_cmp_ps_mask(a, b, _CMP_LE_OQ);
}
boolf32 static inline Sleef_eqf16_avx512f(vecf32 a, vecf32 b){
  return _mm512_cmp_ps_mask(a, b, _CMP_EQ_OQ);
}
boolf32 static inline Sleef_neqf16_avx512f(vecf32 a, vecf32 b){
  return _mm512_cmp_ps_mask(a, b, _CMP_NEQ_OQ);
}
boolf32 static inline Sleef_gtf16_avx512f(vecf32 a, vecf32 b){
  return _mm512_cmp_ps_mask(a, b, _CMP_GT_OQ);
}
boolf32 static inline Sleef_gef16_avx512f(vecf32 a, vecf32 b){
  return _mm512_cmp_ps_mask(a, b, _CMP_GE_OQ);
}


vecf64 static inline Sleef_addd8_u10avx512f(vecf64 a, vecf64 b){return _mm512_add_pd(a, b);}
vecf64 static inline Sleef_subd8_u10avx512f(vecf64 a, vecf64 b){return _mm512_sub_pd(a, b);}
vecf64 static inline Sleef_muld8_u10avx512f(vecf64 a, vecf64 b){return _mm512_mul_pd(a, b);}
vecf64 static inline Sleef_divd8_u10avx512f(vecf64 a, vecf64 b){return _mm512_div_pd(a, b);}


boolf64 static inline Sleef_ltd8_avx512f(vecf64 a, vecf64 b){
  return _mm512_cmp_pd_mask(a, b, _CMP_LT_OQ);
}
boolf64 static inline Sleef_led8_avx512f(vecf64 a, vecf64 b){
  return _mm512_cmp_pd_mask(a, b, _CMP_LE_OQ);
}
boolf64 static inline Sleef_eqd8_avx512f(vecf64 a, vecf64 b){
  return _mm512_cmp_pd_mask(a, b, _CMP_EQ_OQ);
}
boolf64 static inline Sleef_neqd8_avx512f(vecf64 a, vecf64 b){
  return _mm512_cmp_pd_mask(a, b, _CMP_NEQ_OQ);
}
boolf64 static inline Sleef_gtd8_avx512f(vecf64 a, vecf64 b){
  return _mm512_cmp_pd_mask(a, b, _CMP_GT_OQ);
}
boolf64 static inline Sleef_ged8_avx512f(vecf64 a, vecf64 b){
  return _mm512_cmp_pd_mask(a, b, _CMP_GE_OQ);
}



#elif defined(__AVX2__)





#include "sleef/sleefinline_avx2.h"

#define sleefify_vecf32_u10(base_name) Sleef_##base_name##f8_u10avx2
#define sleefify_vecf64_u10(base_name) Sleef_##base_name##d4_u10avx2

#define sleefify_vecf32(base_name) Sleef_##base_name##f8_avx2
#define sleefify_vecf64(base_name) Sleef_##base_name##d4_avx2

typedef __m256 vecf32;
typedef __m256d vecf64;
typedef __m256 boolf32;
typedef __m256d boolf64;
const int SIMD_SINGLE_STRIDE = 8;
const int SIMD_DOUBLE_STRIDE = 4;

vecf32 static inline vecf32_load(float* ptr){ return _mm256_loadu_ps(ptr); }
void static inline vecf32_store(float* ptr, vecf32 v){ return _mm256_storeu_ps(ptr, v); }

vecf64 static inline vecf64_load(double* ptr){ return _mm256_loadu_pd(ptr); }
void static inline vecf64_store(double* ptr, vecf64 v){ return _mm256_storeu_pd(ptr, v); }

void static inline vecf32_store_bool(vecf32 v, _Bool* ptr, const long stride){
  for(int i=0; i<SIMD_SINGLE_STRIDE; i++) (ptr+i*stride)[0] = v[i];
}
void static inline vecf64_store_bool(vecf64 v, _Bool* ptr, const long stride){
  for(int i=0; i<SIMD_DOUBLE_STRIDE; i++) (ptr+i*stride)[0] = v[i];
}

vecf32 static inline Sleef_addf8_u10avx2(vecf32 a, vecf32 b){return _mm256_add_ps(a, b);}
vecf32 static inline Sleef_subf8_u10avx2(vecf32 a, vecf32 b){return _mm256_sub_ps(a, b);}
vecf32 static inline Sleef_mulf8_u10avx2(vecf32 a, vecf32 b){return _mm256_mul_ps(a, b);}
vecf32 static inline Sleef_divf8_u10avx2(vecf32 a, vecf32 b){return _mm256_div_ps(a, b);}

// A quick test on numpy will suggest that numpy uses the "ordered" "non-signalling"
// comparisons
// - https://www.felixcloutier.com/x86/cmppd#tbl-3-1
boolf32 static inline Sleef_ltf8_avx2(vecf32 a, vecf32 b){
  return _mm256_cmp_ps(a, b, _CMP_LT_OQ);
}
boolf32 static inline Sleef_lef8_avx2(vecf32 a, vecf32 b){
  return _mm256_cmp_ps(a, b, _CMP_LE_OQ);
}
boolf32 static inline Sleef_eqf8_avx2(vecf32 a, vecf32 b){
  return _mm256_cmp_ps(a, b, _CMP_EQ_OQ);
}
boolf32 static inline Sleef_neqf8_avx2(vecf32 a, vecf32 b){
  return _mm256_cmp_ps(a, b, _CMP_NEQ_OQ);
}
boolf32 static inline Sleef_gtf8_avx2(vecf32 a, vecf32 b){
  return _mm256_cmp_ps(a, b, _CMP_GT_OQ);
}
boolf32 static inline Sleef_gef8_avx2(vecf32 a, vecf32 b){
  return _mm256_cmp_ps(a, b, _CMP_GE_OQ);
}


vecf64 static inline Sleef_addd4_u10avx2(vecf64 a, vecf64 b){return _mm256_add_pd(a, b);}
vecf64 static inline Sleef_subd4_u10avx2(vecf64 a, vecf64 b){return _mm256_sub_pd(a, b);}
vecf64 static inline Sleef_muld4_u10avx2(vecf64 a, vecf64 b){return _mm256_mul_pd(a, b);}
vecf64 static inline Sleef_divd4_u10avx2(vecf64 a, vecf64 b){return _mm256_div_pd(a, b);}

boolf64 static inline Sleef_ltd4_avx2(vecf64 a, vecf64 b){
  return _mm256_cmp_pd(a, b, _CMP_LT_OQ);
}
boolf64 static inline Sleef_led4_avx2(vecf64 a, vecf64 b){
  return _mm256_cmp_pd(a, b, _CMP_LE_OQ);
}
boolf64 static inline Sleef_eqd4_avx2(vecf64 a, vecf64 b){
  return _mm256_cmp_pd(a, b, _CMP_EQ_OQ);
}
boolf64 static inline Sleef_neqd4_avx2(vecf64 a, vecf64 b){
  return _mm256_cmp_pd(a, b, _CMP_NEQ_OQ);
}
boolf64 static inline Sleef_gtd4_avx2(vecf64 a, vecf64 b){
  return _mm256_cmp_pd(a, b, _CMP_GT_OQ);
}
boolf64 static inline Sleef_ged4_avx2(vecf64 a, vecf64 b){
  return _mm256_cmp_pd(a, b, _CMP_GE_OQ);
}



#else




#include "sleef/sleefinline_sse2.h"

#define sleefify_vecf32_u10(base_name) Sleef_##base_name##f4_u10sse2
#define sleefify_vecf64_u10(base_name) Sleef_##base_name##d2_u10sse2

#define sleefify_vecf32(base_name) Sleef_##base_name##f4_sse2
#define sleefify_vecf64(base_name) Sleef_##base_name##d2_sse2

typedef __m128 vecf32;
typedef __m128d vecf64;
typedef __m128 boolf32;
typedef __m128d boolf64;

const int SIMD_SINGLE_STRIDE = 4;
const int SIMD_DOUBLE_STRIDE = 2;

vecf32 static inline vecf32_load(float* ptr){ return _mm_loadu_ps(ptr); }
void static inline vecf32_store(float* ptr, vecf32 v){ return _mm_storeu_ps(ptr, v); }

vecf64 static inline vecf64_load(double* ptr){ return _mm_loadu_pd(ptr); }
void static inline vecf64_store(double* ptr, vecf64 v){ return _mm_storeu_pd(ptr, v); }

void static inline vecf32_store_bool(vecf32 v, _Bool* ptr, const long stride){
  for(int i=0; i<SIMD_SINGLE_STRIDE; i++) (ptr+i*stride)[0] = v[i];
}
void static inline vecf64_store_bool(vecf64 v, _Bool* ptr, const long stride){
  for(int i=0; i<SIMD_DOUBLE_STRIDE; i++) (ptr+i*stride)[0] = v[i];
}


vecf32 static inline Sleef_addf4_u10sse2(vecf32 a, vecf32 b){return _mm_add_ps(a, b);}
vecf32 static inline Sleef_subf4_u10sse2(vecf32 a, vecf32 b){return _mm_sub_ps(a, b);}
vecf32 static inline Sleef_mulf4_u10sse2(vecf32 a, vecf32 b){return _mm_mul_ps(a, b);}
vecf32 static inline Sleef_divf4_u10sse2(vecf32 a, vecf32 b){return _mm_div_ps(a, b);}

boolf32 static inline Sleef_ltf4_sse2(vecf32 a, vecf32 b){return _mm_cmplt_ps(a, b);}
boolf32 static inline Sleef_lef4_sse2(vecf32 a, vecf32 b){return _mm_cmple_ps(a, b);}
boolf32 static inline Sleef_eqf4_sse2(vecf32 a, vecf32 b){return _mm_cmpeq_ps(a, b);}
boolf32 static inline Sleef_neqf4_sse2(vecf32 a, vecf32 b){return _mm_cmpneq_ps(a, b);}
boolf32 static inline Sleef_gtf4_sse2(vecf32 a, vecf32 b){return _mm_cmpgt_ps(a, b);}
boolf32 static inline Sleef_gef4_sse2(vecf32 a, vecf32 b){return _mm_cmpge_ps(a, b);}


vecf64 static inline Sleef_addd2_u10sse2(vecf64 a, vecf64 b){return _mm_add_pd(a, b);}
vecf64 static inline Sleef_subd2_u10sse2(vecf64 a, vecf64 b){return _mm_sub_pd(a, b);}
vecf64 static inline Sleef_muld2_u10sse2(vecf64 a, vecf64 b){return _mm_mul_pd(a, b);}
vecf64 static inline Sleef_divd2_u10sse2(vecf64 a, vecf64 b){return _mm_div_pd(a, b);}

boolf64 static inline Sleef_ltd2_sse2(vecf64 a, vecf64 b){return _mm_cmplt_pd(a, b);}
boolf64 static inline Sleef_led2_sse2(vecf64 a, vecf64 b){return _mm_cmple_pd(a, b);}
boolf64 static inline Sleef_eqd2_sse2(vecf64 a, vecf64 b){return _mm_cmpeq_pd(a, b);}
boolf64 static inline Sleef_neqd2_sse2(vecf64 a, vecf64 b){return _mm_cmpneq_pd(a, b);}
boolf64 static inline Sleef_gtd2_sse2(vecf64 a, vecf64 b){return _mm_cmpgt_pd(a, b);}
boolf64 static inline Sleef_ged2_sse2(vecf64 a, vecf64 b){return _mm_cmpge_pd(a, b);}



#endif





#endif // __x64_64
