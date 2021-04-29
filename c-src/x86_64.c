#if defined(__x86_64)

#include <immintrin.h>

#if defined(__AVX512F__)

#include "sleef/sleefinline_avx512f.h"

#define sleefify_vecf32(base_name) Sleef_##base_name##f16_u10avx512f
#define sleefify_vecf64(base_name) Sleef_##base_name##d8_u10avx512f

typedef __m512 vecf32;
typedef __m512d vecf64;
const int SIMD_SINGLE_STRIDE = 16;
const int SIMD_DOUBLE_STRIDE = 8;

vecf32 static inline vecf32_load(float* ptr){ return _mm512_loadu_ps(ptr); }
void static inline vecf32_store(float* ptr, vecf32 v){ return _mm512_storeu_ps(ptr, v); }

vecf64 static inline vecf64_load(double* ptr){ return _mm512_loadu_pd(ptr); }
void static inline vecf64_store(double* ptr, vecf64 v){ return _mm512_storeu_pd(ptr, v); }

#elif defined(__AVX2__)

#include "sleef/sleefinline_avx2.h"

#define sleefify_vecf32(base_name) Sleef_##base_name##f8_u10avx2
#define sleefify_vecf64(base_name) Sleef_##base_name##d4_u10avx2

typedef __m256 vecf32;
typedef __m256d vecf64;
const int SIMD_SINGLE_STRIDE = 8;
const int SIMD_DOUBLE_STRIDE = 4;

vecf32 static inline vecf32_load(float* ptr){ return _mm256_loadu_ps(ptr); }
void static inline vecf32_store(float* ptr, vecf32 v){ return _mm256_storeu_ps(ptr, v); }

vecf64 static inline vecf64_load(double* ptr){ return _mm256_loadu_pd(ptr); }
void static inline vecf64_store(double* ptr, vecf64 v){ return _mm256_storeu_pd(ptr, v); }

#else

#include "sleef/sleefinline_sse4.h"

#define sleefify_vecf32(base_name) Sleef_##base_name##f4_u10sse4
#define sleefify_vecf64(base_name) Sleef_##base_name##d2_u10sse4

typedef __m128 vecf32;
typedef __m128d vecf64;
const int SIMD_SINGLE_STRIDE = 4;
const int SIMD_DOUBLE_STRIDE = 2;

vecf32 static inline vecf32_load(float* ptr){ return _mm_loadu_ps(ptr); }
void static inline vecf32_store(float* ptr, vecf32 v){ return _mm_storeu_ps(ptr, v); }

vecf64 static inline vecf64_load(double* ptr){ return _mm_loadu_pd(ptr); }
void static inline vecf64_store(double* ptr, vecf64 v){ return _mm_storeu_pd(ptr, v); }

#endif

#endif // __x64_64
