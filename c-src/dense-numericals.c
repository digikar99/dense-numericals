#include<sleef.h>
#include<math.h>

#if defined(__x86_64)

// TODO: Separate architecture specific code into separate files and configure
// Makefile accordingly

typedef __m256 vecf32;
const int SIMD_SINGLE_STRIDE = 8;
vecf32 static inline vecf32_load(float* ptr){ return _mm256_loadu_ps(ptr); }
void static inline vecf32_store(float* ptr, vecf32 v){ return _mm256_storeu_ps(ptr, v); }
vecf32 static inline vecf32_make(float* ptr, const long stride){
  return _mm256_set_ps(
    (ptr+7*stride)[0],
    (ptr+6*stride)[0],
    (ptr+5*stride)[0],
    (ptr+4*stride)[0],
    (ptr+3*stride)[0],
    (ptr+2*stride)[0],
    (ptr+stride)[0],
    ptr[0]
    );
}
void static inline vecf32_store_multi(vecf32 v, float* ptr, const long stride){
  // TODO: Optimize this
  for(int i=0; i< SIMD_SINGLE_STRIDE; i++) (ptr+i*stride)[0] = v[i];
}

typedef __m256d vecf64;
const int SIMD_DOUBLE_STRIDE = 4;
vecf64 static inline vecf64_load(double* ptr){ return _mm256_loadu_pd(ptr); }
void static inline vecf64_store(double* ptr, vecf64 v){ return _mm256_storeu_pd(ptr, v); }
vecf64 static inline vecf64_make(double* ptr, const long stride){
  return _mm256_set_pd(
    (ptr+3*stride)[0],
    (ptr+2*stride)[0],
    (ptr+stride)[0],
    ptr[0]
    );
}
void static inline vecf64_store_multi(vecf64 v, double* ptr, const long stride){
  // TODO: Optimize this
  for(int i=0; i< SIMD_DOUBLE_STRIDE; i++) (ptr+i*stride)[0] = v[i];
}

#endif

void DN_ssin(const long n,
             float* x, const long ox, const long incx,
             float* y, const long oy, const long incy){
  x += ox, y += oy;
  float* y_end = y + incy * n;
  vecf32 va, vb;
  if (incx == 1 && incy == 1){
    float* simd_end = y + (n/SIMD_SINGLE_STRIDE)*SIMD_SINGLE_STRIDE;
    while(y != simd_end){
      va = vecf32_load(x);
      vb = Sleef_sinf8_u10(va);
      vecf32_store(y, vb);
      x += SIMD_SINGLE_STRIDE;
      y += SIMD_SINGLE_STRIDE;
    }
  }else if(incy == 1){
    float* simd_end = y + (n/SIMD_SINGLE_STRIDE)*SIMD_SINGLE_STRIDE;
    while(y != simd_end){
      va = vecf32_make(x, incx);
      vb = Sleef_sinf8_u10(va);
      vecf32_store(y, vb);
      x += SIMD_SINGLE_STRIDE*incx;
      y += SIMD_SINGLE_STRIDE;
    }
  }else if(incx == 1){
    float* simd_end = x + (n/SIMD_SINGLE_STRIDE)*SIMD_SINGLE_STRIDE;
    while(x != simd_end){
      va = vecf32_load(x);
      vb = Sleef_sinf8_u10(va);
      vecf32_store_multi(vb, y, incy);
      x += SIMD_SINGLE_STRIDE;
      y += SIMD_SINGLE_STRIDE*incy;
    }
  }else{
    long i=0;
    const long simd_end = (n/SIMD_SINGLE_STRIDE)*SIMD_SINGLE_STRIDE;
    while(i != simd_end){
      va = vecf32_make(x, incx);
      vb = Sleef_sinf8_u10(va);
      vecf32_store_multi(vb, y, incy);
      i += SIMD_SINGLE_STRIDE;
      x += SIMD_SINGLE_STRIDE * incx;
      y += SIMD_SINGLE_STRIDE * incy;
    }
  }
  
  while(y!=y_end){
    y[0] = sin(x[0]);
    x += incx;
    y += incy;
  }
};

void DN_dsin(const long n,
             double* x, const long ox, const long incx,
             double* y, const long oy, const long incy){
  x += ox, y += oy;
  double* y_end = y + incy * n;
  vecf64 va, vb;
  if (incx == 1 && incy == 1){
    double* simd_end = y + (n/SIMD_DOUBLE_STRIDE)*SIMD_DOUBLE_STRIDE;
    while(y != simd_end){
      va = vecf64_load(x);
      vb = Sleef_sind4_u10(va);
      vecf64_store(y, vb);
      x += SIMD_DOUBLE_STRIDE;
      y += SIMD_DOUBLE_STRIDE;
    }
  }else if(incy == 1){
    double* simd_end = y + (n/SIMD_DOUBLE_STRIDE)*SIMD_DOUBLE_STRIDE;
    while(y != simd_end){
      va = vecf64_make(x, incx);
      vb = Sleef_sind4_u10(va);
      vecf64_store(y, vb);
      x += SIMD_DOUBLE_STRIDE*incx;
      y += SIMD_DOUBLE_STRIDE;
    }
  }else if(incx == 1){
    double* simd_end = x + (n/SIMD_DOUBLE_STRIDE)*SIMD_DOUBLE_STRIDE;
    while(x != simd_end){
      va = vecf64_load(x);
      vb = Sleef_sind4_u10(va);
      vecf64_store_multi(vb, y, incy);
      x += SIMD_DOUBLE_STRIDE;
      y += SIMD_DOUBLE_STRIDE*incy;
    }
  }else{
    long i=0;
    const long simd_end = (n/SIMD_DOUBLE_STRIDE)*SIMD_DOUBLE_STRIDE;
    while(i != simd_end){
      va = vecf64_make(x, incx);
      vb = Sleef_sind4_u10(va);
      vecf64_store_multi(vb, y, incy);
      i += SIMD_DOUBLE_STRIDE;
      x += SIMD_DOUBLE_STRIDE * incx;
      y += SIMD_DOUBLE_STRIDE * incy;
    }
  }
  
  while(y!=y_end){
    y[0] = sin(x[0]);
    x += incx;
    y += incy;
  }
};

