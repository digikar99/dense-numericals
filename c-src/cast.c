
void DN_cast_sd(const long n,
                float* x, const long incx,
                double* y, const long incy){
  double* y_end = y + incy * n;
  vecf32h va;
  vecf64 vb;
  if (incx == 1 && incy == 1){
    double* simd_end = y + (n/SIMD_DOUBLE_STRIDE)*SIMD_DOUBLE_STRIDE;
    while(y != simd_end){
      va = vecf32h_load(x);
      vb = vecf32h_to_vecf64(va);
      vecf64_store(y, vb);
      x += SIMD_DOUBLE_STRIDE;
      y += SIMD_DOUBLE_STRIDE;
    }
  }else if(incy == 1){
    double* simd_end = y + (n/SIMD_DOUBLE_STRIDE)*SIMD_DOUBLE_STRIDE;
    while(y != simd_end){
      va = vecf32h_make(x, incx);
      vb = vecf32h_to_vecf64(va);
      vecf64_store(y, vb);
      x += SIMD_DOUBLE_STRIDE*incx;
      y += SIMD_DOUBLE_STRIDE;
    }
  }else if(incx == 1){
    float* simd_end = x + (n/SIMD_DOUBLE_STRIDE)*SIMD_DOUBLE_STRIDE;
    while(x != simd_end){
      va = vecf32h_load(x);
      vb = vecf32h_to_vecf64(va);
      vecf64_store_multi(vb, y, incy);
      x += SIMD_DOUBLE_STRIDE;
      y += SIMD_DOUBLE_STRIDE*incy;
    }
  }else{
    long i=0;
    const long simd_end = (n/SIMD_DOUBLE_STRIDE)*SIMD_DOUBLE_STRIDE;
    while(i != simd_end){
      va = vecf32h_make(x, incx);
      vb = vecf32h_to_vecf64(va);
      vecf64_store_multi(vb, y, incy);
      i += SIMD_DOUBLE_STRIDE;
      x += SIMD_DOUBLE_STRIDE * incx;
      y += SIMD_DOUBLE_STRIDE * incy;
    }
  }

  while(y!=y_end){
    y[0] = (double)(x[0]);
    x += incx;
    y += incy;
  }
};


void DN_cast_ds(const long n,
                double* x, const long incx,
                float* y, const long incy){
  float* y_end = y + incy * n;
  vecf64 va;
  vecf32h vb;
  if (incx == 1 && incy == 1){
    float* simd_end = y + (n/SIMD_DOUBLE_STRIDE)*SIMD_DOUBLE_STRIDE;
    while(y != simd_end){
      va = vecf64_load(x);
      vb = vecf64_to_vecf32h(va);
      vecf32h_store(y, vb);
      x += SIMD_DOUBLE_STRIDE;
      y += SIMD_DOUBLE_STRIDE;
    }
  }else if(incy == 1){
    float* simd_end = y + (n/SIMD_DOUBLE_STRIDE)*SIMD_DOUBLE_STRIDE;
    while(y != simd_end){
      va = vecf64_make(x, incx);
      vb = vecf64_to_vecf32h(va);
      vecf32h_store(y, vb);
      x += SIMD_DOUBLE_STRIDE*incx;
      y += SIMD_DOUBLE_STRIDE;
    }
  }else if(incx == 1){
    double* simd_end = x + (n/SIMD_DOUBLE_STRIDE)*SIMD_DOUBLE_STRIDE;
    while(x != simd_end){
      va = vecf64_load(x);
      vb = vecf64_to_vecf32h(va);
      vecf32h_store_multi(vb, y, incy);
      x += SIMD_DOUBLE_STRIDE;
      y += SIMD_DOUBLE_STRIDE*incy;
    }
  }else{
    long i=0;
    const long simd_end = (n/SIMD_DOUBLE_STRIDE)*SIMD_DOUBLE_STRIDE;
    while(i != simd_end){
      va = vecf64_make(x, incx);
      vb = vecf64_to_vecf32h(va);
      vecf32h_store_multi(vb, y, incy);
      i += SIMD_DOUBLE_STRIDE;
      x += SIMD_DOUBLE_STRIDE * incx;
      y += SIMD_DOUBLE_STRIDE * incy;
    }
  }

  while(y!=y_end){
    y[0] = (float)(x[0]);
    x += incx;
    y += incy;
  }
};
