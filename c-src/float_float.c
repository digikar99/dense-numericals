

#define one_arg_fn_body_ff(oaf)                                         \
  void DN_s##oaf(const long n,                                          \
                 float* x, const long incx,                             \
                 float* y, const long incy){                            \
    float* y_end = y + incy * n;                                        \
    vecf32 va, vb;                                                      \
    if (incx == 1 && incy == 1){                                        \
      float* simd_end = y + (n/SIMD_SINGLE_STRIDE)*SIMD_SINGLE_STRIDE;  \
      while(y != simd_end){                                             \
        va = vecf32_load(x);                                            \
        vb = sleefify_vecf32(oaf)(va);                                  \
        vecf32_store(y, vb);                                            \
        x += SIMD_SINGLE_STRIDE;                                        \
        y += SIMD_SINGLE_STRIDE;                                        \
      }                                                                 \
    }else if(incy == 1){                                                \
      float* simd_end = y + (n/SIMD_SINGLE_STRIDE)*SIMD_SINGLE_STRIDE;  \
      while(y != simd_end){                                             \
        va = vecf32_make(x, incx);                                      \
        vb = sleefify_vecf32(oaf)(va);                                  \
        vecf32_store(y, vb);                                            \
        x += SIMD_SINGLE_STRIDE*incx;                                   \
        y += SIMD_SINGLE_STRIDE;                                        \
      }                                                                 \
    }else if(incx == 1){                                                \
      float* simd_end = x + (n/SIMD_SINGLE_STRIDE)*SIMD_SINGLE_STRIDE;  \
      while(x != simd_end){                                             \
        va = vecf32_load(x);                                            \
        vb = sleefify_vecf32(oaf)(va);                                  \
        vecf32_store_multi(vb, y, incy);                                \
        x += SIMD_SINGLE_STRIDE;                                        \
        y += SIMD_SINGLE_STRIDE*incy;                                   \
      }                                                                 \
    }else{                                                              \
      long i=0;                                                         \
      const long simd_end = (n/SIMD_SINGLE_STRIDE)*SIMD_SINGLE_STRIDE;  \
      while(i != simd_end){                                             \
        va = vecf32_make(x, incx);                                      \
        vb = sleefify_vecf32(oaf)(va);                                  \
        vecf32_store_multi(vb, y, incy);                                \
        i += SIMD_SINGLE_STRIDE;                                        \
        x += SIMD_SINGLE_STRIDE * incx;                                 \
        y += SIMD_SINGLE_STRIDE * incy;                                 \
      }                                                                 \
    }                                                                   \
                                                                        \
    while(y!=y_end){                                                    \
      y[0] = sleefify_f32(oaf)(x[0]);                                   \
      x += incx;                                                        \
      y += incy;                                                        \
    }                                                                   \
  };                                                                    \
                                                                        \
  void DN_d##oaf(const long n,                                          \
                 double* x, const long incx,                            \
                 double* y, const long incy){                           \
    double* y_end = y + incy * n;                                       \
    vecf64 va, vb;                                                      \
    if (incx == 1 && incy == 1){                                        \
      double* simd_end = y + (n/SIMD_DOUBLE_STRIDE)*SIMD_DOUBLE_STRIDE; \
      while(y != simd_end){                                             \
        va = vecf64_load(x);                                            \
        vb = sleefify_vecf64(oaf)(va);                                  \
        vecf64_store(y, vb);                                            \
        x += SIMD_DOUBLE_STRIDE;                                        \
        y += SIMD_DOUBLE_STRIDE;                                        \
      }                                                                 \
    }else if(incy == 1){                                                \
      double* simd_end = y + (n/SIMD_DOUBLE_STRIDE)*SIMD_DOUBLE_STRIDE; \
      while(y != simd_end){                                             \
        va = vecf64_make(x, incx);                                      \
        vb = sleefify_vecf64(oaf)(va);                                  \
        vecf64_store(y, vb);                                            \
        x += SIMD_DOUBLE_STRIDE*incx;                                   \
        y += SIMD_DOUBLE_STRIDE;                                        \
      }                                                                 \
    }else if(incx == 1){                                                \
      double* simd_end = x + (n/SIMD_DOUBLE_STRIDE)*SIMD_DOUBLE_STRIDE; \
      while(x != simd_end){                                             \
        va = vecf64_load(x);                                            \
        vb = sleefify_vecf64(oaf)(va);                                  \
        vecf64_store_multi(vb, y, incy);                                \
        x += SIMD_DOUBLE_STRIDE;                                        \
        y += SIMD_DOUBLE_STRIDE*incy;                                   \
      }                                                                 \
    }else{                                                              \
      long i=0;                                                         \
      const long simd_end = (n/SIMD_DOUBLE_STRIDE)*SIMD_DOUBLE_STRIDE;  \
      while(i != simd_end){                                             \
        va = vecf64_make(x, incx);                                      \
        vb = sleefify_vecf64(oaf)(va);                                  \
        vecf64_store_multi(vb, y, incy);                                \
        i += SIMD_DOUBLE_STRIDE;                                        \
        x += SIMD_DOUBLE_STRIDE * incx;                                 \
        y += SIMD_DOUBLE_STRIDE * incy;                                 \
      }                                                                 \
    }                                                                   \
                                                                        \
    while(y!=y_end){                                                    \
      y[0] = sleefify_f64(oaf)(x[0]);                                   \
      x += incx;                                                        \
      y += incy;                                                        \
    }                                                                   \
  };
