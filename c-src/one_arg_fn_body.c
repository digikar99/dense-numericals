

#define one_arg_fn_body_u10(name, sleef_name, stride, itype, ivec, iscalar, otype, ovec) \
  void DN_##name(const long n,                                          \
                 itype *x, const long incx,                             \
                 otype* y, const long incy){                            \
    otype* y_end = y + incy * n;                                        \
    ivec va;                                                            \
    ovec vb;                                                            \
    if (incx == 1 && incy == 1){                                        \
      otype* simd_end = y + (n/stride)*stride;  \
      while(y != simd_end){                                             \
        va = ivec##_load(x);                                            \
        vb = sleefify_##ivec##_u10(sleef_name)(va);                     \
        ovec##_store(y, vb);                                            \
        x += stride;                                        \
        y += stride;                                        \
      }                                                                 \
    }else if(incy == 1){                                                \
      otype* simd_end = y + (n/stride)*stride;  \
      while(y != simd_end){                                             \
        va = ivec##_make(x, incx);                                      \
        vb = sleefify_##ivec##_u10(sleef_name)(va);                     \
        ovec##_store(y, vb);                                            \
        x += stride*incx;                                   \
        y += stride;                                        \
      }                                                                 \
    }else if(incx == 1){                                                \
      itype* simd_end = x + (n/stride)*stride;  \
      while(x != simd_end){                                             \
        va = ivec##_load(x);                                            \
        vb = sleefify_##ivec##_u10(sleef_name)(va);                     \
        ovec##_store_multi(vb, y, incy);                                \
        x += stride;                                        \
        y += stride*incy;                                   \
      }                                                                 \
    }else{                                                              \
      long i=0;                                                         \
      const long simd_end = (n/stride)*stride;  \
      while(i != simd_end){                                             \
        va = ivec##_make(x, incx);                                      \
        vb = sleefify_##ivec##_u10(sleef_name)(va);                     \
        ovec##_store_multi(vb, y, incy);                                \
        i += stride;                                        \
        x += stride * incx;                                 \
        y += stride * incy;                                 \
      }                                                                 \
    }                                                                   \
                                                                        \
    while(y!=y_end){                                                    \
      y[0] = sleefify_##iscalar##_u10(sleef_name)(x[0]);                \
      x += incx;                                                        \
      y += incy;                                                        \
    }                                                                   \
  };                                                                    
