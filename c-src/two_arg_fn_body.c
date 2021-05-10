
#define two_arg_fn_body(name, sleef_name, stride, itype, ivec, iscalar, otype, ovec) \
  void DN_##name(const long n,                                          \
                 itype *x, const long incx,                             \
                 itype *y, const long incy,                             \
                 otype *out, const long inc_out){                       \
    otype *out_end = out + inc_out * n;                                 \
    ivec va, vb;                                                        \
    ovec vc;                                                            \
    if (incx == 1 && incy == 1 && inc_out == 1){                        \
      otype *simd_end = out + (n/stride)*stride;                        \
      while(out != simd_end){                                           \
        va = ivec##_load(x);                                            \
        vb = ivec##_load(y);                                            \
        vc = sleefify_##ivec(sleef_name)(va, vb);                       \
        ovec##_store(out, vc);                                          \
        x += stride;                                                    \
        y += stride;                                                    \
        out += stride;                                                  \
      }                                                                 \
    }else if(incy == 1 && inc_out == 1){                                \
      otype *simd_end = out + (n/stride)*stride;                        \
      while(out != simd_end){                                           \
        va = ivec##_make(x, incx);                                      \
        vb = ivec##_load(y);                                            \
        vc = sleefify_##ivec(sleef_name)(va, vb);                       \
        ovec##_store(out, vc);                                          \
        x += stride*incx;                                               \
        y += stride;                                                    \
        out += stride;                                                  \
      }                                                                 \
    }else if(incx == 1 && inc_out == 1){                                \
      otype *simd_end = out + (n/stride)*stride;                        \
      while(out != simd_end){                                           \
        va = ivec##_load(x);                                            \
        vb = ivec##_make(y, incy);                                      \
        vc = sleefify_##ivec(sleef_name)(va, vb);                       \
        ovec##_store(out, vc);                                          \
        x += stride;                                                    \
        y += stride*incy;                                               \
        out += stride;                                                  \
      }                                                                 \
    }else if(incx == 1 && incy == 1){                                   \
      itype *simd_end = x + (n/stride)*stride;                          \
      while(x != simd_end){                                             \
        va = ivec##_load(x);                                            \
        vb = ivec##_load(y);                                            \
        vc = sleefify_##ivec(sleef_name)(va, vb);                       \
        ovec##_store_multi(vc, out, inc_out);                           \
        x += stride;                                                    \
        y += stride;                                                    \
        out += stride*inc_out;                                          \
      }                                                                 \
    }else if(inc_out == 1){                                             \
      otype *simd_end = out + (n/stride)*stride;                        \
      while(out != simd_end){                                           \
        va = ivec##_make(x, incx);                                      \
        vb = ivec##_make(y, incy);                                      \
        vc = sleefify_##ivec(sleef_name)(va, vb);                       \
        ovec##_store(out, vc);                                          \
        x += stride*incx;                                               \
        y += stride*incy;                                               \
        out += stride;                                                  \
      }                                                                 \
    }else if(incy == 1){                                                \
      itype *simd_end = y + (n/stride)*stride;                          \
      while(y != simd_end){                                             \
        va = ivec##_make(x, incx);                                      \
        vb = ivec##_load(y);                                            \
        vc = sleefify_##ivec(sleef_name)(va, vb);                       \
        ovec##_store_multi(vc, out, inc_out);                           \
        x += stride*incx;                                               \
        y += stride;                                                    \
        out += stride*inc_out;                                          \
      }                                                                 \
    }else if(incx == 1){                                                \
      itype *simd_end = x + (n/stride)*stride;                          \
      while(x != simd_end){                                             \
        va = ivec##_load(x);                                            \
        vb = ivec##_make(y, incy);                                      \
        vc = sleefify_##ivec(sleef_name)(va, vb);                       \
        ovec##_store_multi(vc, out, inc_out);                           \
        x += stride;                                                    \
        y += stride*incy;                                               \
        out += stride*inc_out;                                          \
      }                                                                 \
    }else{                                                              \
      long i=0;                                                         \
      const long simd_end = (n/stride)*stride;                          \
      while(i != simd_end){                                             \
        va = ivec##_make(x, incx);                                      \
        vb = ivec##_make(y, incy);                                      \
        vc = sleefify_##ivec(sleef_name)(va, vb);                       \
        ovec##_store_multi(vc, out, inc_out);                           \
        i += stride;                                                    \
        x += stride*incx;                                               \
        y += stride*incy;                                               \
        out += stride*inc_out;                                          \
      }                                                                 \
    }                                                                   \
                                                                        \
    while(out!=out_end){                                                \
      out[0] = sleefify_##iscalar(sleef_name)(x[0], y[0]);              \
      x += incx;                                                        \
      y += incy;                                                        \
      out += inc_out;                                                   \
    }                                                                   \
  };                                                                    