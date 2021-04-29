
#define two_arg_fn_body_fff(taf)                                        \
  void DN_s##taf(const long n,                                          \
                 float* x, const long incx,                             \
                 float* y, const long incy,                             \
                 float* out, const long inc_out){                       \
    float* out_end = out + inc_out * n;                                 \
    vecf32 va, vb, vc;                                                  \
    if (incx == 1 && incy == 1 && inc_out == 1){                        \
      float* simd_end = out + (n/SIMD_SINGLE_STRIDE)*SIMD_SINGLE_STRIDE; \
      while(out != simd_end){                                           \
        va = vecf32_load(x);                                            \
        vb = vecf32_load(y);                                            \
        vc = sleefify_vecf32(taf)(va, vb);                              \
        vecf32_store(out, vc);                                          \
        x += SIMD_SINGLE_STRIDE;                                        \
        y += SIMD_SINGLE_STRIDE;                                        \
        out += SIMD_SINGLE_STRIDE;                                      \
      }                                                                 \
    }else if(incy == 1 && inc_out == 1){                                \
      float* simd_end = out + (n/SIMD_SINGLE_STRIDE)*SIMD_SINGLE_STRIDE; \
      while(out != simd_end){                                           \
        va = vecf32_make(x, incx);                                      \
        vb = vecf32_load(y);                                            \
        vc = sleefify_vecf32(taf)(va, vb);                              \
        vecf32_store(out, vc);                                          \
        x += SIMD_SINGLE_STRIDE*incx;                                   \
        y += SIMD_SINGLE_STRIDE;                                        \
        out += SIMD_SINGLE_STRIDE;                                      \
      }                                                                 \
    }else if(incx == 1 && inc_out == 1){                                \
      float* simd_end = out + (n/SIMD_SINGLE_STRIDE)*SIMD_SINGLE_STRIDE; \
      while(out != simd_end){                                           \
        va = vecf32_load(x);                                            \
        vb = vecf32_make(y, incy);                                      \
        vc = sleefify_vecf32(taf)(va, vb);                              \
        vecf32_store(out, vc);                                          \
        x += SIMD_SINGLE_STRIDE;                                        \
        y += SIMD_SINGLE_STRIDE*incy;                                   \
        out += SIMD_SINGLE_STRIDE;                                      \
      }                                                                 \
    }else if(incx == 1 && incy == 1){                                   \
      float* simd_end = x + (n/SIMD_SINGLE_STRIDE)*SIMD_SINGLE_STRIDE;  \
      while(x != simd_end){                                             \
        va = vecf32_load(x);                                            \
        vb = vecf32_load(y);                                            \
        vc = sleefify_vecf32(taf)(va, vb);                              \
        vecf32_store_multi(vc, out, inc_out);                           \
        x += SIMD_SINGLE_STRIDE;                                        \
        y += SIMD_SINGLE_STRIDE;                                        \
        out += SIMD_SINGLE_STRIDE*inc_out;                              \
      }                                                                 \
    }else if(inc_out == 1){                                             \
      float* simd_end = out + (n/SIMD_SINGLE_STRIDE)*SIMD_SINGLE_STRIDE; \
      while(out != simd_end){                                           \
        va = vecf32_make(x, incx);                                      \
        vb = vecf32_make(y, incy);                                      \
        vc = sleefify_vecf32(taf)(va, vb);                              \
        vecf32_store(out, vc);                                          \
        x += SIMD_SINGLE_STRIDE*incx;                                   \
        y += SIMD_SINGLE_STRIDE*incy;                                   \
        out += SIMD_SINGLE_STRIDE;                                      \
      }                                                                 \
    }else if(incy == 1){                                                \
      float* simd_end = y + (n/SIMD_SINGLE_STRIDE)*SIMD_SINGLE_STRIDE;  \
      while(y != simd_end){                                             \
        va = vecf32_make(x, incx);                                      \
        vb = vecf32_load(y);                                            \
        vc = sleefify_vecf32(taf)(va, vb);                              \
        vecf32_store_multi(vc, out, inc_out);                           \
        x += SIMD_SINGLE_STRIDE*incx;                                   \
        y += SIMD_SINGLE_STRIDE;                                        \
        out += SIMD_SINGLE_STRIDE*inc_out;                              \
      }                                                                 \
    }else if(incx == 1){                                                \
      float* simd_end = x + (n/SIMD_SINGLE_STRIDE)*SIMD_SINGLE_STRIDE;  \
      while(x != simd_end){                                             \
        va = vecf32_load(x);                                            \
        vb = vecf32_make(y, incy);                                      \
        vc = sleefify_vecf32(taf)(va, vb);                              \
        vecf32_store_multi(vc, out, inc_out);                           \
        x += SIMD_SINGLE_STRIDE;                                        \
        y += SIMD_SINGLE_STRIDE*incy;                                   \
        out += SIMD_SINGLE_STRIDE*inc_out;                              \
      }                                                                 \
    }else{                                                              \
      long i=0;                                                         \
      const long simd_end = (n/SIMD_SINGLE_STRIDE)*SIMD_SINGLE_STRIDE;  \
      while(i != simd_end){                                             \
        va = vecf32_make(x, incx);                                      \
        vb = vecf32_make(y, incy);                                      \
        vc = sleefify_vecf32(taf)(va, vb);                              \
        vecf32_store_multi(vc, out, inc_out);                           \
        i += SIMD_SINGLE_STRIDE;                                        \
        x += SIMD_SINGLE_STRIDE*incx;                                   \
        y += SIMD_SINGLE_STRIDE*incy;                                   \
        out += SIMD_SINGLE_STRIDE*inc_out;                              \
      }                                                                 \
    }                                                                   \
                                                                        \
    while(out!=out_end){                                                \
      out[0] = sleefify_f32(taf)(x[0], y[0]);                           \
      x += incx;                                                        \
      y += incy;                                                        \
      out += inc_out;                                                   \
    }                                                                   \
  };                                                                    \
                                                                        \
  void DN_d##taf(const long n,                                          \
                 double* x, const long incx,                            \
                 double* y, const long incy,                            \
                 double* out, const long inc_out){                      \
    double* out_end = out + inc_out * n;                                \
    vecf64 va, vb, vc;                                                  \
    if (incx == 1 && incy == 1 && inc_out == 1){                        \
      double* simd_end = out + (n/SIMD_DOUBLE_STRIDE)*SIMD_DOUBLE_STRIDE; \
      while(out != simd_end){                                           \
        va = vecf64_load(x);                                            \
        vb = vecf64_load(y);                                            \
        vc = sleefify_vecf64(taf)(va, vb);                              \
        vecf64_store(out, vc);                                          \
        x += SIMD_DOUBLE_STRIDE;                                        \
        y += SIMD_DOUBLE_STRIDE;                                        \
        out += SIMD_DOUBLE_STRIDE;                                      \
      }                                                                 \
    }else if(incy == 1 && inc_out == 1){                                \
      double* simd_end = out + (n/SIMD_DOUBLE_STRIDE)*SIMD_DOUBLE_STRIDE; \
      while(out != simd_end){                                           \
        va = vecf64_make(x, incx);                                      \
        vb = vecf64_load(y);                                            \
        vc = sleefify_vecf64(taf)(va, vb);                              \
        vecf64_store(out, vc);                                          \
        x += SIMD_DOUBLE_STRIDE*incx;                                   \
        y += SIMD_DOUBLE_STRIDE;                                        \
        out += SIMD_DOUBLE_STRIDE;                                      \
      }                                                                 \
    }else if(incx == 1 && inc_out == 1){                                \
      double* simd_end = out + (n/SIMD_DOUBLE_STRIDE)*SIMD_DOUBLE_STRIDE; \
      while(out != simd_end){                                           \
        va = vecf64_load(x);                                            \
        vb = vecf64_make(y, incy);                                      \
        vc = sleefify_vecf64(taf)(va, vb);                              \
        vecf64_store(out, vc);                                          \
        x += SIMD_DOUBLE_STRIDE;                                        \
        y += SIMD_DOUBLE_STRIDE*incy;                                   \
        out += SIMD_DOUBLE_STRIDE;                                      \
      }                                                                 \
    }else if(incx == 1 && incy == 1){                                   \
      double* simd_end = x + (n/SIMD_DOUBLE_STRIDE)*SIMD_DOUBLE_STRIDE; \
      while(x != simd_end){                                             \
        va = vecf64_load(x);                                            \
        vb = vecf64_load(y);                                            \
        vc = sleefify_vecf64(taf)(va, vb);                              \
        vecf64_store_multi(vc, out, inc_out);                           \
        x += SIMD_DOUBLE_STRIDE;                                        \
        y += SIMD_DOUBLE_STRIDE;                                        \
        out += SIMD_DOUBLE_STRIDE*inc_out;                              \
      }                                                                 \
    }else if(inc_out == 1){                                             \
      double* simd_end = out + (n/SIMD_DOUBLE_STRIDE)*SIMD_DOUBLE_STRIDE; \
      while(out != simd_end){                                           \
        va = vecf64_make(x, incx);                                      \
        vb = vecf64_make(y, incy);                                      \
        vc = sleefify_vecf64(taf)(va, vb);                              \
        vecf64_store(out, vc);                                          \
        x += SIMD_DOUBLE_STRIDE*incx;                                   \
        y += SIMD_DOUBLE_STRIDE*incy;                                   \
        out += SIMD_DOUBLE_STRIDE;                                      \
      }                                                                 \
    }else if(incy == 1){                                                \
      double* simd_end = y + (n/SIMD_DOUBLE_STRIDE)*SIMD_DOUBLE_STRIDE; \
      while(y != simd_end){                                             \
        va = vecf64_make(x, incx);                                      \
        vb = vecf64_load(y);                                            \
        vc = sleefify_vecf64(taf)(va, vb);                              \
        vecf64_store_multi(vc, out, inc_out);                           \
        x += SIMD_DOUBLE_STRIDE*incx;                                   \
        y += SIMD_DOUBLE_STRIDE;                                        \
        out += SIMD_DOUBLE_STRIDE*inc_out;                              \
      }                                                                 \
    }else if(incx == 1){                                                \
      double* simd_end = x + (n/SIMD_DOUBLE_STRIDE)*SIMD_DOUBLE_STRIDE; \
      while(x != simd_end){                                             \
        va = vecf64_load(x);                                            \
        vb = vecf64_make(y, incy);                                      \
        vc = sleefify_vecf64(taf)(va, vb);                              \
        vecf64_store_multi(vc, out, inc_out);                           \
        x += SIMD_DOUBLE_STRIDE;                                        \
        y += SIMD_DOUBLE_STRIDE*incy;                                   \
        out += SIMD_DOUBLE_STRIDE*inc_out;                              \
      }                                                                 \
    }else{                                                              \
      long i=0;                                                         \
      const long simd_end = (n/SIMD_DOUBLE_STRIDE)*SIMD_DOUBLE_STRIDE;  \
      while(i != simd_end){                                             \
        va = vecf64_make(x, incx);                                      \
        vb = vecf64_make(y, incy);                                      \
        vc = sleefify_vecf64(taf)(va, vb);                              \
        vecf64_store_multi(vc, out, inc_out);                           \
        i += SIMD_DOUBLE_STRIDE;                                        \
        x += SIMD_DOUBLE_STRIDE*incx;                                   \
        y += SIMD_DOUBLE_STRIDE*incy;                                   \
        out += SIMD_DOUBLE_STRIDE*inc_out;                              \
      }                                                                 \
    }                                                                   \
                                                                        \
    while(out!=out_end){                                                \
      out[0] = sleefify_f64(taf)(x[0], y[0]);                           \
      x += incx;                                                        \
      y += incy;                                                        \
      out += inc_out;                                                   \
    }                                                                   \
  };

