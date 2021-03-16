
#define one_arg_fn(name)                                                \
  void DN_s##name##(const long n,                                       \
                    float* x, const long ox, const long incx,           \
                    float* out, const long o_out, const long inc_out);  \
  void DN_d##name##(const long n,                                       \
                    double* x, const long ox, const long incx,          \
                    double* out, const long o_out, const long inc_out);

#define one_arg_fn_int(name)                                            \
  void DN_s##name##(const long n,                                       \
                    float* x, const long ox, const long incx,           \
                    long* out, const long o_out, const long inc_out);   \
  void DN_d##name##(const long n,                                       \
                    double* x, const long ox, const long incx,          \
                    long* out, const long o_out, const long inc_out);


#define two_arg_fn(name)                                                \
  void DN_s##name##(const long n,                                       \
                    float* x, const long ox, const long incx,           \
                    float* y, const long oy, const long incy,           \
                    long* out, const long o_out, const long inc_out);   \
  void DN_d##name##(const long n,                                       \
                    double* x, const long ox, const long incx,          \
                    double* y, const long oy, const long incy,          \
                    long* out, const long o_out, const long inc_out);


/* Example expansion of one_arg_fn(sin):
 * void DN_ssin(const long n,
 *              float* x, const long ox, const long incx,
 *              float* out, const long o_out, const long inc_out);
 * void DN_dsin(const long n,
 *              double* x, const long ox, const long incx,
 *              double* out, const long o_out, const long inc_out);
 */



// These use the u10 functions
one_arg_fn(sin);
one_arg_fn(cos);
// How do we wish to utilize this at the higher level?
// sincos returns both the sin and the cos of a value
/* one_arg_fn(sincos); */
one_arg_fn(tan);

one_arg_fn(asin);
one_arg_fn(acos);
one_arg_fn(atan);

one_arg_fn(sinh);
one_arg_fn(cosh);
one_arg_fn(tanh);
one_arg_fn(asinh);
one_arg_fn(acosh);
one_arg_fn(atanh);

one_arg_fn(log);
one_arg_fn(log10);
one_arg_fn(log2);
one_arg_fn(log1p);

one_arg_fn(exp);
one_arg_fn(exp2);
one_arg_fn(exp10);
one_arg_fn(expm1);

two_arg_fn(pow);
two_arg_fn(atan2);

one_arg_fn(cbrt);

one_arg_fn(erf);
one_arg_fn(erfc); // u15
one_arg_fn(tgamma);
one_arg_fn(lgamma); // log gamma

// These use the u05 functions
one_arg_fn(sinpi);
one_arg_fn(cospi);
one_arg_fn(sincospi);

two_arg_fn(hypot);

one_arg_fn(sqrt);

// No 'u' suffix
one_arg_fn(trunc);
one_arg_fn(floor);
one_arg_fn(ceil);
one_arg_fn(rint); // round | TODO: Check the diff between rint and round
one_arg_fn(fabs);

two_arg_fn(copysign);
two_arg_fn(fmax);
two_arg_fn(fmin);
two_arg_fn(fdim); // positive difference

// TODO: What do we do with nextafter?


// These return an integer
one_arg_fn_int(frfrexp);   // fractional part
one_arg_fn_int(expfrexp);  // integral part
one_arg_fn_int(ilogb);     // integer exponent


// TODO: fused multiply-accumulation three arg functions
