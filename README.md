# dense-numericals - High Performance number crunching over dense-arrays

Features / Currently Supported:

- Uses [dense-arrays](https://github.com/digikar99/dense-arrays) as the frontend
- Uses [cl-bmas](https://github.com/digikar99/cl-bmas) and [cl-cblas](https://github.com/digikar99/cl-cblas) as the backend. 
  - Currently supports single-float and double-float operations. 
  - Optimized using SIMD on AVX512 (untested), AVX2, SSE2 and NEON
  - and multithreading with [lparallel](https://lparallel.org/)
- Achieves performance comparable to numpy and torch with support for broadcasting: see [benchmarks](./benchmarks/).
- AOT compilation with 
  (i)   Helpful compiler notes for optimization
  (ii)  Parametric Polymorphism support from [polymorphic-functions](https://github.com/digikar99/polymorphic-functions/)
  (iii) Compile time form type inferencing using [cl-form-types](https://github.com/alex-gutev/cl-form-types/)
- Supported functions so far (see [src/package.lisp](src/package.lisp)):
  - DMAS: `+ - * /`
  - Comparisons: `< <= = /= >= >`
  - Trigonometric: `sin cos tan asin acos atan`
  - Hyperbolic `sinh cosh tanh asinh acosh atanh`
  - `log exp expt`
  - `ffloor fceiling fround ftruncate abs`
  - `copy coerce concat two-arg-matmul dot`
  - single-axis / all-axes `sum`
  
Limitations / Future Plans:

- Recheck and improve CUDA support using [cl-cuda](https://github.com/takagi/cl-cuda)
- Improve support for `sum` over multiple axes
- Add more functionality from CBLAS
- A bug on [CCL](https://github.com/Clozure/ccl/pull/369) wouldn't let adhoc-polymorphic-functions work as correctly; the patch is available though. 

### Rationale

- No matrix/array lisp library I know of provides support for SIMD optimized non-arithmetic math functions; prominent ones include femlisp-matlisp and numcl; it might be possible with numcl though
- I'm not a fan of JIT for prototyping purposes - therefore neither julia nor numcl
- The intention is not to redo the whole scientific programming frameworks in Common Lisp - but to use dense-arrays and dense-numericals in conjunction with projects like py4cl/2

# Installation

Follow the instructions [here](https://github.com/digikar99/adhoc-polymorphic-functions/#getting-it-from-ultralisp).

# Examples

- [mnist](examples/mnist.lisp)

# Tests

Test are scattered throughout the system. Once `(ql:quickload "dense-numericals")` is complete, run `(asdf:test-system "dense-numericals")`.
