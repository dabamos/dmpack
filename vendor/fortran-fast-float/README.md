# fortran-fast-float

A modern Fortran port of the [Eisel-Lemire fast float parsing algorithm](https://github.com/fastfloat/fast_float), via the C99 single-header port [ffc.h](https://github.com/kolemannix/ffc.h).

Parses ASCII decimal strings into `real32`, `real64`, `int32`, and `int64` values with exact rounding, typically 4-10x faster than `read(str, *)`.

## Getting started

Add to your `fpm.toml`:

```toml
[dependencies]
fortran-fast-float = { git = "https://github.com/perazz/fortran-fast-float" }
```

## Usage

```fortran
use fast_float_module, only: parse_double, parse_float, parse_i64, parse_i32

real(real64) :: d
real(real32) :: f
integer(int64) :: i8
integer(int32) :: i4

! Basic parsing (pure elemental)
d = parse_double("3.14159265358979323")
f = parse_float("-1.25e-3")
i8 = parse_i64("9223372036854775807", base=10)
i4 = parse_i32("FF", base=16)
```

### Error handling

Standard variants return a `parse_result` with position and outcome:

```fortran
use fast_float_module, only: parse_double, parse_result, outcomes

type(parse_result) :: res
real(real64) :: val

val = parse_double("1.0e999", res=res)
if (res%outcome == outcomes%OUT_OF_RANGE) print *, "overflow"
if (res%outcome == outcomes%INVALID_INPUT) print *, "bad input"
```

### Format options

```fortran
use fast_float_module, only: parse_double, parse_options, &
    PRESET_GENERAL, PRESET_JSON, PRESET_FORTRAN

! Fortran D-exponent notation
d = parse_double("1.5D-10", parse_options(format=PRESET_FORTRAN))

! Custom decimal separator
d = parse_double("3,14", parse_options(decimal_point=','))
```

## Building

```bash
fpm build
fpm test
```

## Benchmarks

Run with `fpm test --profile release --target benchmark_compare` on Apple Silicon (M1 Max).
Data files from [simple_fastfloat_benchmark](https://github.com/lemire/simple_fastfloat_benchmark).
Use `./run_benchmarks.sh` to run the full suite (random + data files) with C++ comparison.

Built with `-O3` (no LTO).

### Random uniform [0,1) -- 100k floats

```
ffc                                     :  1615.05 MB/s (+/- 8.5 %)    76.98 Mfloat/s
ffc via fortran interop                 :  1573.96 MB/s (+/- 1.6 %)    75.02 Mfloat/s
fastfloat (C++)                         :  1556.20 MB/s (+/- 1.3 %)    74.17 Mfloat/s
fortran (fast_float line)               :  1335.51 MB/s (+/- 1.5 %)    63.65 Mfloat/s
fortran (fast_float fast)               :  1308.85 MB/s (+/- 0.9 %)    62.38 Mfloat/s
fortran (fast_float array)              :  1071.00 MB/s (+/- 0.5 %)    51.05 Mfloat/s
fortran (fast_float stream)             :  1057.50 MB/s (+/- 0.9 %)    50.40 Mfloat/s
fortran (stdlib to_num)                 :  1039.68 MB/s (+/- 0.9 %)    49.55 Mfloat/s
abseil                                  :   915.51 MB/s (+/- 2.8 %)    43.64 Mfloat/s
strtod                                  :   787.20 MB/s (+/- 2.6 %)    37.52 Mfloat/s
fortran (str2real)                      :   593.69 MB/s (+/- 0.5 %)    28.30 Mfloat/s
netlib                                  :   404.53 MB/s (+/- 3.6 %)    19.28 Mfloat/s
fortran (read *)                        :    56.19 MB/s (+/- 0.4 %)     2.68 Mfloat/s
```

### canada_short.txt -- 111k lines, 0.70 MB

```
ffc via fortran interop                 :   719.73 MB/s (+/- 0.3 %)   133.73 Mfloat/s
ffc                                     :   663.94 MB/s (+/- 0.4 %)   123.36 Mfloat/s
fortran (stdlib to_num)                 :   651.52 MB/s (+/- 1.2 %)   121.05 Mfloat/s
fastfloat (C++)                         :   574.50 MB/s (+/- 0.3 %)   106.74 Mfloat/s
netlib                                  :   545.65 MB/s (+/- 2.1 %)   101.38 Mfloat/s
fortran (fast_float line)               :   535.45 MB/s (+/- 0.1 %)    99.49 Mfloat/s
fortran (fast_float fast)               :   503.87 MB/s (+/- 0.0 %)    93.62 Mfloat/s
fortran (fast_float array)              :   424.48 MB/s (+/- 0.4 %)    78.87 Mfloat/s
fortran (fast_float stream)             :   407.15 MB/s (+/- 0.3 %)    75.65 Mfloat/s
strtod                                  :   396.19 MB/s (+/- 2.3 %)    73.61 Mfloat/s
abseil                                  :   354.24 MB/s (+/- 0.3 %)    65.82 Mfloat/s
fortran (str2real)                      :   231.10 MB/s (+/- 1.1 %)    42.94 Mfloat/s
fortran (read *)                        :    21.65 MB/s (+/- 0.5 %)     4.02 Mfloat/s
```

### canada.txt -- 111k lines, 2.04 MB

```
ffc                                     :  1338.35 MB/s (+/- 2.3 %)    76.91 Mfloat/s
fortran (fast_float line)               :  1186.35 MB/s (+/- 0.3 %)    68.18 Mfloat/s
fortran (fast_float fast)               :  1166.31 MB/s (+/- 0.5 %)    67.02 Mfloat/s
ffc via fortran interop                 :  1110.07 MB/s (+/- 1.5 %)    63.79 Mfloat/s
fastfloat (C++)                         :  1097.76 MB/s (+/- 1.7 %)    63.08 Mfloat/s
fortran (stdlib to_num)                 :  1022.06 MB/s (+/- 1.4 %)    58.73 Mfloat/s
fortran (fast_float array)              :   948.84 MB/s (+/- 0.4 %)    54.53 Mfloat/s
fortran (fast_float stream)             :   934.18 MB/s (+/- 0.5 %)    53.68 Mfloat/s
abseil                                  :   868.45 MB/s (+/- 2.1 %)    49.91 Mfloat/s
strtod                                  :   676.41 MB/s (+/- 0.3 %)    38.87 Mfloat/s
fortran (str2real)                      :   431.74 MB/s (+/- 0.5 %)    24.81 Mfloat/s
netlib                                  :   386.63 MB/s (+/- 1.9 %)    22.22 Mfloat/s
fortran (read *)                        :    49.02 MB/s (+/- 0.5 %)     2.82 Mfloat/s
```

### mesh.txt -- 73k lines, 0.61 MB

```
ffc                                     :   991.77 MB/s (+/- 1.7 %)   135.11 Mfloat/s
ffc via fortran interop                 :   952.06 MB/s (+/- 2.9 %)   129.70 Mfloat/s
fortran (fast_float line)               :   877.26 MB/s (+/- 1.0 %)   119.51 Mfloat/s
fastfloat (C++)                         :   838.88 MB/s (+/- 0.3 %)   114.28 Mfloat/s
fortran (fast_float fast)               :   836.21 MB/s (+/- 0.4 %)   113.91 Mfloat/s
fortran (stdlib to_num)                 :   765.73 MB/s (+/- 2.5 %)   104.31 Mfloat/s
fortran (fast_float array)              :   641.93 MB/s (+/- 0.5 %)    87.45 Mfloat/s
fortran (fast_float stream)             :   592.27 MB/s (+/- 1.0 %)    80.68 Mfloat/s
netlib                                  :   537.69 MB/s (+/- 3.0 %)    73.25 Mfloat/s
strtod                                  :   518.11 MB/s (+/- 0.3 %)    70.58 Mfloat/s
abseil                                  :   407.59 MB/s (+/- 0.5 %)    55.52 Mfloat/s
fortran (str2real)                      :   287.87 MB/s (+/- 1.3 %)    39.22 Mfloat/s
fortran (read *)                        :    27.50 MB/s (+/- 0.5 %)     3.75 Mfloat/s
```

## Acknowledgements

This library is a Fortran translation of the Eisel-Lemire algorithm. Credit goes to:

- **[fast_float](https://github.com/fastfloat/fast_float)** by Daniel Lemire and contributors -- the original C++ implementation. Licensed under Apache-2.0, BSL-1.0, and MIT.
- **[ffc.h](https://github.com/kolemannix/ffc.h)** by Koleman Nix and contributors -- the C99 single-header port used as the direct reference for this Fortran translation. Licensed under Apache-2.0, BSL-1.0, and MIT.

## License

Licensed under your choice of:

- [Apache License, Version 2.0](LICENSE-APACHE)
- [Boost Software License, Version 1.0](LICENSE-BOOST)
- [MIT License](LICENSE-MIT)

matching the upstream projects.
