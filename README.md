# fast-math

### _Jeremiah LaRocco <jeremiah_larocco@fastmail.com>_

This package provides (or will eventually provide) fast math routines using SIMD
instructions.  SBCL will be the supported CL dialect, using :sb-simd. I would
like to add support for CCL, but don't know anything about SIMD support on that
compiler.

Initially I want to focus on 2, 3, and 4 dimensional vectors and square matrices
of single and double floats.  For these operations I'll probably try to keep the
API more-or-less compatible with the :3d-vectors and :3d-matrices packages.

Eventually I may add support for doulble and float complex types, and other
operations like fast fourier transform (FFT).

## License

ISC

Copyright (c) 2022 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>


