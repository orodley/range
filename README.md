This package provides Haskell-style ranges in Common Lisp, using a reader macro.
It performs fairly robust checking on the range to make sure it's well formed.
The only difference (as far as I know) between this macro and Haskell-style
ranges is that it won't construct infinite lists, e.g. `[1..]`

For example:

```common-lisp
CL-USER> (asdf:load-system :range)
[snip]
T
CL-USER> (in-package :range)
#<PACKAGE "RANGE">
RANGE> (in-readtable range)
[snip]
RANGE> #[1..10]
(1 2 3 4 5 6 7 8 9 10)
RANGE> #[56,76..200]
(56 76 96 116 136 156 176 196)
RANGE> #[100,50..0]
(100 50 0)
RANGE> #[0.1,0.2..1] ; Note the inaccuracy produced by float ranges
(0.1 0.2 0.3 0.4 0.5 0.6 0.70000005 0.8000001 0.9000001)
RANGE> #[-2.4, 2.6..7.9]
(-2.4 2.6 7.6)
RANGE> #[-2.4, -2.6..7.9]
NIL
RANGE>
```
