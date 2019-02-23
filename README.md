[![Build Status](https://travis-ci.org/commonlispbr/quicklisp-meta.svg?branch=master)](https://travis-ci.org/commonlispbr/quicklisp-meta)

# quicklisp-meta

## Description

A Quicklisp meta distribution tool to track multiple quicklisp
distributions over world in a unique place. This software should build
a catalog of common distributions over world like:

+ cl-bodge
+ cl21
+ shinmera

That way you can use to fetch first the distribution and so on the
proper dependencies from it automatically. Today there is no automatic
tool for softwares that are not in quicklisp xach central repository,
so you need make a manual intervation to add different distributions.

## Requirements

+ SBCL (for now)
+ Quicklisp

## How to use

Suppose we want to use the nice shinmera softwares, so what you need
is to subscribe to shinmera own distribution list.

``` lisp
(ql-meta:subscribe 'shinmera)
```

That will install the distribution based on quicklisp-client. So you
can load from `ql:quickload` any related shinmera software.



## License

MIT
