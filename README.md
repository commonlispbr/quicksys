[![Build Status](https://travis-ci.org/commonlispbr/quicklisp-meta.svg?branch=master)](https://travis-ci.org/commonlispbr/quicklisp-meta)

# quicklisp-meta

## Description

A Quicklisp meta distribution tool to track multiple quicklisp
distributions over world in a unique place. This software should build
a catalog of common distributions over world like:

+ cl-bodge
+ cl21
+ shirakumo
+ ultralisp

That way you can use to fetch first the distribution and so on the
proper dependencies from it automatically. Today there is no automatic
tool for softwares that are not in quicklisp xach central repository,
so you need make a manual intervation to add different distributions.

## Requirements

+ Common Lisp implementation
+ Quicklisp

## How to use

Suppose we want to use the nice shirakumo softwares, so what you need
is to subscribe to shirakumo own distribution list.

``` lisp
(ql-meta:subscribe 'shirakumo)
```

That will install the distribution based on quicklisp-client. So you
can load from `ql:quickload` any related shirakumo software.



## License

MIT
