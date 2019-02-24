[![Build Status](https://travis-ci.org/commonlispbr/quicksys.svg?branch=master)](https://travis-ci.org/commonlispbr/quicksys)

# Quicksys

## Description

Quicksys is a tool for fetching Common Lisp systems, regardless of the Quicklisp
distribution they are catalogued in. To achieve that, this software is purposely
built as a hardcoded catalog of famous Quicklisp distributions, including:

+ cl-bodge
+ cl21
+ shirakumo
+ ultralisp

You may also add your own distributions at your will.

## Motivation

[Quicklisp](https://www.quicklisp.org/beta/) is an awesome tool for fetching
systems, which you may use on your Common Lisp projects. However, Quicklisp only
fetches systems from its central repository, by default. Adding a new repository
implies manual management of distributions (here called _dists_), in order to
fetch systems that are not in the central repo.

In an effort to speed up the development of software in Common Lisp, we present
Quicksys as a means of unifying some of the most popular distributions. This
way, one can load specific systems, given that one also knows in which
distribution it is; or even, one may also install a distribution on Quicklisp
without ever manually configurating it.

## Requirements

+ A Common Lisp implementation
+ Quicklisp

## How to use

Quicksys exports some symbols which may be easily used. The following
instructions relate to the most important operations.

### Installing a system

Systems may be loaded regardless of whether its dists were installed under
Quicklisp or not.

To install a system *from a dist that was not installed*, use the following:

```lisp
(qs:quickload system-atom :dist dist-name)
```

Where `system-name` is the proper identifier for the desired system, and
`dist-name` is a proper name for a dist -- see "Installing a dist".

You may also use `qs:quickload` in the same way one would use `ql:quickload`.
The effect is the same, for dists that are already installed.

### Installing a dist

One may easily install a dist by using:

```lisp
(qs:install-dist dist-name)
```

Where `dist-name` is a name (symbol or string) for a specific dist.
Available dists may be listed using Quicksys' apropos command:

```lisp
(qs:dist-apropos "")
```

This outputs a list of all available dists currently hardcoded, with their
respective symbols and URLs.

After installing a dist, you may install any system using Quicklisp or Quicksys
normally.

### Removing a dist

Just like installation, one may remove an already installed dist by using a
single command:

```lisp
(qs:uninstall-dist dist-name)
```

## License

This project is distributed under the MIT License.
