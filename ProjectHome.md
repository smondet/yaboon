# Yaboon, Yet Another Bunch Of Ocaml Modules #

## Presentation ##

Here is a set of [Objective Caml](http://www.ocaml.org) modules that do not deserve a complete project. They are all MIT-licensed and any ocaml developer is welcome to join with his code (just contact one of the project owners).

## Modules ##

### SimpleTestParser ###

SimpleTestParser is a syntax extension providing an minimalistic unit-testing framework (see [test\_TestParser.ml](http://yaboon.googlecode.com/svn/trunk/SimpleTestParser/test_TestParser.ml) for usage).

### IList ###

IList ([doc](http://yaboon.googlecode.com/svn/trunk/IList/doc/index.html)) implements an imperative circular singly-linked list with many _O(1)_ operations (`length`, `add_head`, `add_last`, `see_head`, `see_last`, `take_head`, `append`...).

### BitBuffer ###

BitBuffer ([doc](http://yaboon.googlecode.com/svn/trunk/BitBuffer/doc/index.html)) is a buffer accessible and mutable with "bit" precision.

### Unsafix ###


Unsafix ([doc](http://yaboon.googlecode.com/svn/trunk/Unsafix/doc/index.html)) provides unsafe and non-portable features of Unix to OCaml (Linux or BSD's socket options, etc...).

### PolyComp ###

PolyComp removes or overrides polymorphic comparisons (from the module Pervasives of the stdlib).

### Timeout ###

Timeout ([doc](http://yaboon.googlecode.com/svn/trunk/Timeout/doc/index.html)) manages
timeouts using the Unix.ITIMER\_REAL timer.


