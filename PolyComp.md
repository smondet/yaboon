PolyComp allows to deal with polymorphic comparisons (mainly remove degrees of freedom)

You can:

  * `open PolyComp.Comparisons`: import many non-polymorphic comparisons (`eqi`, `ltf`, ...)
  * `open PolyComp.CompAndNoPolyPhy`: import the non-polymorphic comparisons and _remove_ polymorphic physical comparisons (`(==)` and `(!=)`)
  * `open PolyComp.CompAndOveridePoly`: import non-polymorphic comparisons and override all polymorphic comparisons (remove physical ones, and define `(=)` for integers, `(=.)` for floats, etc...) and adds some operators (`(=$=)` for strings and `(=@=)` for polymorphic equality)

Please see the [code](http://yaboon.googlecode.com/svn/trunk/PolyComp/PolyComp.ml) for details.

Examples:

With the code
```
open PolyComp.CompAndNoPolyPhy
let _ = 34 == 45
```
the ocaml compiler will raise:
```
This expression has type int but is here used with type
  PolyComp.CompAndNoPolyPhy.shouldnt_be_used
```