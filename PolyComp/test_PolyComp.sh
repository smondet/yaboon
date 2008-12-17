#! /bin/sh

TSTF=/tmp/poly_comp_test.ml

assert_compilation ()
{
    ocamlc PolyComp.ml $2  > /tmp/ocamlc.log 2>&1
    if [ $? -ne $3 ]; then
        echo "=== Test \"$1\" failed:"
        cat /tmp/ocamlc.log
    fi;
}

cat > $TSTF << EOF
open PolyComp.Comparisons;;

assert (1 <> 3 && 1 < 3 && [] == [] && "ETT" >= "ABBDF");;
let e = 45.2;;
if (eqi 4 4) && (eqf 5.2 e) then ();;
EOF
assert_compilation "open comp, success" $TSTF 0


cat > $TSTF << EOF
open PolyComp.Comparisons;;

assert (1 <> 3 && 1 < 3 && [] == [] && "ETT" >= "ABBDF");;
let e = 45.2;;
if (eqi 4 4) && (eqf 5 e) then ();;
EOF
assert_compilation "open comp, failure" $TSTF 2

cat > $TSTF << EOF
open PolyComp.CompAndNoPolyPhy;;

assert (1 <> 3 && 1 < 3 && [] = [] && "ETT" >= "ABBDF");;
let e = 45.2;;
if (eqi 4 4) && (eqf 5.2 e) then ();;
EOF
assert_compilation "open comp and no poly phy, success" $TSTF 0


cat > $TSTF << EOF
open PolyComp.CompAndNoPolyPhy;;

assert (1 <> 3 && 1 < 3 && [] == [] && "ETT" >= "ABBDF");;
let e = 45.2;;
if (eqi 4 4) && (eqf 5 e) then ();;
EOF
assert_compilation "open comp and no poly phy, failure" $TSTF 2

cat > $TSTF << EOF
open PolyComp.CompAndOveridePoly;;

1 <> 3 &&
1. <. 3. &&
[] =@= [] &&
"ETT" =$= "ABBDF";;

let e = 45.2 in (eqi 4 4) && (eqf 5.2 e);;
EOF
assert_compilation "open comp and no poly, success" $TSTF 0


cat > $TSTF << EOF
open PolyComp.CompAndOveridePoly;;

assert (1 <> 3 && 1 < 3 && [] = [] && "ETT" =@ "ABBDF");;
let e = 45.2;;
if (eqi 4 4) && (eqf 5 e) then ();;
EOF
assert_compilation "open comp and no poly, failure" $TSTF 2


