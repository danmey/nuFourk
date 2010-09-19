; Define `not'
[ [ true ] [ false ] ? ] : ~

; Not `equal'
[ = ~ ] : <>

[ < ~ ] : >

; Next line
[ "
" show ] : \n

[ "Number: " show . \n ] : number

[ 0 [ dup number ] [ 1 + dup 10 < ] loop ] check

[ "" [ append ] [ key dup 32 <> ] loop drop ] : token


[ swap dup rot dup rot swap ] : 2dup
[ rot drop ] : nip2

[ 1 swap [ 2dup * nip2 swap ] [ 1 - dup 1 < ~ ] loop drop ] : factorial

[ swap [ 2dup rot $ nip2 swap ] swap loop ] : binrec

[ 1 swap [ * ] [ 1 - dup 1 < ~ ] binrec drop ] : fact2
