; Define `not'
[ [ true ] [ false ] [ ] ? ] : ~

[ "
" show ] : \n

[ "Number: " show . \n ] : number
[ 0 [ dup number 1 + dup ] [ 10 < ] loop ] $
; [ "" [ dup key dup rot swap append ] [ dup 20 = ~ ] loop ] : token
