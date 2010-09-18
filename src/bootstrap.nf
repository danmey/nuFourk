; Define `not'
[ [ true ] [ false ] [ ] ? ] : ~

[ "
" show ] : \n

[ "Number: " show . \n ] : number
[ 0 [ dup number 1 + dup ] [ 10 < ] loop ] $
;[ "" 
; [ "" dup key  ] check

; [ 20 = ~ ] loop ] : token
[ "" [ dup key dup rot append swap ] [ dup 20 = ~ ] loop ] : token
