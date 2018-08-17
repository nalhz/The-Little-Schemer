#lang racket

;Preface IX  [atom? x]
[define atom?
  [lambda [x]
    [and [not [pair? x]] [not [null? x]]]]]

;P16  [lat? l]
[define lat?
  [lambda [l]
    [cond
      [[null? l] #t]
      [[atom? [car l]][lat? [cdr l]]]
      [else #f]]]]

;P22  [member? a lat]
[define member?
  [lambda [a lat]
    [cond
      [[null? lat] #f]
      [else [or [eq? [car lat] a]
                [member? a [cdr lat]]]]]]]

;P34  [rember a lat]
[define rember
  [lambda [a lat]
    [cond
      [[null? lat] '[]]
      [[eq? [car lat] a] [cdr lat]]
      [else [cons [car lat] [rember a [cdr lat]]]]]]]


;P43  [first l]
[define first
  [lambda [l]
    [cond
      [[null? l] '[]]
      [else [cons [car [car l]] [first [cdr l]]]]]]]
; [first [list '[a b] '[b c] '[c d]]]

;P47  [insertR new old lat]
[define insertR
  [lambda [new old lat]
    [cond
      [[null? lat] '[]]
      [else [cond
              [[eq? [car lat] old]
               [cons old [cons new [cdr lat]]]]
              [else [cons [car lat]
                          [insertR new old [cdr lat]]]]]]]]]
; other edition
[define insertx
  [lambda [new old lat]
    [cond
      [[null? lat] '[]]
      [[eq? [car lat] old] [cons old [cons new [cdr lat]]]]
      [else [cons [car lat] [insertx new old [cdr lat]]]]]]]

;P51  [insertL new old lat]
[define insertL
  [lambda [new old lat]
    [cond
      [[null? lat] '[]]
      [[eq? [car lat] old] [cons new lat]]
      [else [cons [car lat] [insertL new old [cdr lat]]]]]]]

;P51  [subst new old lat]
[define subst
  [lambda [new old lat]
    [cond
      [[null? lat] '[]]
      [[eq? [car lat] old] [cons new [cdr lat]]]
      [else [cons [car lat] [subst new old [cdr lat]]]]]]]

;P52  [subst2 new o1 o2 lat]
[define subst2
  [lambda [new o1 o2 lat]
    [cond
      [[null? lat] '[]]
      [[or [eq? [car lat] o1] [eq? [car lat] o2]] [cons new [cdr lat]]]
      [else [cons [car lat] [subst2 new o1 o2 [cdr lat]]]]]]]


;P53  [multrember a lat]
[define multirember
  [lambda [a lat]
    [cond
      [[null? lat] '[]]
      [[eq? a [car lat]] [multirember a [cdr lat]]]
      [else [cons [car lat] [multirember a [cdr lat]]]]]]]

;P56  [multiinsertR new old lat]
[define multiinsertR
  [lambda [new old lat]
    [cond
      [[null? lat] '[]]
      [[eq? old [car lat]] [cons [cons old new] [multiinsertR new old [cdr lat]]]]
      [else [cons [car lat] [multiinsertR new old [cdr lat]]]]]]]


;P57  [multiinsertL new old lat]
[define multiinsertL
  [lambda [new old lat]
    [cond
      [[null? lat] '[]]
      [[eq? old [car lat]] [cons [cons new old] [multiinsertL new old [cdr lat]]]]
      [else [cons [car lat] [multiinsertL new old [cdr lat]]]]]]]

;P57  [multisubst new old lat]
[define multisubst
  [lambda [new old lat]
    [cond
      [[null? lat] '[]]
      [[eq? old [car lat]] [cons new [multisubst new old [cdr lat]]]]
      [else [cons [car lat] [multisubst new old [cdr lat]]]]]]]

;P60  [o+ n m]
[define o+
  [lambda [n m]
    [cond
      [[zero? m] n]
      [else [o+ [add1 n] [sub1 m]]]]]]

;P61  [o- n m]
[define o-
  [lambda [n m]
    [if [zero? m] n [o- [sub1 n] [sub1 m]]]]]


;P64  [addtup tup]
[define addtup
  [lambda [tup]
    [if [null? tup] 0 [o+ [car tup] [addtup [cdr tup]]]]]]

;P65 [times n m]
[define times
  [lambda [n m]
    [if [zero? m] 0 [o+ n [times n [sub1 m]]]]]]

;P67 [tup+ tup1 tup2]
[define tup+
  [lambda [tup1 tup2]
    [cond
      [[null? tup1] tup2]
      [[null? tup2] tup1]
      [else [cons [o+ [car tup1] [car tup2]] [tup+ [cdr tup1] [cdr tup2]]]]]]]

;P71 [bigger n m]
[define >>
  [lambda [n m]
    [cond
      [[zero? n] #f]
      [[zero? m] #t]
      [else [>> [sub1 n] [sub1 m]]]]]]
;P73 [smaller n m]
[define <<
  [lambda [n m]
    [cond
      [[zero? m] #f]
      [[zero? n] #t]
      [else [<< [sub1 n] [sub1 m]]]]]]

;P74 [:= n m]
[define :=
  [lambda [n m]
    [cond
      [[>> n m] #f]
      [[<< n m] #f]
      [else #t]]]]

;P74 [^ n m]
[define ^
  [lambda [n m]
    [cond
      [[zero? m] 1]
      [else [times n [^ n [sub1 m]]]]]]]


;P75 [// n m]
[define //
  [lambda [n m]
    [cond
      [[<< n m] 0]
      [else [add1 [// [o- n m] m]]]]]]


;P76 [len lat]
[define len
  [lambda [lat]
    [cond
      [[null? lat] 0]
      [else [add1 [len [cdr lat]]]]]]]

;P76 [pick n lat]
[define pick
  [lambda [n lat]
    [cond
      [[zero? [sub1 n]] [car lat]]
      [else [pick [sub1 n][cdr lat]]]]]]

;P77 [rempick n lat]
[define rempick
  [lambda [n lat]
    [cond
      [[zero? [sub1 n]] [cdr lat]]
      [else [cons [car lat] [rempick [sub1 n][cdr lat]]]]]]]

;P77 [no-nums lat]
[define no-nums
  [lambda [lat]
    [cond
      [[null? lat] '[]]
      [[number? [car lat]] [no-nums [cdr lat]]]
      [else [cons [car lat] [no-nums [cdr lat]]]]]]]

;P78 [all-nums lat]
[define all-nums
  [lambda [lat]
    [cond
      [[null? lat] '[]]
      [[number? [car lat]] [cons [car lat] [all-nums [cdr lat]]]]
      [else [all-nums [cdr lat]]]]]]

;P78 [eqan? a1 a2]
[define eqan?
  [lambda [a1 a2]
    [cond
      [[and [number? a1] [number? a2]] [= a1 a2]]
      [[or [number? a1] [number? a2]] #f]
      [else [eq? a1 a2]]]]]

;P78 [occur a lat]
[define occur
  [lambda [a lat]
    [cond
      [[null? lat] 0]
      [[eq? a [car lat]] [add1 [occur a [cdr lat]]]]
      [else [occur a [cdr lat]]]]]]

;P79 [one? n]
[define one?
  [lambda [n]
    [= n 1]]]

;P79 [rempic2 n lat]
[define rempick2
  [lambda [n lat]
    [cond
      [[one? n] [cdr lat]]
      [else [cons [car lat] [rempick2 [sub1 n][cdr lat]]]]]]]

;P81 [rember* a l]
[define rember*
  [lambda [a l]
    [cond
      [[null? l] '[]]
      [[atom? [car l]]
       [cond
         [[eq? [car l] a] [rember* a [cdr l]]]
         [else [cons [car l] [rember* a [cdr l]]]]]]
      [else [cons [rember* a [car l]] [rember* a [cdr l]]]]]]]

;P82 [insertR* new old l]
[define [insertR* new old l]
  [cond
    [[null? l] '[]]
    [[atom? [car l]]
     [cond
       [[eq? [car l] old] [cons [cons old new] [insertR* new old [cdr l]]]]
       [else [cons [car l] [insertR* new old [cdr l]]]]]]
    [else
     [cons [insertR* new old [car l]] [insertR* new old [cdr l]]]]]]


;P84 [occur* a l]
[define [occur* a l]
  [cond
    [[null? l] 0]
    [[atom? [car l]]
     [cond
       [[eq? [car l] a] [add1 [occur* a [cdr l]]]]
       [else [occur* a [cdr l]]]]]
    [else [+ [occur* a [car l]] [occur* a [cdr l]]]]]]

;P85 [subst* new old l]
[define [subst* new old l]
  [cond
    [[null? l] '[]]
    [[atom? [car l]]
     [cond
       [[eq? [car l] old][cons new [subst* new old [cdr l]]]]
       [else [cons [car l] [subst* new old [cdr l]]]]]]
    [else [cons [subst* new old [car l]] [subst* new old [cdr l]]]]]]

;P86 [insert* new old l]
[define [insertL* new old l]
  [cond
    [[null? l] '[]]
    [[atom? [car l]]
     [cond
       [[eq? [car l] old] [cons new [cons old [insertL* new old [cdr l]]]]]
       [else [cons [car l] [insertL* new old [cdr l]]]]]]
    [else [cons [insertL* new old [car l]] [insertL* new old [cdr l]]]]]]

    
;P86 [member* a l]
[define [member* a l]
  [cond
    [[null? l] #f]
    [[atom? [car l]]
     [cond
       [[eq? [car l] a] #t]
       [else [member* a [cdr l]]]]]
    [else [or [member* a [car l]] [member* a [cdr l]]]]]]

;P87 [leftmost