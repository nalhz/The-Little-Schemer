#lang racket

;Preface IX define function: [atom? x]
[define atom?
  [lambda [x]
    [and [not [pair? x]] [not [null? x]]]]]

;P16 define function: [lat? l]
[define lat?
  [lambda [l]
    [cond
      [[null? l] #t]
      [[atom? [car l]][lat? [cdr l]]]
      [else #f]]]]

;P22 define function: [member? a lat]
[define member?
  [lambda [a lat]
    [cond
      [[null? lat] #f]
      [else [or [eq? [car lat] a]
                [member? a [cdr lat]]]]]]]

;P34 define function: [rember a lat]
[define rember
  [lambda [a lat]
    [cond
      [[null? lat] '[]]
      [[eq? [car lat] a] [cdr lat]]
      [else [cons [car lat] [rember a [cdr lat]]]]]]]


;P43 define function: [first l]
[define first
  [lambda [l]
    [cond
      [[null? l] '[]]
      [else [cons [car [car l]] [first [cdr l]]]]]]]
; [first [list '[a b] '[b c] '[c d]]]

;P47 define function: [insertR new old lat]
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

;P51 define function: [insertL new old lat]
[define insertL
  [lambda [new old lat]
    [cond
      [[null? lat] '[]]
      [[eq? [car lat] old] [cons new lat]]
      [else [cons [car lat] [insertL new old [cdr lat]]]]]]]

;P51 define function: [subst new old lat]
[define subst
  [lambda [new old lat]
    [cond
      [[null? lat] '[]]
      [[eq? [car lat] old] [cons new [cdr lat]]]
      [else [cons [car lat] [subst new old [cdr lat]]]]]]]

;P52 define function: [subst2 new o1 o2 lat]
[define subst2
  [lambda [new o1 o2 lat]
    [cond
      [[null? lat] '[]]
      [[or [eq? [car lat] o1] [eq? [car lat] o2]] [cons new [cdr lat]]]
      [else [cons [car lat] [subst2 new o1 o2 [cdr lat]]]]]]]


;P53 define function: [multrember a lat]
[define multirember
  [lambda [a lat]
    [cond
      [[null? lat] '[]]
      [[eq? a [car lat]] [multirember a [cdr lat]]]
      [else [cons [car lat] [multirember a [cdr lat]]]]]]]

;P56 define function: [multiinsertR new old lat]
[define multiinsertR
  [lambda [new old lat]
    [cond
      [[null? lat] '[]]
      [[eq? old [car lat]] [cons [cons old new] [multiinsertR new old [cdr lat]]]]
      [else [cons [car lat] [multiinsertR new old [cdr lat]]]]]]]


;P57 define function: [multiinsertL new old lat]
[define multiinsertL
  [lambda [new old lat]
    [cond
      [[null? lat] '[]]
      [[eq? old [car lat]] [cons [cons new old] [multiinsertL new old [cdr lat]]]]
      [else [cons [car lat] [multiinsertL new old [cdr lat]]]]]]]

;P57 define function: [multisubst new old lat]
[define multisubst
  [lambda [new old lat]
    [cond
      [[null? lat] '[]]
      [[eq? old [car lat]] [cons new [multisubst new old [cdr lat]]]]
      [else [cons [car lat] [multisubst new old [cdr lat]]]]]]]



