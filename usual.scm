(use gauche.lazy)
(use gauche.sequence)
(use util.match)

; -------------------------------------------
; ライブラリ、エイリアス
; -------------------------------------------

; lambda -> ^
; lambda (x) -> ^x
; setter
(define-inline ! set!)
; 「~」 万能アクセサ (~ v i) (! (~ v i) 0)
(define-syntax ~!
 ; (set! (~ v 1) 2) -> (~! v 1 : 2)
 (syntax-rules (:)
   ((_ obj arg ... : val)
    (! (~ obj arg ...) val))))
; vector
(define-inline mv make-vector)
(define-inline vs! vector-set!)
(define-method object-apply ((vec <vector>) (i <integer>) :optional fallback)
  ; (vector-ref v i) -> (v i)
  (vector-ref vec i fallback))
; hash-table
(define-inline mt make-hash-table)
(define-inline ts! hash-table-set!)
(define-method object-apply ((table <hash-table>) key :optional default)
  ; (hash-table-get t k d) -> (t k d)
  (hash-table-get table key default))
; lib
(define-inline (++ x) (+ x 1))
(define-inline (-- x) (- x 1))
(define-inline % remainder)
; lazy-partial
(define (lmap$ f) (pa$ lmap f))
(define (lfilter$ f) (pa$ lfilter f))
; functional
;   partinal: pa$ map$ filter$ any$ every$
;   compose: .$
; lazy
;   (lrange start end)
;   lmap lfilter
; pattern match function
(define-syntax defn
  (syntax-rules ()
    ((_ f expr ...)
     (define f 
       (match-lambda* expr ...)))))
; (defn fac
;   [(1) 1]
;   [(n) (* n (fac (-- n)))])
; input
(define (my-read)
  (let ([a (read)])
    (if (symbol? a)
      (symbol->string a)
      a)))
(define (my-read-line)
  (let ([line (read-line)])
    (if (string=? "" line)
        (my-read-line)
        line)))
; input macro
(define-syntax solve
  (syntax-rules (: list vec grid)
    ((_ [var : list n])
     (define var
       (let loop ([i n])
         (if (zero? i) () (cons (my-read) (loop (- i 1)))))))
    ((_ [var : vec n])
     (define var
       (let ([v (make-vector n)])
         (dotimes (i n)
           (vector-set! v i (my-read)))
         v)))
    ((_ [var : vec n m])
     (define var
       (let ([v (make-vector m)])
         (dotimes (i m)
           (solve [u : vec n])
           (vector-set! v i u))
         v)))
    ((_ [var : grid h])
     (define var
       (let ([v (make-vector h)])
         (dotimes (i h)
           (let ([row (list->vector (string->list (my-read-line)))])
           (vector-set! v i row)))
         v)))
    ((_ (expr ...))
     (expr ...))
    ((_ var)
     (define var
       (my-read)))
    ((_ expr1 expr2 ...)
     (begin
       (solve expr1)
       (solve expr2 ...)))))
(solve
  x y s
  [l : list x]
  [v : vec x]
  [m : vec x y]
  [g : grid y]
  (print (list x y s l v m g)))
; 入力
; 2 3 aaa
; 6 6
; 7 7
; 8 8
; 9 9
; 10 bb
; #.###
; #...#
; ###.#

; 出力
; (2 3 aaa (6 6) #(7 7) #(#(8 8) #(9 9) #(10 bb)) #(#(# . # # #) #(# . . . #) #(# # # . #)))
; -------------------------------------------

