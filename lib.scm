(use gauche.lazy)
(use gauche.sequence)
(use util.match)

; 丸め
(define-inline flint floor->exact)
(define-inline ceint ceil->exact)

; キャスト
(define-inline int x->integer)

; bit ビット
(define-inline << ash)
(define-inline & logand)
(define-inline lor logior)
(define-inline xor logxor)

; heap ヒープ 優先度付きキュー
(use data.heap)
(define-inline mh make-binary-heap)
(define-inline hpush! binary-heap-push!)
(define-inline hpop-min! binary-heap-pop-min!)
(define-inline hpop-max! binary-heap-pop-max!)
(define-inline heapify build-binary-heap)

; ring-buffer リングバッファ Deque deque
(use data.ring-buffer)
(define-inline mdq make-ring-buffer)
(define-inline add-front! ring-buffer-add-front!)
(define-inline add-back! ring-buffer-add-back!)
(define-inline remove-front! ring-buffer-remove-front!)
(define-inline remove-back! ring-buffer-remove-back!)

; Counter
(define (counter seq :optional (comp 'equal?))
  (fold
   (lambda (x col)
     (hash-table-set! col x (+ 1 (hash-table-get col x 0)))
     col)
   (make-hash-table comp)
   seq))

; メモ化
(define (memoize f)
  (let ([cache (make-hash-table equal-comparator)])
    (lambda x
      (or (hash-table-get cache x #f)
          (let ([result (apply f x)])
            (hash-table-put! cache x result)
            result)))))
(define-syntax define-memo
  ; パターンマッチ
  (syntax-rules ()
    ((_ f expr ...)
     (define f (memoize
       (match-lambda* expr ...))))))
; (define-memo fib
;   [(0) 0]
;   [(1) 1]
;   [(n) (+ (fib (- n 1)) (fib (- n 2)))])

; 整数論
; gcd, lcm
; 素数
(use math.prime)
; prime
;   (take *primes* n)
;   naive-factorize, small-prime?(341,550,071,728,321まで)

; 約数
(define (ldivisors n)
  (lconcatenate
   (lfilter-map
    (lambda (i)
      (cond
        [(= n (* i i)) (list i)]
        [(zero? (remainder n i)) (list i (exact (/ n i)))]
        [else #f]))
    (ltake-while
     (lambda (i) (>= n (* i i)))
     (lrange 1)))))

(define (divisors n :optional (sort? #t))
  (let ([divs (ldivisors n)])
    (if sort? (sort divs) (apply list divs))))

; 組合せ論
(use util.combinations)
; permutaions combinations power-set cartesian-product(直積)
