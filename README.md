# Scheme(Gauche)でAtCoderの問題を解くときのライブラリやエイリアスまとめ

定数倍程度のオーバーヘッドは気にせず、書きやすさを重視しています。

## 入力

入力として使える`read`は、標準入力から**S式を構成する最後の文字までを読みます(※)**(https://practical-scheme.net/gauche/man/gauche-refj/Ru-Chu-Li-.html#g_t_5165_529b)

---
(※ readによる入力は、以下のような動きもできるということです)

```
gosh> (eval (read) (interaction-environment))
(+ 1 2)
3
```
---

カッコで閉じられていない入力に対しては型を以下のように解釈するみたいです。

|入力|型|
|---|---|
|整数|`<integer>`|
|小数点数|`<real>`|
|文字列|`<symbol>`|

文字列の入力はsymbolとなるので、解くときはstringに変換したほうが扱いやすいです。

また、`###`のような入力については、`#`がサポートされていない記号になるので`read-line`で文字列を取得する必要があります。

また、`read-line`は改行まで文字列を改行抜きで読み込みますが、「`read`からの`read-line`」のような流れの入力になると、readで改行が取り残された結果空文字だけが読まれる場合があります。

```scheme
; 入力
; 1 3
; ###

(let ([x (read)]
      [y (read)]
      [g (read-line)])
  (print (list x y g)))
; 1 3 (空文字)
```

以上のことを考慮した入力の関数が以下です。

```scheme
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
```

入力を楽にするマクロを作りました。

```scheme
(solve
  x y s          ; x y s それぞれを読み込んで代入
  [l : list x]   ; x 個の入力をリストとして
  [v : vec x]    ; x 個の入力をベクターとして
  [m : vec x y]  ; y 個の x要素ベクター (リストバージョンは用意してません)
  [g : grid y]   ; 「#」が壁、 「.」が道になるような入力
  (print (list x y s l v m g))) ; S式は普通に評価する。


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
```

```scheme
(define-syntax solve
  (syntax-rules (: list vec grid)
    ((_ [var : list n])
     (define var
       (map (^ (_) (my-read)) (lrange 0 n))))
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
```

## 出力

`display`がありますが、gaucheでは改行をしてくれる`print`もあります。

```scheme
(display ans) ; 改行なし
(print ans)   ; 改行あり
```

## リファレンス

特記すべき組み込みライブラリや標準ライブラリです。

### 整数

https://practical-scheme.net/gauche/man/gauche-refj/Shu-Zhi-.html

* gcd
* lcm

### リスト

https://practical-scheme.net/gauche/man/gauche-refj/peatorisuto.html

* delete-duplicates

### シーケンスに関するライブラリ

https://practical-scheme.net/gauche/man/gauche-refj/sikensuhuremuwaku.html
https://practical-scheme.net/gauche/man/gauche-refj/korekusiyonhuremuwaku.html

```scheme
(use gauche.sequence)
```

ベクターやリストなどのシーケンスに対してジェネリックに使えるライブラリです。

* map
* filter
* fold
* 部分適用
  * map$
  * fold$
* map-with-index
* など

### ソート

https://practical-scheme.net/gauche/man/gauche-refj/sototomazi.html

* sort

> `リスト、ベクタ、文字列だけでなく、あらゆるシーケンス (<sequence>のインスタンス)をソートできます。`

### 遅延シーケンス

遅延評価 - https://practical-scheme.net/gauche/man/gauche-refj/Chi-Yan-Ping-Jia-.html#g_t_9045_5ef6_30b7_30fc_30b1_30f3_30b9

* lrange (範囲)

遅延シーケンスユーティリティ - https://practical-scheme.net/gauche/man/gauche-refj/Chi-Yan-sikensuyuteiritei.html#g_t_9045_5ef6_30b7_30fc_30b1_30f3_30b9_30e6_30fc_30c6_30a3_30ea_30c6_30a3

R7RS遅延シーケンス - https://practical-scheme.net/gauche/man/gauche-refj/R7RS-large.html#R7RS_9045_5ef6_30b7_30fc_30b1_30f3_30b9

```scheme
(use gauche.lazy)
```

* lmap 
* lfilter 
* lfilter-map
* lconcatenate
* ltake
* ltake-while
* lslices

```scheme
(use scheme.lseq)
```

* lseq-zip

## エイリアス

エイリアス(自作含む)を紹介します。

### lambda

```scheme
(lambda (x) (+ x 1))
(^ (x) (+ x 1))
; 変数名が一文字であれば以下のように書ける
(^x (+ x 1))
```

### アクセサ

万能アクセサ(`~`)です。
後述する自作エイリアスをよく使うのでこちらはあまり使わないですが便利です。

```scheme
(vector-ref! v 1)
(vector-set! v 1 3)
(hash-table-get! t "a")
(hash-table-set! t "a" 3)
; それぞれ以下のように書ける
(~ v 1)
(set! (~ v 1) 3)
(~ t "a")
(set! (~ t "a") 3)
```

#### vector, hash-table

`make-vector`や`vector-set!`などは長いので短くしました。
ClojureやScalaのベクターやハッシュマップは関数として振舞うので、そのような書き方ができると記述がスッキリします。

```scheme
; vector
(define-inline mv make-vector)
(define-inline vs! vector-set!)
(define-method object-apply ((vec <vector>) (i <integer>) :optional fallback)
  (vector-ref vec i fallback))

(vector-ref v i)
; は次のように書ける
(v i)

; hash-table
(define-inline mt make-hash-table)
(define-inline ts! hash-table-set!)
(define-method object-apply ((table <hash-table>) key :optional default)
  (hash-table-get table key default))

(hash-table-get t k d)
; は次のように書ける
(t k d)
```

(実用上の注意: https://practical-scheme.net/gauche/man/gauche-refj/Shou-Sok-kitoJi-Sok-.html#g_t_9069_7528_53ef_80fd_306a_30aa_30d6_30b8_30a7_30af_30c8)

### よく使う関数

#### 目次
##### インクリメント
##### デクリメント
##### 剰余

```scheme
(define-inline (++ x) (+ x 1))
(define-inline (-- x) (- x 1))
(define-inline % remainder)
```

### たまに使う関数

#### 目次
##### ビット
##### 丸め
##### キャスト

```scheme
; bit
(define-inline << ash)
(define-inline & logand)
(define-inline lor logior)
(define-inline xor logxor)

; 丸め
; floorだけだと小数点数のままなので (exact (floor x)) が必要。
; この糖衣構文 floor->exactがある。 ceilも同様。
; それをさらに短く
(define-inline flint floor->exact)
(define-inline ceint ceil->exact)

; キャスト
; number->string
; x->integer
; string->number
(define-inline int x->integer)
```

### パターンマッチ

https://practical-scheme.net/gauche/man/gauche-refj/patanmatutingu.html

パターンマッチは便利ですが、SchemeはHaskellやF#などと比べると少し記述がめんどくさいです。

```scheme
(use util.match)

(define factorial
  (match-lambda*
    [(1) 1]
    [(n) (* n (factorial (- n 1)))]))
```

`defn`という名前で簡単な記述になるようにしました。

```scheme
(use util.match)

(define-syntax defn
  (syntax-rules ()
    ((_ f expr ...)
     (define f 
       (match-lambda* expr ...)))))
; (defn fac
;   [(1) 1]
;   [(n) (* n (fac (-- n)))])
```

### 遅延評価×部分適用

```scheme
(use gauche.lazy)

(define (lmap$ f) (pa$ lmap f))
(define (lfilter$ f) (pa$ lfilter f))
```

### 優先度付きキュー、ヒープ

https://practical-scheme.net/gauche/man/gauche-refj/hipu.html#g_t_30d2_30fc_30d7

```scheme
(use data.heap)
(define-inline mh make-binary-heap)
(define-inline hpush! binary-heap-push!)
(define-inline hpop-min! binary-heap-pop-min!)
(define-inline hpop-max! binary-heap-pop-max!)
(define-inline heapify build-binary-heap)
```

### Deque 両端キュー、リングバッファ

https://practical-scheme.net/gauche/man/gauche-refj/ringubatuhua.html#g_t_30ea_30f3_30b0_30d0_30c3_30d5_30a1

```scheme
(use data.ring-buffer)
(define-inline mdq make-ring-buffer)
(define-inline add-front! ring-buffer-add-front!)
(define-inline add-back! ring-buffer-add-back!)
(define-inline remove-front! ring-buffer-remove-front!)
(define-inline remove-back! ring-buffer-remove-back!)
```

## その他便利ライブラリ リンク集

### スタック、キュー

スタックはリスト、キューは上記Dequeで十分な気がします。

一応リンクは載せます。

* キュー: https://practical-scheme.net/gauche/man/gauche-refj/kiyu.html
* (スタック: なし)

### 組合せ

https://practical-scheme.net/gauche/man/gauche-refj/Zu-miHe-wase.html#g_t_7d44_307f_5408_308f_305b

* combinations(組み合わせ)
* permutations(順列)
* power-set(べき集合)
* cartesian-product(直積、デカルト積)

### トポロジカルソート

https://practical-scheme.net/gauche/man/gauche-refj/toporozikarusoto.html#g_t_30c8_30dd_30ed_30b8_30ab_30eb_30bd_30fc_30c8

### 素数、素因数分解

https://practical-scheme.net/gauche/man/gauche-refj/Su-Shu-.html#g_t_7d20_6570


## 自作ライブラリ

### 約数列挙

```scheme
(use gauche.lazy)
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
```

### Counter

PythonのCounter的な

```scheme
(use gauche.sequence)
(define (counter seq :optional (comp 'equal?))
  (fold
   (lambda (x col)
     (hash-table-set! col x (+ 1 (hash-table-get col x 0)))
     col)
   (make-hash-table comp)
   seq))
```

### メモ化

```scheme
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
;   [(n) (+ (fib (- n 1)) (fib (- n 2))))]
```

---

### おわりに

やる気があれば随時更新します。
