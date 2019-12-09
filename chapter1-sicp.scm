;; example 1.3
(define biggest
  (lambda (m n k)
    (cond
     ((and (<= m n) (<= m k)) (+ n k))
     ((and (<= n m) (<= n k)) (+ m k))
     (else (+ m n)))))

;; example 1.4
;; 请仔细考察上面给出的允许运算符为复合表达式的组合式的求值模型，根据对这一模型的认识，描述下面过程的行为：
(define a-plus-abs-b
  (lambda (a b)
    ((if (> b 0)
	+
	-) a b)))
;; 给定两个参数a b，判断b是否大于0，如果b大于0，则返回过程＋， 否则返回过程－，将过程应用于两个操作数

;; example 1.5  Ben Bitdiddle发明了一种检测方法，能够确定解释器究竟采用哪种序求值，是应用序还是正则
;; 序，他定义了下面两个过程：
(define p
  (lambda ()
    (p)))

(define test
  (lambda (x y)
    (if (= x 0)
	0
	y)))

(test 0 (p))
;; 如果某个解释器采用的是应用序求值（Applicative order），则解释器会陷入死循环，如果解释器采用的是正
;; 则序，将返回0（正则序会lazy bind）
;;-----------------------------------------------------------------------------------
(define sqrt-r
  (lambda (n)
    (letrec
	((sqrt-iter (lambda (guess)
		      (let ((nxt-guess (improve guess)))
			(if (good-enough? guess nxt-guess)
			    nxt-guess
			    (sqrt-iter nxt-guess)))))
	 (improve (lambda (guess)
		    (average guess (/ n guess))))
	 (average (lambda (m n)
		    (/ (+ m n) 2)))
	 (good-enough? (lambda (p n)
			 (< (abs (- p n)) 0.0001))))
      (sqrt-iter 1.0))))
		       
(define cube-root
  (lambda (n)
    (letrec
	((cube-iter (lambda (guess)
		      (let ((nxt-guess (improve guess)))
			(if (good-enough? guess nxt-guess)
			    nxt-guess
			    (cube-iter nxt-guess)))))
	 (improve (lambda (y)
		    (/ (+ (/ n (square y)) (* 2 y)) 3)))
	 (good-enough? (lambda (p n)
			 (< (abs (- p n)) 0.0001))))
      (cube-iter 1.0))))
;; 可以看到，使用牛顿法计算平方根和立方根的方法和过程基本类似，区别在于improve方法
;; 在更新的时候，使用的公式不一样。但是其思路都一样：需要不断逼近－improve；需要
;; 不断判断是否达到最终结果－good-enough?

(define factorial
  (lambda (n)
    (if (= n 1)
	1
	(* n (factorial (- n 1))))))
;; 上面的这个计算阶乘的函数是线性递归的过程，需要在栈中保存计算轨迹，阶乘的数学定义
;; 也是如此，所以可以用定义来编写，比较直观

(define factorial
  (lambda (n)
    (letrec
	((fac (lambda (result count)
		(if (= count 1)
		    result
		    (fac (* count result) (- count 1))))))
      (fac 1 n))))
;; 这种线性的递归过程，改写为迭代，状态变量是2个，而下面的那个树形递归（二个分支），状态变量是3个

(define fib
  (lambda (n)
    (cond
     ((= n 0) 0)
     ((= n 1) 1)
     (else (+ (fib (- n 1))
	      (fib (- n 2)))))))
;; 第一个fib是递归的计算过程，是树形的递归计算过程，这样的树形递归计算过程非常低效
;; 因为存在冗余的计算过程fib(1) fib(0)有多次的计算。

(define fib
  (lambda (n)
    (letrec
	((fib-iter (lambda (p n count)
		     (if (= count 0)
			 p
			 (fib-iter n (+ p n) (- count 1))))))
      (fib-iter 0 1 n))))
;; 第二个fib是迭代的计算过程，通过3个状态变量的更新，最终得到值，这样的计算过程更加高效

(define fib-list
  (lambda (n)
    (letrec
	((fib-list-iter (lambda (l n)
			  (if (= n 1)
			      l
			      (fib-list-iter (cons (+ (car l)
						(car (cdr l))) l)
					     (- n 1))))))
      (let ((lst (fib-list-iter '(1 0) n)))
	(reverse lst)))))
;; 上面的这个计算过程，用于列出fibonacci数列的前n项。思路比较简单

(define fib-list
  (lambda (n)
    (letrec
	((fib-list-iter (lambda (l count)
			  (if (> count n)
			      l
			      (fib-list-iter (cons (* (car l) count) l)
					     (+ count 1))))))
      (let ((lst (fib-list-iter '(1) 1)))
	(reverse lst)))))
;;-------------------------------------------------------------------------------------
;; 1.1.8过程做为黑箱抽象
;;-------------------------------------------------------------------------------------
;; 以上为线性的递归过程和线性的迭代过程
;; example 1.9
(define plus
  (lambda (a b)
    (if (= a 0)
	b
	(inc (plus (- a 1) b)))))

(define plus
  (lambda (a b)
    (if (= a 0)
	b
	(plus (- a 1) (+ b 1)))))
;; example 1.2.2
;; 换零钱方式的统计。给50美分，25美分，10美分，5美分和1美分的硬币，将1美元换成零钱，共有多少种方式
;; 如果用递归方式，可以考虑：(50 25 10 5 1)===>(100)
;; 确定用某种硬币＋确定不用某种硬币：l1:（50 25 10 5 1）==>(- 100 (car l1)) + (cdr l1) ==>(100)
(define change
  (lambda (amount)
    (letrec
	((change-iter (lambda (amount kinds-of-coins)
			(cond
			 ((= amount 0) 1)
			 ((or (< amount 0) (= kinds-of-coins 0)) 0)
			 (else (+ (change-iter amount (- kinds-of-coins 1))
				  (change-iter (- amount (value-of-coins kinds-of-coins)) kinds-of-coins))))))
	 (value-of-coins (lambda (n)
			   (cond
			    ((= n 1) 1)
			    ((= n 2) 5)
			    ((= n 3) 10)
			    ((= n 4) 25)
			    ((= n 5) 50)))))
      (change-iter amount 5))))

(define change
  (lambda (amount count p50 p25 p10 p5 p1)  ;; 换零钱在这里的主要问题是如何更新状态变量，更新状态变量的规则是什么？
    (cond
     ((= amount (+ (* p50 50) (* p25 25) (* p10 10) (* p5 5) p1))
      (let ()
	(display (list p50 p25 p10 p5 p1))
	(newline)
	(change amount (+ count 1) p50 p25 p10 p5 (+ p1 1))))
     ((> p50 2) count)
     ((> p25 4) (change amount count (+ p50 1) 0 0 0 0))
     ((> p10 10) (change amount count p50 (+ p25 1) 0 0 0))
     ((> p5 20) (change amount count p50 p25 (+ p10 1) 0 0))
     ((> p1 100) (change amount count p50 p25 p10 (+ p5 1) 0))
     (else (change amount count p50 p25 p10 p5 (+ p1 1))))))
;;-------------------------------------------------------------------------------------
(define pascal-triangle
  (lambda (n)
    (letrec
	((pascal-iter (lambda (row col)
			(cond
			 ((= col 1) 1)
			 ((= row col) 1)
			 (else (+ (pascal-iter (- row 1) (- col 1))
				  (pascal-iter (- row 1) col))))))  ;; 计算pascal三角中的值
	 (make-a-line (lambda (row col)
			(cond
			 ((< col 1) '())
			 (else (cons (pascal-iter row col)
				     (make-a-line row (- col 1)))))))
	 (make-pascal-triangle (lambda (m)
				 (cond
				  ((> m n) '())
				  (else (cons (make-a-line m m)
					      (make-pascal-triangle (+ m 1))))))))
      (make-pascal-triangle 1))))

;; 下面的这个是pascal三角的迭代实现
(define pascal-triangle
  (lambda (n)
    (letrec
	((pascal-iter (lambda (m l)            ;; l是pascal三角的一行的值, m是行数
			(cond
			 ((= m n) l)
			 (else
			  (pascal-iter (+ m 1) (make-list l))))))
	 (make-list (lambda (l)
		      (letrec
			  ((mklist (lambda (l)
				     (cond
				      ((null? (cdr l)) '(1))
				      (else (cons (+ (car l) (car (cdr l))) (mklist (cdr l))))))))
			(cons 1 (mklist l))))))
      (pascal-iter 1 '(1)))))
		       
;;-------------------------------------------------------------------------------------
;; excerise 1.11
(define fac
  (lambda (n)
    (cond
     ((< n 3) n)
     (else (+ (fac (- n 1))
	      (* 2 (fac (- n 2)))
	      (* 3 (fac (- n 3))))))))

;; if n<3 return n, else return f(n-1)+2*f(n-2)+3*f(n-3)
(define fac
  (lambda (n)
    (letrec
	((fac-iter (lambda (n p1 p2 p3)
		    (let ((p (+ p1 (* 2 p2) (* 3 p3))))
		      (cond
		       ((= n 3) p)
		       (else (fac-iter (- n 1) p p1 p2)))))))
      (fac-iter n 2 1 0))))
;;--------------------------------------------------------------------------------------
(define sine
  (lambda (x)
    (if (< (abs x) 0.01)
	x
	(- (* 3 (sine (/ x 3)))
	   (* 4 (cube (sine (/ x 3))))))))
(define cube
  (lambda (m)
    (* m m m)))
;; 以上是计算sin的递归计算方法，也是很自然的思考方式，下面的这个实现，是迭代，但是不自然
(define sine
  (lambda (x)
    (letrec
	((val-times (lambda (val times)  ;; val是最终的那个迭代的小于0.01的值，times是需要迭代的次数
		      (if (< (abs val) 0.01)
			  (cons val (cons times '()))
			  (val-times (/ val 3) (+ times 1)))))
	 (sine-iter (lambda (smallest times)
		      (letrec
			  ((sine-iter-small (lambda (result count)  ;; count初值为0, result初值为最小的那个值
					      (if (= count times)
						  result
						  (sine-iter-small (- (* 3 result)
								      (* 4 (cube result)))
								   (+ count 1))))))
			(sine-iter-small smallest 0)))))
      (let ((val (val-times x 0)))
	(sine-iter (car val) (car (cdr val)))))))

(define sine
  (lambda (x)
    (letrec
	((sine-iter (lambda (item result count)  ;; item x, result x, count is 1
		      (cond
		       ((< (abs item) 0.01) result)
		       (else
			(let ((nxtitem (calc item count)))
			  (sine-iter nxtitem (+ result nxtitem) (+ count 2)))))))
	 (calc (lambda (n count)  ;; n's initial value is x too
		 (- 0 (/ (* n (square x)) (* (+ count 1) (+ count 2)))))))
      (sine-iter x x 1))))
;; sinx = x - x^3/3! + x^5/5!....
;;----------------------------------------------------------------------------------------
(define expt
  (lambda (b n)
    (if (= n 0)
	1
	(* b (expt b (- n 1))))))

(define expt
  (lambda (b n)
    (letrec
	((expt-iter (lambda (result times)
		      (if (= times 0)
			  result
			  (expt-iter (* b result) (- times 1))))))
      (expt-iter 1 n))))

(define expt
  (lambda (b n)
    (letrec
	((expt-iter (lambda (result product m)
		      (cond
		       ((= m 0) result)
		       ((even? m) (expt-iter result (square product) (/ m 2)))
		       (else (expt-iter (* result product) product (- m 1)))))))
      (expt-iter 1 b n))))
;; 一般说，定义一个不变量，要求它在状态之间保持不变，这一技术是思考迭代算法设计问题的一种非常强有力的方法

(define fast-exp
  (lambda (b n)
    (letrec
	((fast-exp-iter (lambda (m result)
			  (cond
			   ((= m n) result)
			   ((> m n) (fast-exp-iter (- m 1) (/ result b)))
			   (else (fast-exp-iter (* m 2) (square result)))))))
      (fast-exp-iter 1 b))))
;; 事实证明，我的思维没有变化（也可以说是没有提高，现在想到的办法和以前的办法是一样）
(define fast-exp
  (lambda (b n)
    (letrec
	((fast-exp-iter (lambda (result m)  ;; m的初值是n，当m为1时，返回result的值
			  (cond
			   ((= m 0) result)
			   ((odd? m) (fast-exp-iter (* b result) (- m 1)))
			   (else (fast-exp-iter (* result (square b)) (- m 2)))))))
      (fast-exp-iter 1 n))))
;; 上面的这个计算过程，效率也不高，因为指数是每次减2，而不是每次除2变化的。

;;-----------------------------------------------------------------------------------------
;; excerise 1.17
(define o*
  (lambda (a b)
    (if (= b 0)
	0
	(+ a (o* a (- b 1))))))
;; 这一算法具有相对于b的线性步数。现在假定除了加法操作之外，还有运算double，它能求出一个整数的两倍；还有halve，
;; 它将一个偶数除以2.请用这些运算设计一个类似fast-expt的求乘积过程，使之只用对数的计算步数。
(define o*
  (lambda (a b)
    (cond
     ((= b 0) 0)
     ((even? b) (double (o* a (halve b))))
     (else (+ a (o* a (- b 1)))))))

(define double
  (lambda (m)
    (+ m m)))

(define halve
  (lambda (m)
    (/ m 2)))

;; excerise 1.18
(define o*
  (lambda (a b)
    (letrec
	((o*iter (lambda (sum product count)  ;; count的初值应当为b, sum的初值为0, product的初值为a
		   (cond
		    ((= count 0) sum)
		    ((even? count) (o*iter sum (double product) (halve count)))  ;; 构造一个不变量，在状态转换之间不变
		    (else (o*iter (+ sum product) product (- count 1))))))       ;; 在此构造的一个不变量，是b的值
	 (double (lambda (m)
		   (+ m m)))
	 (halve (lambda (m)
		  (/ m 2))))
      (o*iter 0 a b))))
;;---------------------------------------------------------------------------------------------		   
;; excerise 1.19 存在着一种以对数步数求出斐波那契数的巧妙算法。存在着变换规则a<-a+b和b<-a，现在将这种变换称为T变换。通过观察可以发现
;; 从1和0开始，将T反复应用n次，将产生出一对数Fib(n+1)和Fib(n)。换句话说，斐波那契数可以通过将T^n（变换T的n次方）应用于对偶（1 0）而产生
;; 现在将T看做是变换族T(pq)中p＝0，q＝1的特殊情况，其中T(pq)是对于对偶（a，b）按照a<-bq+aq+ap和b<-bp+aq规则的变换。
(define fib
  (lambda (n)
    (letrec
	((fib-iter (lambda (a b p q count)
		     (cond
		      ((= count 0) b)
		      ((even? count) (fib-iter a b (+ (square p) (square q)) (+ (square q) (* 2 p q)) (/ count 2)))
		      (else (fib-iter (+ (* b q) (* a q) (* a p))
				      (+ (* b p) (* a q))
				      p
				      q
				      (- count 1)))))))
      (fib-iter 1 0 0 1 n))))

;; exercise 用欧几里得算法求最大公约数， 基于以下方法，(a b)的最大公约数，也是(b (a % b))的最大公约数
(define gcde
  (lambda (m n)
    (if (= n 0)
	m
	(gcde n (remainder m n)))))
;; Lame定理：如果欧几里得算法需要用k步计算出一对整数的GCD，那么这对数中较小的那个数必然大于或者等于第k个斐波那契数
;; 这个算法的时间复杂度是（logN）
;; ----------------------------------------------------------------------------------------------
(define prime?
  (lambda (n)
    (letrec
	((prime-iter (lambda (m)   ;; m的初值为2
		       (cond
			((> (square m) n) #t)
			((= (remainder n m) 0) #f)
			(else (prime-iter (+ m 1)))))))
      (prime-iter 2))))
;; 上面这个算法的时间复杂度是sqrt(n)
;; -----------------------------------------------------------------------------------------------
;; 费马小定理：如果n是一个素数，a是小于n的任意正整数，那么a的n次方与a模n同余: a^n mod n = a 我们需要一个过程，计算
;; 某个数的幂对另一个数取模的结果。
;; 对于指数值e大于1的情况，所采用归约方式是基于下面的事实：对于任意的x、y和m，我们总可以通过分别计算x取模m和y取模m，而
;; 后将它们乘起来之后取模m，而后将它们乘起来之后取模m，得到x乘y取模的余数.
;; 证明如下：令x＝k1*m+p(p<m), y=k2*m+q(q<m)，则x % m = p, y % m = q，
;; 所以(x*y) % m = (k1*k2*m^2 + k1*m*q + k2*m*p + p*q) % m = (p*q) % m
;; 即：((x % m) * (y % m)) % m = (x*y) % m
;; 如果e是偶数时，我们计算b^(e/2)取m的余数，求它的平方，而后再求它取模m的余数。这种技术非常有用，因为它意味着我们的计算
;; 中不需要处理比m大很多的数
(define expmod
  (lambda (base exp m)
    (cond
     ((= exp 0) 1)
     ((even? exp) (remainder (square (expmod base (/ exp 2) m)) m))
     (else (remainder (* base (expmod base (- exp 1) m)) m)))))
;; 这个过程采用连续求平方的方式，使相对于计算中指数步数增长的阶是对数的。
;; 执行费马检查需要选取位于1和n-1之间的数a，而后检查a的n次幂取模n的余数是否等于a。随机数a的选取通过过程random完成。我们
;; 假定它已经包含在scheme的基本过程中，它返回比其整数输入小的某个非负整数。这样，要得到1和n－1之间的随机数，只需用输入n-1
;; 去调用random，并将结果加1(最小要从2开始)
(define fermat-test
  (lambda (n)
    (letrec
	((expmod (lambda (base exp n)
		   (cond
		    ((= exp 0) 1)
		    ((even? exp) (remainder (square (expmod base (/ exp 2) n)) n))
		    (else (remainder (* base (expmod base (- exp 1) n)) n))))))
      (let ((base (+ (random (- n 1)) 1)))
	(= (expmod base n n) base)))))

;; 下面的这个费马测试，是取遍了小于n的所有整数来检测（其实意义不大）
(define fermat-test
  (lambda (n)
    (letrec
	((expmod (lambda (base exp)
		   (cond
		    ((= exp 0) 1)
		    ((even? exp) (remainder (square (expmod base (/ exp 2))) n))
		    (else (remainder (* base (expmod base (- exp 1))) n)))))
	 (fermat (lambda (base)
		   (cond
		    ((= base n) #t)
		    ((not (= (expmod base n) base)) #f)
		    (else (fermat (+ base 1)))))))
      (fermat 2))))

;; -------------------------------------------------------------------------------------
(define fermat-test
  (lambda (n)
    (letrec
	((expmod (lambda (base exp)
		   (cond
		    ((= exp 0) 1)
		    ((even? exp) (remainder (square (expmod base (/ exp 2))) n))
		    (else (remainder (* base (expmod base (- exp 1))) n)))))
	 (fast-prim? (lambda (base)
		       (= (expmod base n) base))))
      (let ((base (+ (random (- n 1)) 1)))
	(fast-prim? base)))))           ;;2147483647=2^31-1 is a big prime. You can test the time consumed by the two method.

(define t-prime?
  (lambda (n)
    (letrec
	((smallest-divsor (lambda (m)
			    (cond
			     ((= (remainder n m) 0) m)
			     ((> (square m) n) n)
			     (else (smallest-divsor (+ m 1)))))))
      (cond
       ((= (smallest-divsor 2) n) #t)
       (else #f)))))

(define timed-prime-test
  (lambda (primfunc n)
    (letrec
	((timeused (lambda (start)
		     (display (primfunc n))
		     (newline)
		     (display n)
		     (newline)
		     (display (- (runtime) start)))))
      (timeused (runtime)))))
;; -----------------------------------------------------------------------------------------------
(define factor
  (lambda (n)
    (letrec
	((factor-iter (lambda (m)
			(cond
			 ((> (square m) n) (cons n '()))
			 ((= (remainder n m) 0) (cons m (factor (/ n m))))
			 (else (factor-iter (+ m 1)))))))
      (factor-iter 2))))

(define timed-prime-test
  (lambda (n)
    (letrec
	((start-prime-test (lambda (n start-time)
			     (if (prime? n)
				 (report-prime (- (runtime) start-time)))))
	 (report-prime (lambda (elapsed-time)
			 (let ()
			   (display "***")
			   (display elapsed-time))))
	 (prime? (lambda (n)
		  (letrec
		      ((expmod (lambda (base exp)
				 (cond
				  ((= exp 0) 1)
				  ((even? exp) (remainder (square (expmod base (/ exp 2))) n))
				  (else (remainder (* base (expmod base (- exp 1))) n)))))
		       (fermat (lambda (exp)
				 (let ((base (+ (random (- n 1)) 1)))
				   (= (expmod base exp) base)))))
		    (fermat n)))))
      (start-prime-test n (runtime)))))
;; --------------------------------------------------------------------------------------------------
(define search-for-primes
  (lambda (prime? first last)
    (letrec
	((search-iter (lambda (start)     ;; start的初值是first
			(cond
			 ((> start last) '())
			 ((prime? start) (cons start (search-iter (+ start 1))))
			 (else (search-iter (+ start 1)))))))
      (search-iter first))))

(define search-for-primes
  (lambda (prime? first last count)
    (letrec
	((search-iter (lambda (current count)    ;; current的初值是first，count的初值是count
			(cond
			 ((or (> current last) (= count 0)) '())
			 ((prime? current) (cons current (search-iter (next-odd current) (- count 1))))
			 (else (search-iter (next-odd current) count)))))
	 (next-odd (lambda (n)
		     (+ n 2))))
      (if (even? first)
	  (search-iter (+ first 1) count)
	  (search-iter first count)))))
;; --------------------------------------------------------------------------------------------------
;; 费马检查的一种不会被欺骗的变形称为Miller－Rabin检查，它来源于费马小定理的一个变形。这一变形断言，如果n是素数，a是任何小于
;; n的整数，则a的（n－1）次幂与1模n同余。要用Miller－Rabin检查考察n的素性，我们应随机地取一个数a小于n，并用过程expmod求
;; a的（n－1）次幂对n的模。然而，在执行expmod的平方步骤时，我们需要查看是否遇到了“1取模n的非平凡平方根”，也就是说，是不是存
;; 在不等于1或者n－1的数，其平方取模n等于1。可以证明，如果1的这种非平凡平方根存在，那么n就不是素数。还可以证明，如果n是非素数
;; 的奇数，那么至少有一半的数a<n，按照这种方式计算a^(n-1)，将会帅到1取模n的非平凡平方根。这也是Miller－Rabin检查不会受骗的原因
(define Miller-Rabin
  (lambda (n)
    (letrec
	((expmod (lambda (base exp)     ;; exp的初值是n－1，base的初值是一个小于n的数
		   (cond
		    ((= exp 0) 1)
		    ((even? exp)
		     (let ((v (expmod base (/ exp 2))))
		       (if (Miller-Test v)
			   0
			   (remainder (square v) n))))
		    (else (remainder (* base (expmod base (- exp 1))) n)))))
	 (Miller-Test (lambda (x)
			(cond
			 ((or (= x 1) (= x (- n 1))) #f)
			 (else (= (remainder (square x) n) 1))))))
      (let ((base (+ (random (- n 1)) 1)))
	(= (expmod base (- n 1)) 1)))))
		   
;;-----------------------用高阶函数做抽象----------------------------------------------------------------
;; 1.3.1过程作为参数
(define sum-integers
  (lambda (a b)
    (cond
     ((> a b) 0)
     (else (+ a (sum-integers (+ a 1) b))))))

(define sum-integers
  (lambda (a b)
    (letrec
	((sum-integers-iter (lambda (current result)
			(if (> current b)
			    result
			    (sum-integers-iter (+ current 1) (+ result current))))))
      (sum-integers-iter a 0))))

(define sum-cubes
  (lambda (a b)
    (cond
     ((> a b) 0)
     (else (+ (cube a) (sum-cubes (+ a 1) b))))))

(define sum-cubes
  (lambda (a b)
    (letrec
	((sum-cubes-iter (lambda (current result)
			   (if (< current b)
			       result
			       (sum-cubes-iter (+ current 1) (+ result (cubes current))))))
	 (cube (lambda (m)
		 (* m m m))))
      (sum-cubes-iter a 0))))

(define pi-sum
  (lambda (a b)
    (if (> a b)
	0
	(+ (/ 1.0 (* a (+ a 2))) (pi-sum (+ a 4) b)))))
;; 可以明显看出，这三个过程共享着一种公共的基础模式。它们的很大一部分是共同的。只在所用的过程名字上不一样：用于从a
;; 算出需要加的项的函数，还有用于提供下一个a值的函数。所以可以定义如下的过程来表达＋这个概念！：
;; *****************************************************
(define sum
  (lambda (term a next b)
    (if (> a b)
	0
	(+ (term a) (sum term (next a) next b)))))

(define sum
  (lambda (term a next b)
    (letrec
	((sum-iter (lambda (current result)
		     (cond
		      ((> current b) result)
		      (else (sum-iter (next current) (+ result (term current))))))))
      (sum-iter a 0))))
;; *****************************************************
(define inc
  (lambda (a)
    (+ a 1)))
(define identity
  (lambda (x)
    x))
;; 一旦有了sum，我们就能用它作为基本构件，去形式化其他概念。例如求函数f在范围a和b之间的定积分的近似值
(define integral
  (lambda (f a b dx)
    (letrec ((add-dx (lambda (x)
		       (+ x dx))))
      (* (sum f (+ a (/ dx 2.0)) add-dx b) dx))))
;; 采用辛普森规则计算数值积分
;; (h/3)[y0+4y1+2y2+4y3+2y4+...+2y(n-2)+4y(n-1)+yn], h=(b-a)/n,n是某个偶数,yk=f(a+kh)
(define simpson-integral
  (lambda (f a b n)       ;; n取某个偶数值
    (let ((h (/ (- b a) n)))
      (letrec
	  ((sum (lambda (count)
		  (if (> count n)
		      0
		      (+ (p count) (sum (+ count 1))))))
	   (p (lambda (count)
		(cond
		 ((= count 0) (f a))
		 ((= count n) (f b))
		 ((even? count) (* 2 (f (+ a (* count h)))))
		 (else (* 4 (f (+ a (* count h)))))))))
	(/ (* h (sum 0)) 3)))))
	
(define simpson-integral
  (lambda (f a b n)
    (let ((h (/ (- b a) n)))
      (letrec
	  ((yn (lambda (k)
		 (let ((val (+ a (* k h))))
		   (cond
		    ((or (= k 0) (= k n)) (f val))
		    ((even? k) (* 2 (f val)))
		    (else (* 4 (f val)))))))
	   (sum (lambda (fun start next end)    ;; fun是函数yn， start是0， next是将start增1， end是n
		  (if (> start end)
		      0
		      (+ (fun start) (sum fun (next start) next end)))))
	   (next (lambda (x)
		   (+ x 1))))
	(/ (* h (sum yn 0 next n)) 3)))))
;; 将函数做了一个适当的变形，sum函数所提供的抽象的概念仍保持不变。比上面的那个函数实现更接近课本所要表达的意思
;; ********************Give product function**************************************************
(define product
  (lambda (term a b next)
    (cond
     ((> a b) 1)
     (else (* (term a) (product term (next a) b next))))))

(define product
  (lambda (term a b next)
    (letrec
	((product-recusive (lambda (current)
			     (cond
			      ((> current b) 1)
			      (else (* (term current) (product-recursive (next a))))))))
      (product-recusive a))))

(define product
  (lambda (term a b next result)
    (if (> a b)
	result
	(product (term (next a) b next (* result (term a)))))))

(define product
  (lambda (term a b next)
    (letrec
	((product-iter (lambda (current result)
			 (if (> current b)
			     result
			     (product-iter (next current) (* result (term current)))))))
      (product-iter a 1))))

(define factorial
  (lambda (n)
    (product (lambda (x) x) 1 n (lambda (x) (+ x 1)))))

(define quarter-pi
  (lambda (n)
    (letrec
	((term1 (lambda (x)
		  (if (or (= x 2) (= x n))
		      x
		      (square x))))
	 (next (lambda (x)
		 (+ x 2))))
      (let ((x (product term1 2 n next))
	    (y (product square 3 (- n 1) next)))
	(/ x y)))))
;; the above solution may cause overflow, because the multiply is too big maybe

(define quarter-pi
  (lambda (n)
    (letrec
	((pi-term (lambda (x)
		    (if (even? x)
			(/ (+ x 2) (+ x 1))
			(/ (+ x 1) (+ x 2))))))
      (product pi-term 1 n (lambda (x) (+ x 1))))))
;;-----------------------------------------------------------------------------------------
;; sum和product是另一个称为accumulate的更一般概念的特殊情况，accumulate使用某些一般性的累积函数组合起一系列项
;; (accumulate combiner null-value term a next b)  ;; 对sum来说，combiner就是＋，对product来说，combiner
;; 就是＊，所以accumulate函数，多了一个combiner，用于表达更一般的累积。
(define accumulate
  (lambda (combiner null-value term a next b)
    (if (> a b)
	null-value
	(combiner (term a) (accumulate combiner null-value term (next a) next b)))))

(define accumulate
  (lambda (combiner null-value term a next b)
    (letrec
	((accumulate-iter (lambda (curr result)
		       (if (> curr b)
			   result
			   (accumulate-iter (next curr) (combiner curr result))))))
      (accumulate-iter a null-value))))

(define sum
  (lambda (term a next b)
    (accumulate + 0 term a next b)))

(define product
  (lambda (term a next b)
    (accumulate * 1 term a next b)))
;;------------------------------------------------------------------------------------------
;; 你可以通过引进一个处理被组合项的过滤器概念，写出一个比accumulate更一般的版本。也就是说，在计算过程中，只组合起
;; 由给定范围得到的项里的那些满足特定条件的项。这样得到的filtered－accumulate抽象取与上面累积过程同样的参数，再
;; 加上一个另外的描述有关过滤器的谓词参数。
(define filtered-accumulate
  (lambda (combiner filter? null-value term a next b)
    (if (> a b)
	null-value
	(if (filter? a)
	    (combiner (term a) (filtered-accumulate combiner filter? null-value term (next a) next b))
	    (combiner (term null-value) (filtered-accumulate combiner filter? null-value term (next a) next b))))))

(define sum-of-primes
  (lambda (a b)
    (filtered-accumulate + t-prime? 0 (lambda (x) x) a (lambda (x) (+ x 1)) b)))

(define multi-prime-lessn
  (lambda (n)
    (letrec
	((prime-each-other (lambda (x)
			     (= (gcde n x) 1))))  ;; gcde的定义见上面
      (filtered-accumulate * prime-each-other 1 (lambda (x) x) 1 (lambda (x) (+ x 1)) n))))
;;------------------------------------------------------------------------------------------
;; 通过区间折半寻找方程的根
(define half-interval-method
  (lambda (f a b)
    (letrec
	((half-method (lambda (neg-point pos-point)
			(let ((mid-point (/ (+ neg-point pos-point) 2)))
			  (if (good-enough? neg-point pos-point)
			      mid-point
			      (if (positive? (f mid-point))
				  (half-method neg-point mid-point)
				  (half-method mid-point pos-point))))))
	 (good-enough? (lambda (neg pos)
			 (< (abs (- neg pos)) 0.001))))
      (let ((a-value (f a))
	    (b-value (f b)))
	(cond
	 ((and (negative? a-value) (positive? b-value)) (half-method a b))
	 ((and (negative? b-value) (positive? a-value)) (half-method b a))
	 (else (error "Values are not of opposite sign" a b)))))))
;; 思考：用区间的折半查找寻找函数的0点，在数学上有较为严格的定义，也给出了计算的方法，用计算机来做比较简单，用
;; scheme来实现也比较容易，这个题目中，稍微难想的一点是在最开始就限定了参数的neg－point和pos－point
;; 寻找函数的不动定：如果函数满足方程f(x)=x,则x就是函数f的不动点，判定不动点的依据是f(f(f(f(x))))和x足够接近
(define fixed-point
  (lambda (f guess tolerance)  ;; 这个guess是最初给猜测值，tolerance是精度值
    (letrec
	((fixed-point-iter (lambda (guess)  ;; 这个guess的值会随着计算过程不断变动
			     (let ((val (f guess)))
			       (if (close-enough? val guess)
				   val
				   (fixed-point-iter val)))))
	 (close-enough? (lambda (v1 v2)
			  (< (abs (- v1 v2)) tolerance))))
      (fixed-point-iter guess))))
;; 上面这一不动点的计算过程，使人想起了之前用于找平方根的计算过程。两者都是基于同样的想法：通过不断地改进猜测，直至
;; 结果满足某一评价准则为止。事实上，我们完全可以将平方根的计算形式化为一个寻找不动点的计算过程。计算某个数的平方根
;; 就是要找到一个y，使得y^2=x（注意，x是已知的数）: y=x/y
(define sqrt-fixed
  (lambda (n)
    (fixed-point (lambda (y) (/ n y)) 1.0 0.001)))
;; 遗憾的是，上面的这个不动点搜寻并不收敛。考虑某个初始猜测y1，下一个猜测将是y2＝x/y1，而再下一个猜测是y3=x/y2=x/(x/y1)=y1
;; 结果是进入了一个无限循环，其中没完没了地反复出现两个猜测y1和y2，在答案的两边往复振荡。控制这类振荡的一种方法是不让有关的猜测
;; 变化太剧烈。因这实际答案总是在两个猜测y和x/y之间， 我们可以做出一个猜测，使之不像x/y那样远离y，为此可以用y和x/y的平均值。
;; 这样，我们取y之后的下一个猜测值为(1/2)(y+x/y)而不是x/y。请注意：y=(1/2)(y+x/y)是方程y=x/y经过简单变换的结果。导出它的
;; 方式是在方程两边都加y，然后将两边都除以2（将函数做变形处理）。经过这一修改，平方根过程就能正常工作了。事实上，如果我们仔细分析
;; 这一定义，那么就可以看到，它在求平方根时产生的近似值序列。这种取逼进一个解的一系列值的平均值的方法，是一种称为平均阻尼的技术。
;; 它常常用在不动点搜寻中，作为帮助收敛的手段。
(define gold-ration
  (lambda ()
    (fixed-point (lambda (y) (+ 1 (/ 1 y))) 2.0 0.001)))
;; ------------------------------------------------------------------------------------------------------
(define fixed-point
  (lambda (f guess tolerance)
    (letrec
	((fixed-point-iter (lambda (guess)
			     (let ((next (f guess)))
			       (let ()
				 (newline)
				 (display next)
				 (if (close-enough? next guess)
				     next
				     (fixed-point-iter next))))))
	 (close-enough? (lambda (next guess)
			  (< (abs (- next guess)) tolerance))))
      (fixed-point-iter guess))))
;; 之所以要写这个函数的目的是通过这个函数，可以查看该不动点函数收敛的状态，收敛的快慢,从而有可能需要改进函数的表达式
;; 找出函数x=log(1000)/log(x)的不动点 : (fixed-point (lambda (x) (/ (log 1000) (log x))) 4.0 0.001)
;; 确定方程x^x=1000的一个根，可以归纳为求函数的不动点
;; 如果要测试平均阻尼，可以将方程变形为如下形式：(/ (+ x (/ (log 1000) (log x))) 2)
;; (fixed-point (lambda (x) (/ (+ x (/ (log 1000) (log x))) 2)) 4.0 0.001)这个计算的步骤确实小很多
;; *******************************************************************************************************
(define cont-frac
  (lambda (n d k)      ;; n和d都是函数，用于产生无限连分式的分子和分母，k则是要计算无限连分式的截断值
    (letrec
	((cont-frac-iter (lambda (count)
			   (cond
			    ((= count k) (/ (n count) (d count)))
			    (else (/ (n count) (+ (d count) (cont-frac-iter (+ count 1)))))))))
      (cont-frac-iter 1))))

(cont-frac (lambda (x) 1.0) (lambda (x) 1.0) 20)

;; 迭代方式从最下层开始
(define cont-frac
  (lambda (n d k)
    (letrec
	((cont-frac-iter (lambda (count curd)   ;; curd 应为最后的那个分母
			   (cond
			    ((= count 1) (/ (n count) curd))
			    (else (cont-frac-iter (- count 1) (+ (d (- count 1)) (/ (n count) curd)))))))) ;; 每次更新的都是被除数的值
      (cont-frac-iter k (d k)))))
;; 上面的那个迭代实现中，最主要的是那个curd量的更新方法，curd的初值是最后的那个分母，每次更新时，都更新一下分母，更新
;; 的公式是(+ (d (- count 1)) (/ (n count) curd)):D(n-1)+N(n)/D(n)
;;*********************************************************************************************************
;; 练习1.38中，欧拉发表的论文包含了e-2的一个连分式展开，在这一分式中， Ni全部都是1， Di依次是1，2，1，1，4，1，1，6，1，1，8，1
;; 利用cont－frac过程，求出基于这种连分式的e的近似值。很显然，这个题目中，最主要的就是要写出合适的D函数（每3个数1组）
(define D
  (lambda (n)
    (if (or (= (remainder n 3) 0) (= (remainder n 3) 1))
	1
	(+ (* (quotient n 3) 2) 2))))

(cont-frac (lambda (x) 1.0) D n)
;; 上面的这个函数是我想出来的，虽然比较正常，但哈哈哈哈哈，感觉很好
(define tan-cf
  (lambda (x k)
    (letrec
	((N (lambda (count)
	      (if (= count 1)
		  x
		  (- 0 (square x)))))
	 (D (lambda (count)
	      (- (* 2 count) 1))))
      (cont-frac N D k))))
;; 思考：对于后续的应用(e-2), (tan-cf)等，写出那个连分式的对应函数，是最重要的！而连分式是具有一般概念的一种形式，一旦将这种一般概念
;; 所对应的内容表达了出来，对于特殊的应用就比较容易了
;; ---------------------------------------------------------------------------------------------------------
;; 过程作为返回值
;; 之前的例子说明，将过程作为参数传递，能够显著增强我们的程序设计语言的表达能力，通过创建另一种其返回值本身也是过程的过程，我们还能得到
;; 进一步的表达能力。平均阻尼本身也是一种很有用的一般性技术。自然，给定一个函数f之后，就可以考虑另一个函数，它在x处的值等于x和f（x）的平均值
(define average-damp
  (lambda (f)
    (lambda (x)
      (average x (f x)))))    ;; 表达了平均阻尼技术的实现,利用平均阻尼技术来改定函数

(define average
  (lambda (m n)
    (/ (+ m n) 2)))
;; 利用average－damp，我们可以重做前面的平方根过程如下：
(define sqrt-fixed
  (lambda (x)
    (fixed-point (average-damp (lambda (y) (/ x y))) 1.0 0.0001)))

(define fixed-point
  (lambda (f guess tolerance)  ;; 这个guess是最初给猜测值，tolerance是精度值
    (letrec
	((fixed-point-iter (lambda (guess)  ;; 这个guess的值会随着计算过程不断变动
			     (let ((val (f guess)))
			       (if (close-enough? val guess)
				   val
				   (fixed-point-iter val)))))
	 (close-enough? (lambda (v1 v2)
			  (< (abs (- v1 v2)) tolerance))))
      (fixed-point-iter guess))))
;; 同之前函数不动点的比较，这些过程表述的是同一计算过程，也应注意，当我们利用这些抽象描述该计算过程时，其中的想法如何变得更加清晰了
;; 将一个计算过程形式化为一个过程，一般存在很多不同的方式，有经验的程序员知道如何选择过程的形式，使其特别地清晰且易于理解，使该计算
;; 过程中有用的元素能表现为一些相互分离的个体，并使它们还可能重新用于其他的应用！作为重用的一个简单实例，请注意x的立方根是函数y->x/y^2
;; 的不动点，因此我们可以立即将前面的平方根过程推广为一个提取立方根的过程
(define cube-root
  (lambda (x)
    (fixed-point (average-damp (lambda (y) (/ x (square y)))) 1.0 0.001)))
;; --------------------------------------------------------------------------------------------------------
;; 牛顿法:如果g（x）是一个可微函数，那么议程g（x）＝0的一个解就是函数f（x）的一个不动点，其中：f(x)=x-g(x)/Dg(x)，这里的Dg(x)是g对x的层数。
;; 牛顿法是用来求方程的根的，但是有了上面的公式，我们就可以把求根的过程，改变成求函数的不动点的过程。而函数不动点的求法，我们已经定义过了
;; 求导函数的方法：Dg(x)=(g(x+dx)-g(x))/dx
(define deriv
  (lambda (g)    ;; g是一个函数
    (lambda (x)
      (/ (- (g (+ x dx)) (g x)) dx))))   ;; 此处的dx可以自行定义为0.0001

(define newton-transform     ;; 通过牛顿变換，将要求根的方程，转变成要求函数不动点的方程
  (lambda (g)
    (lambda (x)
      (- x (/ (g x) ((deriv g) x))))))

(define newton-method        ;; 牛顿法求g的根，guess为最初的猜测值
  (lambda (g guess tolerance)
    (fixed-point (newton-transform g) guess tolerance)))

(define sqrt-newton
  (lambda (x)
    (newton-method (lambda (y) (- (square y) x)) 1.0 0.001)))
;; 思考：有了求不动点的函数，根据牛顿法（即牛顿公式），可以将求方程根的问题转换为求函数不动点的问题,不动点求出来了，相关的方程的根也求出来了
;; ****************************************************************************************************************
;; 抽象和第一级过程
;; 上面我们已经看到，用两种方式，它们都能将平方根计算表述为某种更一般方法的实例，一个是作为不动点搜寻过程，另一个是使用牛顿法。因为牛顿法本身
;; 表述的也是一个不动点的计算过程，所以我们实际上看到了将平方根计算作为不动点的两种形式。每种方法都是从一个函数出发，找出这一函数在某种变换下
;; 的不动点。我们可以将这一具有普遍性的思想表述为一个函数
(define fixed-point-of-transform
  (lambda (g transform guess tolerance)
    (fixed-point (transform g) guess tolerance)))
;; 这个非常具有一般性的过程有一个计算某个函数的过程参数g，一个变换g的过程，和一个初始猜测，它返回经这个一般性方法的实例：
(define sqrt-aver
  (lambda (x tolerance)
    (fixed-point-of-transform (lambda (y) (/ x y))
			      average-damp              ;; 将上面的函数变换的过程
			      1.0
			      tolerance
			      )))

(define sqrt-newton
  (lambda (x tolerance)
    (fixed-point-of-transform (lambda (y) (- (square y) x))
			      newton-transform
			      1.0
			      tolerance)))
;; 思考：上述的fixed-point-of-transform是将抽象的层次又上升了一层， 本质上都是使用了求函数不动点的方法
;; 在1.3节开始时研究复合过程，并将其作为一种至关重要的抽象机制，因为它使我们能将一般性的计算方法，用这一程序设计语言里的元素明确描述。现在我
;; 们又看到，高阶函数能如何去操作这些一般性的方法，以便建立起进一步的抽象。
;; 作为编程者，我们应该对这类可能性保持高度敏感，设法从中识别出程序里的基本抽象（上述的例子中，求函数不动点就是最基本的抽象），基于它们去进一
;; 点构造，并推广它们以创建威力更加强大的抽象。当然，这并不是说总应该采用尽可能抽象的方式去写程序，程序设计专家们知道如何根据工作中的情况，去
;; 选择合适的抽象层次。但是，能基于这种抽象去思考确实是最重要的，只有这样才可能在新的上下文中去应用它们。高阶过程的重要性，就在于使我们能显式
;; 地用程序设计的要素去描述这些抽象，使我们能像操作其他计算元素一样去操作它们（它们应该指那些抽象）。
;; 一般而言，程序设计语言总会对计算元素的可能使用方式强加上某些限制（如C语言中对于数的使用，就只能做加减乘除）。带有最少限制的元素被称为具有
;; 第一级的状态，第一级元素的某些权利或特权包括：（1）可以用变量命名；（2）可以提供给过程作为参数；（3）可以由过程作为结果返回；（4）可以包含
;; 在数据结构中。LISP不像其他程序设计语言，它给了过程完全第一级状态。这就给有效实现提出了挑战，但由此所获得的描述能力却是极其惊人的。
;; -----------------------------------------------------------------------------------------------------------------
;; 请定义一人过程cubic，它和newton-method过程一起使用在下面形式的表达式里：(newton-method (cubic a b c) 1)，能逼近三次方程的零点
;; example 1.40 x^3+a*x^2+b*x+c的零点:需要使用牛顿变换将此函数变换成另一个函数，然后用求函数不动点的方法求此函数的零点
(define cubic
  (lambda (a b c)
    (lambda (x)
      (+ (* x x x) (* a (square x)) (* b x) c))))

(fixed-point-of-transform (cubic 2 -3 2) newton-transform 1.0 0.001)

(define inc
  (lambda (x)
    (+ x 1)))

;; 定义一个过程double，它以一个有一个参数的过程作为参数，double返回一个过程。这一过程将原来那个参数过程应用两次。
(define double
  (lambda (f)      ;; f是一个过程，这个过程会带一个参数
    (lambda (x)
      (f (f x)))))

(((double (double double)) inc) 5)

;; (double double) => (double (double x))
;; (double (double double)) => ((double (double (double (double x)))) inc) => 2 4 8 16
;; example 1.42令f和g是两个单参数的函数，f在g之后的复合定义为函数f(g(x))，请定义一个函数compose实现函数复合。
;; compose函数的作用，取两个函数，返回一个复合的函数。
(define compose
  (lambda (f g)
    (lambda (x)
      (f (g x)))))
;; 如果f是一个数值函数，n是一个正整数，那么我们可以构造出f的n次重复应用
(define repeated
  (lambda (f n)
    (if (= n 1)
	f
	(compose f (repeated f (- n 1))))))
;; 这里涉及到一个lambda函数，参数的替换等应用，形式化上的一点东西
;; 平滑一个函数的想法是信号处理中的一个重要概念。如果f是一个函数，dx是某个很小的数值，那么f的平滑也是一个函数，它在
;; 点x的值就是f(x-dx),f(x)和f(x+dx)的平均值。请写一个过程smooth，它的输入是一个计算f的过程，返回一个计算平滑后
;; 的f的过程。有时可能发现，重复地平滑一个函数，得到经过n次平滑的函数（也就是说，对平滑后的函数再做平滑）也很有价值
(define smooth
  (lambda (f)
    (lambda (x)
      (/ (+ (f (- x dx)) (f x) (f (+ x dx))) 3))))

;; 下面的两个计算过程，使得过程更加像是变量一样可传递的参数
(define smooth-n-times
  (lambda (f n)
    (if (= n 1)
	(smooth f)
	(smooth (smooth-n-times f (- n 1)))))) ;; 这是递归的计算过程

(define smooth-n-times
  (lambda (f n)
    (letrec
	((iter (lambda (i smoothed-f)
		 (if (= i 0)
		     smoothed-f
		     (iter (- i 1) (smooth smoothed-f))))))
      (iter n f))))

(define repeated-smooth
  (lambda (f n)
    (let ((n-times-smooth (repeated smooth n)))
      (n-times-smooth f))))

(define repeated-smooth
  (lambda (f n)
    ((repeated smooth n) f)))
;; 在1.3.3节里，我们看到企图用相互的方法去找y=x/y的不动点，以便计算平方根的方式不收敛，这个缺陷可以通过平均阻尼的
;; 方式弥补。同样方法也可用于找立方根，将它看做是平均阻尼后的y=x/y^2的不动点。遗憾的是，这一计算过程对于四次方根却
;; 行不通。，一次平均阻尼不足以使唤对y=x/y^3的不动点搜寻收敛。而在另一方面，如果我们求两次平均阻尼（即y=x/y^3的平
;; 均阻尼的平均阻尼），这一不动点搜寻就会收敛了。请做一些试验，考虑将计算n次方根作为y=x/y^(n-1)的反复做平均阻尼的
;; 不动点搜寻过程，请设法确定各种情况下需要做多少次平均阻尼。并基于这一认识，实现一个过程，它使用fixed-point,
;; average-damp和repeated过程计算n次方根
;; ******************************************************************************************
(define fixed-point
  (lambda (f guess tolerance)
    (letrec
	((fixed-point-iter (lambda (guess)
			     (let ((next (f guess)))
			       (let ()
				 (newline)
				 (display next)
				 (if (close-enough? next guess)
				     next
				     (fixed-point-iter next))))))
	 (close-enough? (lambda (next guess)
			  (< (abs (- next guess)) tolerance))))
      (fixed-point-iter guess))))

(define compose
  (lambda (f g)
    (lambda (x)
      (f (g x)))))
;; 如果f是一个数值函数，n是一个正整数，那么我们可以构造出f的n次重复应用
(define repeated
  (lambda (f n)
    (if (= n 1)
	f
	(compose f (repeated f (- n 1))))))

(define average-damp
  (lambda (f)
    (lambda (x)
      (average x (f x)))))    ;; 表达了平均阻尼技术的实现,利用平均阻尼技术来改定函数

(define average
  (lambda (m n)
    (/ (+ m n) 2)))

(define power
  (lambda (x n)
    (if (= n 1)
	x
	(* x (power x (- n 1))))))

(define sqrt-n-f     ;; 目的是生成一个函数,x/y^(n-1)
  (lambda (x n)
    (lambda (y)
      (/ x (power y (- n 1))))))

(define lg
  (lambda (n)
    (if (< (/ n 2) 2)
	1
	(+ 1 (lg (/ n 2))))))

(define sqrt-n
  (lambda (x n)             ;; 求x的n次方根
    (fixed-point ((repeated average-damp (lg n)) (sqrt-n-f x n))
		 1.0
		 0.001)))
;; 计算n次方根收敛所需的平均阻尼次数，是lgN（实验所得）
;; ********************************************************************************************
;; 本章描述的一些数值算法都是迭代式改进的实例。迭代式改进是一种非常具有一般性的计算策略，它说的是：为了计算出某些东西
;; 我们可以从对答案的某个初始猜测开始，检查这一猜测是否足够好，如果不行就改进这一猜测，将改进后的猜测作为新的猜测去继
;; 续这一计算过程。请写一个过程iterative-improve，它以两个过程为参数：其中之一表示告知某一猜测是否足够好的方法，另
;; 一个表示改进猜测的方法。iterative-improve返回值应该是一个过程，它以一个猜测为参数，通过不断改进，直至得到的猜测
;; 足够好为止。
(define iterative-improve
  (lambda (good-enough? improve-method)
    (lambda (init-guess)
      (letrec
	  ((iter-guess (lambda (guess)
			 (let ((nxt-guess (improve-method guess)))
			   (if (good-enough? guess nxt-guess)
			       nxg-guess
			       (iter-guess nxt-guess))))))
	(iter-guess init-guess)))))
