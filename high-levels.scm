;; Find nth element of a list
(define (nth index collection)
  (cond ((= 0 index) (car collection))
        (else (nth (- index 1) (cdr collection)))))

;; Apply procedure to car and cadr of a list until one element remains
(define (reduce function collection)
  (define (loop collection)
    (cond  ((<= (length collection) 1)
            (if (eq? collection '()) collection (car collection)))
           (else (loop (cons (function (car collection) (cadr collection))
                             (cddr collection))))))
  (loop collection))

;; Collect all items into a list that satisfy the predicate
(define (filter predicate collection)
  (define (loop predicate collection results)
    (cond ((not (pair? collection)) results)
          (else (loop predicate (cdr collection)
                       (if (predicate (car collection))
                           (cons (car collection) results)
                           results)))))
  (loop predicate collection '()))

;; Find element that best satisfies the predicate (better? a b) a b)
(define (satisfies-best predicate collection)
  (reduce (lambda (a b) (if (predicate a b) a b)) collection))
    
;; Call function on each item in a list for side effects return no list
(define (mapc function collection)
  (cond ((not (eq? collection '()))
         (function (car collection))
         (mapc function (cdr collection)))))

;; Call function on each parameter for side effects
(define (do-parameters function . items)
  (mapc function items))

;; List contains item, predicate is comparison predicate
(define (contains? item predicate sequence)
  (cond ((not (pair? sequence)) #f)
        ((predicate (car sequence) item) #t)
        (else (contains? item predicate (cdr sequence)))))

;; See if a list contains a value, if so return number of occurances
(define (count item predicate sequence)
  (define (loop sequence counter)
    (if (not (pair? sequence)) 
        counter
        (loop (cdr sequence) 
              (if (predicate (car sequence) item) 
                  (+ counter 1) 
                  counter))))
  (loop sequence 0))

;; Find the average datum of a list, allows for general averaging
;  func-divide must allow an int for the 2nd parameter
;  func-add is the result of adding two datums
(define (mean funct-add func-divide sequence)
  (cond ((not(pair? sequence)) '())
        (else (func-divide (reduce funct-add sequence) 
                           (length sequence)))))
    
;; Interpolate between two datums where percent is 0.0 to 1.0 (a to b)
;  4 functions must be provided for higher order data
;  add is the function of adding two datums giving a datum
;  multiply is function that multiplies a datum and scaler giving a datum
;  subtract is the function that subtracts datum from datum giving a datum
(define (interpolate funct-add funct-multiply funct-subtract a b percent)
  (let ((diff (funct-subtract b a)))
    (funct-add a (funct-multiply diff percent))))

;; Join contents of two lists into one
(define (join-lists lis1 lis2)
  (define (process l1 l2)
    (cond ((not (pair? l1)) l2)                                                                  
          ((not (pair? l2)) l1)                                                             
          (else (process (cdr l1) (cons (car l1) l2)))))
  (cond ((not (pair? lis1)) lis2)
        ((not (pair? lis2)) lis1)
        (else (process (reverse lis1) lis2))))

;; Join the contents of many lists into one
(define (join-all-lists l . rest)
  (define (join lists)
    (if (= (length lists) 1)
        (car lists)
        (join (cons (join-lists (car lists) (cadr lists)) (cddr lists)))))
    (join (cons l rest)))