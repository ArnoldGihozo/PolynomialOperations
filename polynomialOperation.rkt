;; AUCSC 370
;; Scheme Polynomial Manipulation
;;
;;
;; A set of Scheme functions that operate on single-variable polynomials.
;;
;; Arnold Gihozo (gihozo@ualberta.ca)
;; Keegan Petreman (petreman@ualberta.ca)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; makePoly(ints) -> formatted list of pairs based on input ints
;;
;; If the provided list is empty or contains an odd number of values,
;; returns ((0 0)) by default (no polynomial). Otherwise, returns the
;; list of numbers but with every pair in its own list.   
;;
(define (makePoly ints)
  (cond
    ((or (equal? ints '()) (not (= (mod (length ints) 2) 0))) '((0 0)))
    (#t (makePolySub '() ints)))) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; makePolySub(poly leftoverInts) -> formatted list of pairs based on input ints
;;
;; Subroutine for makePoly. Pairs up numbers until the list is empty. 
;; poly contains the pairs already done, and leftoverInts are the ones that
;; have not been match yet.
;;
(define (makePolySub poly leftoverInts)
  (cond
    ((equal? leftoverInts '()) poly) 
    (#t (makePolySub (append poly (list (list (car leftoverInts) (cadr leftoverInts)))) (cddr leftoverInts)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; writePoly(poly) -> void
;;
;; Displays the provided polynomial made with makePoly through use of
;; the subroutines writePolyFirst (for the first term) and writePolySub
;; (for the rest of the terms).
;;
(define (writePoly poly)
  (cond
    ((or (equal? poly '((0 0))) (equal? poly '())) (display "0\n"))
    (#t (writePolyFirst (car poly)) (writePolySub (cdr poly)))))
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; writePolyFirst(poly) -> void
;;
;; Subroutine for writePoly. Used for displaying only the first term of 
;; a polynomial.
;;
;; The first term of the polynomial is considered separately from the rest
;; to ensure that if the first term is negative, the negative is put right
;; against the first term.
;;   
(define (writePolyFirst poly)
  (cond
    ((= (car poly) 0) (display "")) ;; covers the case where coefficient is 0 (returns immediately)
    (#t
     
      (cond
        ((= (car poly) 1) (display "")) ;; covers the case where the coefficient is 1 (don't print it)
        ((= (car poly) -1) (display "-")) ;; covers the case where the coefficient is -1 (just print "-")
        (#t (display (car poly))))
     
      (cond
        ((= (cadr poly) 0) (display "")) ;; covers the case where the exponent is 0 (don't put it or x)
        ((= (cadr poly) 1) (display "x")) ;; covers the case where the exponent is 1 (just display x)
        (#t
          (display "x^")
          (display (cadr poly)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; writePolySub(poly) -> void
;;
;; Subroutine for writePoly. Used for displaying every term except the
;; first of a polynomial.
;;
(define (writePolySub poly)
  (cond
    ((equal? poly '()) (display "\n")) ;; base condition (no more terms left)
    ((= (caar poly) 0) (writePolySub (cdr poly))) ;; covers the case where coefficient is 0 (skip to next term)
    (#t
  
      (cond
        ((< (caar poly) 0) (display " - "))
        (#t (display " + ")))
     
      (cond
        ((= (abs (caar poly)) 1) (display "")) ;; covers the case where the coefficient is 1 (don't print it)
        (#t (display (abs(caar poly)))))
     
      (cond
        ((= (cadar poly) 0) (display "")) ;; covers the case where the exponent is 0 (don't display x)
        ((= (cadar poly) 1) (display "x")) ;; covers the case where the exponent is 1 (just display x)
        (#t
          (display "x^")
          (display (cadar poly))))
        
      (writePolySub (cdr poly)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; addPolys(poly1 poly2) -> new poly that is sum of poly1 and poly2
;;
;; Add two polynomials together. Wrapper for addPolySub.
;; Checks first if either of the polynomials is empty or the zero polynomial:
;; if one is, returns the other. Otherwise, add them using addPolysSub.
;;
(define (addPolys poly1 poly2)
  (cond
    ((and (not (null? poly1)) (or (null? poly2) (equal? poly2 '((0 0))))) poly1)
    ((and (not (null? poly2)) (or (null? poly1) (equal? poly1 '((0 0))))) poly2)
    (#t (removeZeroCoeffs(addPolysSub poly1 poly2)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; addPolysSub(poly1 poly2) -> new poly that is sum of poly1 and poly2
;;
;; Subroutine for addPolys. Add two polynomials together.
;;
(define (addPolysSub poly1 poly2)
  (cond
    ((and (null? poly1) (null? poly2)) '())
    ((null? poly1) poly2)
    ((null? poly2) poly1)
    ((= (cadar poly1) (cadar poly2)) (addWorker poly1 poly2))
    ((> (cadar poly1) (cadar poly2)) (cons (car poly1) (addPolysSub (cdr poly1) poly2)))
    ((< (cadar poly1) (cadar poly2)) (cons (car poly2) (addPolysSub poly1 (cdr poly2))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; addWorker(poly1 poly2) -> list pair of first terms after adding
;;
;; Assumes the first two terms of the provided polys have equal exponents.
;; Then adds the two terms and puts them in their own list
;;
(define (addWorker poly1 poly2)
  (cons (cons (+ (caar poly1) (caar poly2)) (cons (cadar poly1) '())) (addPolysSub (cdr poly1) (cdr poly2))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; subPolys(poly1 poly2) -> new poly that is difference of poly1 and poly2
;;
;; Wrapper for subPolysSub so that the result can be wrapped with removeZeroCoeffs.
;;
(define (subPolys poly1 poly2)
  (removeZeroCoeffs (subPolysSub poly1 poly2 )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; subPolysSub(poly1 poly2) -> new poly that is difference of poly1 and poly2
;;
;; Subtracts two polynomials by using the function subWorker.
;;
(define (subPolysSub poly1 poly2)
  (cond
    ((and (null? poly1) (null? poly2)) '())
    ((null? poly1) poly2)
    ((null? poly2) poly1)
    ((equal? (cadar poly1) (cadar poly2)) (subWorker  poly1 poly2))
    ((> (cadar poly1) (cadar poly2)) (cons (car poly1) (subPolysSub  (cdr poly1) poly2)))
    ((< (cadar poly1) (cadar poly2)) (cons (flipTermSign (car poly2)) (subPolysSub  poly1 (cdr poly2))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; subWorker(poly1 poly2) -> list pair of first terms after subtraction
;;
;; Assumes the first two terms of the provided polys have equal exponents.
;; Then subtracts the two terms and puts them in their own list
;;
(define (subWorker poly1 poly2)
  (cons (cons (- (caar poly1) (caar poly2)) (cons (cadar poly1) '())) (subPolysSub (cdr poly1) (cdr poly2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; flipTermSign(poly) -> term but with opposite coefficent sign
;;
(define (flipTermSign term)
  (list (* (car term) -1) (cadr term)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; multPolys(poly1 poly2) -> new poly that is the product of poly1 and poly2
;;
;; Wrapper for multPolySub.
;; Takes two polynomials and multiplies them together through use of multPolysSub.
;; If either provided polynomial is an empty list, returns the zero polynomial.
;;
(define (multPolys poly1 poly2)
  (cond
    ((or (eq? poly1 '()) (eq? poly2 '())) '((0 0)))
    (#t (multPolysSub poly1 poly2 '()))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; multPolysSub(poly1 poly2 resultPoly) -> new poly that is the product of poly1 and poly2
;;
;; Multiples two polynomials together through recursion.
;;
;; Takes the front term of the first polynomial and multiplies it
;; against every term in the second polynomial through the use of
;; multPolySingle. This result is then added to the overall result with
;; addPolys, and multPolysSub is called again with this result, poly2,
;; and the cdr of poly1. After this process has been done for every term
;; in poly1, returns the overall result.
;;
(define (multPolysSub poly1 poly2 resultPoly)
  (cond
    ((equal? poly1 '()) (removeZeroCoeffs resultPoly))
    (#t (multPolysSub (cdr poly1) poly2 (addPolys resultPoly (multPolySingle (car poly1) poly2 '()))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; multPolySingle(scalar poly result) -> product of a monomial and the given poly
;;
;; Takes a single term of a polynomial and multiplies it against an
;; entire other polynomial through recursion. Returns the result after
;; every term in the polynomial has been multiplied by the single term.
;;
(define (multPolySingle scalar poly result)
  (cond
    ((equal? poly '()) result)
    (#t 
      (multPolySingle scalar (cdr poly) (append result 
        (list (list 
          (* (car scalar) (caar poly))
          (+ (cadr scalar) (cadar poly)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; evalPoly(integer poly) -> result of evaluating polynomial with x = integer
;;
;; Evaluates and returns the result of the polynomial using x = integer.
;;
(define (evalPoly integer poly)
  (cond
    ((equal? poly '()) '((0 0)))
    (#t (* (exponentEval integer (cadar poly)) (caar poly))) (evalPoly integer (cdr poly))))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; exponentEval(integer exponent) -> integer^exponent
;;
;; Returns integer^exponent. If exponent = 0, returns 1. If exponent = 1,
;; returns integer. Evaluates by using repeated multiplication.
;;
(define (exponentEval integer exponent)
  (cond
    ((eq? 0 exponent) 1)
    (#t (* integer (exponentEval integer (- exponent 1 ))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; diffPoly(poly) -> derivative of poly
;;
;; Takes a single polynomial as a parameter and returns a polynomial
;; which is its derivative.
;;
(define (diffPoly poly)
  (cond
    ((equal? poly '()) '((0 0)))
    (#t (diffPolySub poly '()))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; diffPolySub(poly) -> derivative of poly
;;
;; Recursive call for diffPoly. Differentiates each term in the polynomial
;; by using diffTerm until all are done. Then returns the differentiated
;; polynomial.
;;
(define (diffPolySub poly result)
  (cond
    ((equal? poly '()) (removeZeroCoeffs result))
    (#t (diffPolySub (cdr poly) (append result (list (diffTerm (car poly))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; diffTerm(term) -> derivative of the given term (or monomial)
;;   
;; Differentiates the given term and returns the result in a list.
;; 
(define (diffTerm term)
  (cond
    ((= (cadr term) 0) '(0 0))
    (#t (list (* (car term) (cadr term)) (- (cadr term) 1)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; removeZeroCoeffs(poly) -> poly with all terms with coeffecient = 0 removed
;;
;; If poly is the zero polynomial, returns itself (as per the assignment,
;; the zero polynomial is allowed to have a coefficient of 0). 
;; Otherwise removeZeroCoeffsSub is called to process the polynomial.
;;
(define (removeZeroCoeffs poly)
  (cond
    ((equal? poly '(0 0)) poly)
    (#t (removeZeroCoeffsSub poly '()))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; removeZeroCoeffsSub(poly) -> poly with all terms with coeffecient = 0 removed
;;
(define (removeZeroCoeffsSub poly result)
  (cond
    ((equal? poly '()) result)
    ((= (caar poly) 0) (removeZeroCoeffsSub (cdr poly) result))
    (#t (removeZeroCoeffsSub (cdr poly) (append result (list(car poly)))))))