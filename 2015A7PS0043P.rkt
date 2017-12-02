#lang racket
;;(file->lines "C:/Users/aayush/Desktop/BITS 3 1/Principles of Programming Language/project/resources/t0.in")
;;(map (lambda (str) (regexp-split #px" " str)) (file->lines "C:/Users/aayush/Desktop/BITS 3 1/Principles of Programming Language/project/resources/t0.in"))
(define (readfile filename) (file->lines filename))
(define (strlist input) (map (lambda (str) (regexp-split #px" " str)) input))
(define (strtonum input) (map (lambda(str) (string->number str)) input))
(define (numlist input) (map strtonum input))

(define precision '6)

(define (mysetprecision n p)
  (if (= n +inf.0) +inf.0
      (string->number (~r n #:precision p))
  )
) 

 (define (precision_util lst)
  (if (null? lst) '()
      (cons (list (car(car lst)) (mysetprecision (car(cdr(car lst))) precision))  (precision_util (cdr lst))))
)

(define (modify_precision lst)
  (if (null? lst) '()
  (cons (precision_util (car lst)) (modify_precision (cdr lst))))
)
;;(define (s1 filename) (map FUNCTION (cdr (numlist(strlist(readfile filename))))))
;; REMEMBER TO PASS cdr and count = 0
;; (func1 (cdr(numlist(strlist(readfile "C:/Users/aayush/Desktop/BITS 3 1/Principles of Programming Language/project/resources/t0.in")))))
(define (func1 ls [count 0])
  (if (null? ls)
      '()
      (append (list (cons (+ count 1) (list (car ls)))) (func1 (cdr ls) (+ count 1)))
  )
)
(define input (numlist (strlist (readfile "C:/Users/aayush/Desktop/BITS 3 1/Principles of Programming Language/project/resources/t0.in"))))
; Parameters
(define parameters (car input)) 
(define k (list-ref parameters 2))
(define e (list-ref parameters 3))
(define minpts (list-ref parameters 4))
(define step1 (func1 (cdr input)))
;; (display (step1 "C:/Users/aayush/Desktop/BITS 3 1/Principles of Programming Language/project/resources/t0.in")) WORKING!!

(define (dist pt1 pt2)
  (if (null? pt1)
       0
       (+ (expt (- (car pt1) (car pt2)) 2) (dist (cdr pt1) (cdr pt2)))
  )
)
(define (distance pt1 pt2)
  (if (equal? (car pt1) (car pt2))
      +inf.0 
      (sqrt(dist (cadr pt1) (cadr pt2)))
  )
)
(define (similar iterlist [matrix '()])
  (map (lambda (x) (append (ptdist x iterlist) matrix)) iterlist))
(define (ptdist pt ptlist [distlist '()])
  (map (lambda (x) (append (cons (car x) (list (distance pt x))) distlist)) ptlist))
      
(define step2 (modify_precision(similar step1)))
;;(define (distsort list)
;;  (map (lambda (x) (sort x (lambda (x y) (< (cadr x) (cadr y))))) list))
(define (distsort list)
  (map (lambda (x) (sort x (lambda (x y) (if (equal? (cadr x) (cadr y)) (< (car x) (car y)) (< (cadr x) (cadr y)))))) list))
(define (knn ls x)
  (map (lambda (i) (extractknn i x)) ls))
(define (extractknn ls x)
  (if (equal? x 0)
      '()
      (append (list (car (car ls))) (extractknn (cdr ls) (- x 1)))
  )
)
(define (ptsort list)
  (map (lambda (x) (sort x (lambda (x y) (< x y)))) list))

;;Step 3
(define step3 (ptsort (knn (distsort step2) k)))


;(define (vertices list)
;  (map (lambda (x) (nbour (list-ref list (index-of list x)) (+ (index-of list x) 1) list)) list))

(define (vertices iter list [index 0])
  (if (null? iter)
      '()
      (cons (nbour (car iter) (+ index 1) list) (vertices (cdr iter) list (+ index 1)))
  )
)

(define (bin_member x ls)
  (if (equal? (member x ls) #f) #f #t))

(define (nbour knnls i ls)
  (map (lambda (x) (if (equal? (bin_member i (list-ref ls (- x 1))) #t) (cons x (list (wt knnls (list-ref ls (- x 1))))) null)) knnls))

; sorts each list (in list of lists) based on edge weights and vertex index after removing empty list elements within each list 
(define (nbour-sort list)
  (map (lambda (x) (sort (remove-null x) (lambda (x y) (if (equal? (cadr x) (cadr y)) (< (car x) (car y)) (> (cadr x) (cadr y)))))) list))

; calculates edge weight based on common vertices in knn list
(define (wt a b)
  (length (set-intersect a b)))

; removes empty (sub)list elements from a list
(define (remove-null ls)
  (if (null? ls)
      '()
      (if (null? (car ls))
          (remove-null (cdr ls))
          (cons (car ls) (remove-null (cdr ls)))
      )
  ))

; step4 output
(define step4 (nbour-sort (vertices step3 step3)))

(define (pts-density list)
  (map density list))

(define (density list)
  (if (null? list)
      0
      (if (>= (cadr (car list)) e)
          (+ 1 (density (cdr list)))
          (+ 0 (density (cdr list)))
      )
   ))

; step5 output
(define step5 (pts-density step4))

(define (core-pts list [index 1])
  (if (null? list)
      '()
      (if (>= (car list) minpts)
          (cons index (core-pts (cdr list) (+ index 1)))
          (core-pts (cdr list) (+ index 1))
      )
  ))

; step6 output
(define step6 (core-pts step5))

(define (core-nbours pt corepts graph)
  (if (null? corepts)
      '()
      (if (>= (ret-wt (car corepts) (list-ref graph (- pt 1))) e)
          (cons (car corepts) (core-nbours pt (cdr corepts) graph))
          (core-nbours pt (cdr corepts) graph)
      )
  ))
 
(define (check-append ls res checkls)
  (if (null? ls)
      res
      (if (bin_member (car ls) checkls)
          (check-append (cdr ls) res checkls)
          (cons (car ls) (check-append (cdr ls) res checkls))
      )
  ))

(define (ret-wt pt ls)
  (if (null? ls)
      0
      (if (equal? pt (car (car ls)))
          (cadr (car ls))
          (ret-wt pt (cdr ls))
      )
  ))
(define (make-cluster explore corepts graph visited)
  (if (null? explore)
      (sort visited <)
      (if (bin_member (car explore) visited)
          (make-cluster (cdr explore) corepts graph visited)
          (make-cluster (check-append (core-nbours (car explore) corepts graph) (cdr explore) (cons (car explore) visited)) corepts graph (cons (car explore) visited))
      )
  ))

(define (search-clusters pt ls)
  (if (null? ls)
      #f
      (if (bin_member pt (cadr (car ls)))
          #t
          (search-clusters pt (cdr ls))
      )
  ))
(define (all-clusters iter-core core-pts graph [index 1] [clusterls '()])
  (if (null? iter-core)
      clusterls
      (if (search-clusters (car iter-core) clusterls)
          (all-clusters (cdr iter-core) core-pts graph index clusterls)
          (all-clusters (cdr iter-core) core-pts graph (+ index 1) (append (list (cons index (list (make-cluster (list (car iter-core)) core-pts graph '() )))) clusterls))
      )
  ))
(define (sort-clusters ls)
  (sort ls (lambda (x y) (< (car x) (car y)))))

; step7 output
(define step7 (sort-clusters (all-clusters step6 step6 step4)))
#|
(define (noise_nbours pt iter-nbours graph)
  (if (null? iter-nbours)
      0
      (if (>= (ret-wt (car iter-nbours) (list-ref graph (- pt 1))) e)
          (+ (noise_nbours pt (cdr iter-nbours) graph) 1)
          (+ (noise_nbours pt (cdr iter-nbours) graph) 0)
      )
  ))
|#
(define (list-pts list)
  (if (null? list)
      '()
      (cons (car (car list)) (list-pts (cdr list)))
  ))
(define (noise-pts iter-pts core-pts clusterls pts-dens)
  (if (null? iter-pts)
      '()
      (if (and (and (not(bin_member (car iter-pts) core-pts)) (not (search-clusters (car iter-pts) clusterls))) (equal? (list-ref pts-dens (- (car iter-pts) 1)) 0))
          (cons (car iter-pts) (noise-pts (cdr iter-pts) core-pts clusterls pts-dens))
          (noise-pts (cdr iter-pts) core-pts clusterls pts-dens)
      )
  ))

; step8 output
(define step8 (noise-pts (list-pts step1) step6 step7 step5))

(define (border-pts iter-pts clusterls noise-pts)
  (if (null? iter-pts)
      '()
      (if (and (not (search-clusters (car iter-pts) clusterls)) (not (bin_member (car iter-pts) noise-pts)))
          (cons (car iter-pts) (border-pts (cdr iter-pts) clusterls noise-pts))
          (border-pts (cdr iter-pts) clusterls noise-pts)
      )
  ))

; step9 output
(define step9 (border-pts (list-pts step1) step7 step8))

;;Print

(display step1)
(newline)
(display step2)
(newline)
(display step3)
(newline)
(display step4)
(newline)
(display step5)
(newline)
(display step6)
(newline)
(display step7)
(newline)
(display step8)
(newline)
(display step9)
(newline)
