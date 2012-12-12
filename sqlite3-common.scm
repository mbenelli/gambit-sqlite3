;; Gambit-c's sqlite3 binding. High-level interface.
;;
;; Copyright (c) 2008, Marco Benelli <mbenelli@yahoo.com>
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;;
;;   Redistributions of source code must retain the above copyright
;;     notice, this list of conditions and the following disclaimer.
;;
;;   Redistributions in binary form must reproduce the above copyright
;;     notice, this list of conditions and the following disclaimer in
;;     the documentation and/or other materials provided with the
;;     distribution.
;;
;;   Neither the name of the author nor the names of its contributors
;;     may be used to endorse or promote products derived from this
;;     software without specific prior written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;; A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;; HOLDERS OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
;; INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
;; BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS
;; OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED
;; AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
;; LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY
;; WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;; POSSIBILITY OF SUCH DAMAGE.

(define sqlite3 #f)

(let ()

  ;;; Utils
  (define (iota n . opt)
    (let ([%iota (lambda (start count step)
		   (let loop ([i start] [res '()])
		     (if (= (length res) count)
			 (reverse res)
			 (loop (+ i step) (cons i res)))))])
      (cond
       [(null? opt)        (%iota 0 n 1)]
       [(= (length opt) 1) (%iota (car opt) n 1)]
       [(= (length opt) 2) (%iota (car opt) n (cadr opt))]
       [else               (error "iota: wrong argument number.")])))

  ;;; Constants 
  (define (sqlite-integer? x) (eq? x 1))
  (define (sqlite-float?   x) (eq? x 2))
  (define (sqlite-text?    x) (eq? x 3))
  (define (sqlite-blob?    x) (eq? x 4))
  (define (sqlite-null?    x) (eq? x 5))

  (define (sqlite-busy?    x) (eq? x 5))
  (define (sqlite-row?     x) (eq? x 100))
  (define (sqlite-done?    x) (eq? x 101))

  (define (open name)
    (let ([db (%sqlite3-open name)])
      (if (zero? (sqlite3-errcode db))
	  db
	  (raise (sqlite3-errmsg db)))))

  ;;; Result handling

  (define (process-row query ncol fn seed)
    (let ([get-item (lambda (i)
		      (let ([x (sqlite3-column-type query i)])
			(cond
			 [(sqlite-integer? x) (sqlite3-column-int    query i)]
			 [(sqlite-float? x)   (sqlite3-column-double query i)]
			 [(sqlite-text? x)    (sqlite3-column-text   query i)]
			 [(sqlite-blob? x)    (sqlite3-column-blob   query i)]
			 [(sqlite-null? x)    #f])))])
      (apply fn (cons seed (map get-item (iota ncol))))))


  ; Interface
  ; fn: seed col col ... -> continue? new-seed
  (define (db-fold-left db fn seed query)
    (let ([x (sqlite3-step query)])
      (cond
       [(sqlite-done? x) (sqlite3-finalize query) seed]
       [(sqlite-row? x) (let ([ncol (sqlite3-column-count query)])
			  (call-with-values
			      (lambda ()
				(process-row query ncol fn seed))
			    (lambda (continue? res)
			      (if (not continue?)
				  res
				  (db-fold-left db fn res query)))))]
       [(sqlite-busy? x) #f]
       [else (raise (sqlite3-errmsg db))])))


  (set! sqlite3
	(lambda ( name)
	  (let ([db (open name)])
	    (values (lambda (fn seed query)
		      (db-fold-left db fn seed (%sqlite3-prepare-v2 db query)))
		    (lambda ()
		      (sqlite3-close db))))))
)
