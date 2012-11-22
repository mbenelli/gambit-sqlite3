;; Gambit-c's sqlite3 binding. High-level interface.
;;
;; Copyright (C) 2008 Marco Benelli <mbenelli@yahoo.com>
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


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
