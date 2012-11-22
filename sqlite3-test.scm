;; Gambit-c's sqlite3 binding. Test.
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

;; In order to run this test, ~~/syntax-case must be loaded
;; (see gambit manual)

(load "sqlite3")


;;; Utils

(define-syntax test
  (syntax-rules ()
    ((_ name exp res) (if (equal? exp res)
                          (print name ":\t\tpassed.\n")
                          (print name ":\t\tfailed.\n")))
    ((_ exp res) (if (equal? exp res)
                     (print 'exp "\t" "passed.\n")
                     (print 'exp ":\t\tfailed.\n")))
    ((_ a) (error "Bad formed test."))))

;;;

(define *dbname* "test.db")

(define (expected-promise query)
  (let ([p (open-process (list path: "/usr/bin/sqlite3"
			       arguments: (list *dbname*)))])
    (display query p)
    (newline p)
    (force-output p)
    (let ([res (read-line p)])
      (close-port p)
      res)))

(define (get-expected query)
  (delay (expected-promise query)))

(define (test-query query fn seed expected)
  (call-with-values
      (lambda () (sqlite3 *dbname*))
    (lambda (db-fold-left close)
      (let [(res (db-fold-left fn seed query))]
	(close)
	(test query res (force expected))))))

(define (run-tests)

  (let ([q "CREATE TABLE tb1 (c0 INTEGER, c1 TEXT, c2 REAL);"])
    (test-query q values q (get-expected ".schema tb1")))

  (test-query "INSERT INTO tb1 VALUES(1, 'one', 1.001);"
              values
              "1|one|1.001"
              (get-expected "select * from tb1;"))

  (let ([fn (lambda (seed c0 c1 c2)
              (values #t (with-output-to-string
                           seed
                           (lambda ()
                             (print c0 "|" c1 "|" c2)))))]) ;
    (test-query "SELECT * FROM tb1;" fn ""
                (get-expected "select * from tb1;"))))


(define (run)
  (if (file-exists? *dbname*)
    (delete-file *dbname*))

  (time (run-tests))

  (delete-file *dbname*))




