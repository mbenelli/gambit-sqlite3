;; Gambit-c's sqlite3 binding. Test.
;;
;; Copyright (C) 2008 Marco Benelli <mbenelli@yahoo.com>
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




