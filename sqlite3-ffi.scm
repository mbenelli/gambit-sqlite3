;; Gambit-c's sqlite3 binding. Low-level interface.
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

(c-declare #<<C-END

#include <sqlite3.h>

C-END
)

;;; Types

(c-define-type void* (pointer void))
(c-define-type char* char-string)
(c-define-type char** (pointer char-string))

(c-define-type sqlite3 (struct "sqlite3"))
(c-define-type sqlite3* (pointer sqlite3))
(c-define-type sqlite3** (pointer sqlite3*))
(c-define-type sqlite3-stmt (struct "sqlite3_stmt"))
(c-define-type sqlite3-stmt* (pointer sqlite3-stmt))
(c-define-type sqlite3-stmt** (pointer sqlite3-stmt*))

;;; Error messages

(define sqlite3-errcode
  (c-lambda (sqlite3*) int "sqlite3_errcode"))

(define sqlite3-errmsg
  (c-lambda (sqlite3*) char* "sqlite3_errmsg"))

;;; Database connection

(define sqlite3-open
  (c-lambda (char* sqlite3**) int "sqlite3_open"))

(define sqlite3-close
  (c-lambda (sqlite3*) int "sqlite3_close"))

;;; Executing SQL

(define sqlite3-prepare-v2
  (c-lambda (sqlite3* char* int sqlite3-stmt** char**) int
            "sqlite3_prepare_v2"))

(define sqlite3-finalize
  (c-lambda (sqlite3-stmt*) int "sqlite3_finalize"))

(define sqlite3-reset
  (c-lambda (sqlite3-stmt*) int "sqlite3_reset"))

(define sqlite3-step
  (c-lambda (sqlite3-stmt*) int "sqlite3_step"))

(define sqlite3-column-count
  (c-lambda (sqlite3-stmt*) int "sqlite3_column_count"))

(define sqlite3-column-type
  (c-lambda (sqlite3-stmt* int) int "sqlite3_column_type"))

(define sqlite3-column-int
  (c-lambda (sqlite3-stmt* int) int "sqlite3_column_int"))

(define sqlite3-column-double
  (c-lambda (sqlite3-stmt* int) double "sqlite3_column_double"))

(define sqlite3-column-text
  (c-lambda (sqlite3-stmt* int) char* "sqlite3_column_text"))

(define sqlite3-column-blob
  (c-lambda (sqlite3-stmt* int) void* "sqlite3_column_blob"))

;;; Following functions have a modified API

(define %sqlite3-open
  (c-lambda (char-string) sqlite3*
#<<C-END
  sqlite3* db;
  int res = sqlite3_open(___arg1, &db);
  ___result_voidstar = db;
C-END
))

(define %sqlite3-prepare-v2
  (c-lambda (sqlite3* char*) sqlite3-stmt*
#<<C-END
  sqlite3_stmt *stmt;
  const char *rest;
  int res = sqlite3_prepare_v2(___arg1, ___arg2, -1, &stmt, &rest);
  ___result_voidstar = stmt;
C-END
))

