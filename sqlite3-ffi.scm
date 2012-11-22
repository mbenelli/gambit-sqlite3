;; Gambit-c's sqlite3 binding. Low-level interface.
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

