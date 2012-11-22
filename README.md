SQLITE3 INTERFACE FOR GAMBIT-C
==============================

Disclaimer
----------

This is a lightweight sqlite3 interface for gambit-c.
This work for me. It has been tested only in a few number of use-case.

Installation
------------

    make

There may be need of some customization in (the very simple) Makefile.

    make clean

Remove all .o* and .c files (usefull only in development)

A couple of test are contained in sqlite3-test.scm.
In order to run test "~~/syntax-case" must be loaded.

Low-level layer
---------------

This is a wrapper of a minimal subset of sqlite3 functions.
Scheme procedures are named following the usual convention of substituting "_"
with "-".
There are also a couple of convenience functions that could came in handle
in avoiding output variables or extra arguments.
All these functions are listed in sqlite3-ffi.scm.

High-level layer
----------------

The high-level layer is made of a single function (sqlite3).
This function take a string (name of database file) and return two functions:
a left-fold enumerator and a function to close database.

The left-fold enumerator is inspired from the Oleg Kiselyov [1] and has
following signature:

function initial-seed query -> result

where fn must have signature:

seed col col ... -> continue? new-seed

"fn" can stop computation by returning #f as first value.
If first value is always true, computation continue.

Example
-------

    (call-with-values
        (lambda () (sqlite3 "test.db"))
      (lambda (db-fold-left close)
        (let* ((fn (lambda (seed c0 c1 c2)
                   (values #t (+ seed (* c0 c1 c2))))))
          (display (db-fold-left fn 0 "SELECT * FROM test-table"))
          (close))))

Contact
-------

Marco Benelli <mbenelli@yahoo.com>


[1] http://okmij.org/ftp/Scheme/#databases
