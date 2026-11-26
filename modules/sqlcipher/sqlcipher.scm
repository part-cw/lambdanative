#|
LambdaNative - a cross-platform Scheme framework
Copyright (c) 2009-2014, University of British Columbia
All rights reserved.

Redistribution and use in source and binary forms, with or
without modification, are permitted provided that the
following conditions are met:

* Redistributions of source code must retain the above
copyright notice, this list of conditions and the following
disclaimer.

* Redistributions in binary form must reproduce the above
copyright notice, this list of conditions and the following
disclaimer in the documentation and/or other materials
provided with the distribution.

* Neither the name of the University of British Columbia nor
the names of its contributors may be used to endorse or
promote products derived from this software without specific
prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR
CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
|#

;; minimal bindings for the sqlcipher database library

(define sqlcipher:debuglevel 0)
(define (sqlcipher:log level . x)
   (if (>= sqlcipher:debuglevel level) (apply log-system (append (list "sqlcipher: ") x))))

(c-declare  #<<end-of-c-declare
#ifndef SQLITE_HAS_CODEC
#define SQLITE_HAS_CODEC 1
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include <sqlcipher/sqlite3.h>

struct sqlite_db {
  sqlite3 *db;
  sqlite3_stmt *pst;
};

static struct sqlite_db *_sqlite_open(char *filename, char *key, int len)
{
  struct sqlite_db* d = (struct sqlite_db *)malloc(sizeof(struct sqlite_db));
  int err;
  if (!d) return 0;
  d->db = 0;
  d->pst = 0;
  err = sqlite3_open(filename, &d->db);
  if (err != SQLITE_OK) {
    if (d->db) sqlite3_close(d->db);
    free(d);
    return 0;
  }
  if (len > 0) {
    err = sqlite3_key(d->db,key,len);
    if (err != SQLITE_OK) {
      sqlite3_close(d->db);
      free(d);
      return 0;
    }
  }
  return d;
}

static void _sqlite_close(struct sqlite_db *d)
{
  sqlite3_close(d->db);
  free(d);
}

static int _sqlite_column_type(struct sqlite_db *d, int i)
{
  return sqlite3_column_type(d->pst, i);
}

static int _sqlite_column_integer(struct sqlite_db *d, int i)
{
  const unsigned char *field;
  field = sqlite3_column_text(d->pst, i);
  return atoi((char*)field);
}

static double _sqlite_column_float(struct sqlite_db *d, int i)
{
  const unsigned char *field;
  field = sqlite3_column_text(d->pst, i);
  return atof((char*)field);
}

static char *_sqlite_column_string(struct sqlite_db *d, int i)
{
  const unsigned char *field;
  field = sqlite3_column_text(d->pst, i);
  return (char*)field;
}

static int _sqlite_data_count(struct sqlite_db *d)
{
  return sqlite3_data_count(d->pst);
}

static int _sqlite_step(struct sqlite_db *d)
{
  return sqlite3_step(d->pst);
}

static int _sqlite_query(struct sqlite_db *d, char* sql)
{
  const char *sql2;
  return sqlite3_prepare_v2(d->db, sql, strlen(sql), &d->pst, &sql2);
}

static int _sqlite_finalize(struct sqlite_db *d)
{
  return sqlite3_finalize(d->pst);
}

end-of-c-declare
)

(define SQLCIPHER_ERROR ((c-lambda () int "___result = SQLITE_ERROR;")))
(define SQLCIPHER_OK ((c-lambda () int "___result = SQLITE_OK;")))
(define SQLCIPHER_INTEGER ((c-lambda () int "___result = SQLITE_INTEGER;")))
(define SQLCIPHER_FLOAT ((c-lambda () int "___result = SQLITE_FLOAT;")))
(define SQLCIPHER_ROW ((c-lambda () int "___result = SQLITE_ROW;")))

(define sqlcipher:open (c-lambda (char-string char-string int) (pointer void) "_sqlite_open"))
(define sqlcipher:close (c-lambda ((pointer void)) void "_sqlite_close"))
(define sqlcipher:column-type (c-lambda ((pointer void) int) int "_sqlite_column_type"))
(define sqlcipher:column-integer (c-lambda ((pointer void) int) int "_sqlite_column_integer"))
(define sqlcipher:column-float (c-lambda ((pointer void) int) double "_sqlite_column_float"))
(define sqlcipher:column-string (c-lambda ((pointer void) int) char-string "_sqlite_column_string"))
(define sqlcipher:data-count (c-lambda ((pointer void)) int "_sqlite_data_count"))
(define sqlcipher:step (c-lambda ((pointer void)) int "_sqlite_step"))
(define sqlcipher:query (c-lambda ((pointer void) char-string) int "_sqlite_query"))
(define sqlcipher:finalize (c-lambda ((pointer void)) int "_sqlite_finalize"))

(define (sqlcipher-open dbname . key)
  (sqlcipher:log 1 "sqlcipher-open " dbname)
  (if (fx= (length key) 1)
    (let ((skey (car key)))
      (sqlcipher:open dbname skey (string-length skey))
    )
    (sqlcipher:open dbname "" 0)
  ))

(define (sqlcipher-close db)
  (sqlcipher:log 1 "sqlcipher-close " db)
  (sqlcipher:close db))

(define (sqlcipher-query db q)
  (sqlcipher:log 1 "sqlcipher-query " db " " q)
  (if (fx= (sqlcipher:query db q) SQLCIPHER_OK)
     (let loop ((res '()))
        (if (not (fx= (sqlcipher:step db) SQLCIPHER_ROW)) (begin (sqlcipher:finalize db) res)
           (loop (append res (list
             (let ((n (sqlcipher:data-count db)))
               (let loop2 ((i 0)(col '()))
                  (if (fx= i n) col
                    (let* ((type (sqlcipher:column-type db i))
                           (data ((if (fx= type SQLCIPHER_INTEGER)
                                    sqlcipher:column-integer
                                    (if (fx= type SQLCIPHER_FLOAT)
                                      sqlcipher:column-float
                                      sqlcipher:column-string
                                    )) db i)))
                      (loop2 (fx+ i 1) (append col (list data)))))))))))) #f))

;; simple test
(define (sqlcipher:test filename)
  (let ((db (sqlcipher-open filename)))
    (sqlcipher-query db "create table tbl1(one varchar(10), two smallint)")
    (sqlcipher-query db "insert into tbl1 values('hello!',10)")
    (sqlcipher-query db "insert into tbl1 values('goodbye', 20)")
    (sqlcipher-query db "insert into tbl1 values('one more', 2.3)")
    (let ((res (sqlcipher-query db "select * from tbl1")))
      (sqlcipher-close db) res)))

;; Unit test
(unit-test "sqlcipher-rw" "Write and read an SQLcipher database"
  (lambda () (let* ((filename "LNtest_sqlcipher.sqlcipher")
		    (password "topsecret")
		    (passwordlen 9)
                    (res '(("hello!" 10) ("goodbye" 20) ("one more" 2.3)))
                    (db (sqlcipher-open filename password passwordlen)))
    (sqlcipher-query db "CREATE TABLE tbl1(one VARCHAR(10), two NUMERIC)")
    (sqlcipher-query db "INSERT INTO tbl1 VALUES('hello!',10)")
    (sqlcipher-query db "INSERT INTO tbl1 VALUES('goodbye', 20)")
    (sqlcipher-query db "INSERT INTO tbl1 VALUES('one more', 2.3)")
    (if (equal? (sqlcipher-query db "SELECT * FROM tbl1") res)
      (begin
        (sqlcipher-close db)
        (delete-file filename)
        #t
      )
      #f
    )
  )))

;; eof
