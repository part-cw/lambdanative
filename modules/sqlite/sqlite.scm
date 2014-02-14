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

;; minimal bindings for the sqlite database library

(define sqlite:debuglevel 0)
(define (sqlite:log level . x)
   (if (>= sqlite:debuglevel level) (apply log-system (append (list "sqlite: ") x))))

(c-declare  #<<end-of-c-declare

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include <sqlite3.h>

struct sqlite_db {
  sqlite3 *db;
  sqlite3_stmt *pst;
};

static struct sqlite_db *_sqlite_open(char *filename)
{
  struct sqlite_db* d  = (struct sqlite_db *)malloc(sizeof(struct sqlite_db));
  int err = sqlite3_open(filename, &d->db);
  if (err!=SQLITE_OK) { free(d); d=0; }
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

(define SQLITE_ERROR ((c-lambda () int "___result = SQLITE_ERROR;")))
(define SQLITE_OK ((c-lambda () int "___result = SQLITE_OK;")))
(define SQLITE_INTEGER ((c-lambda () int "___result = SQLITE_INTEGER;")))
(define SQLITE_FLOAT ((c-lambda () int "___result = SQLITE_FLOAT;")))
(define SQLITE_ROW ((c-lambda () int "___result = SQLITE_ROW;")))

(define sqlite:open (c-lambda (char-string) (pointer void) "_sqlite_open"))
(define sqlite:close (c-lambda ((pointer void)) void "_sqlite_close"))
(define sqlite:column-type (c-lambda ((pointer void) int) int "_sqlite_column_type"))
(define sqlite:column-integer (c-lambda ((pointer void) int) int "_sqlite_column_integer"))
(define sqlite:column-float (c-lambda ((pointer void) int) double "_sqlite_column_float"))
(define sqlite:column-string (c-lambda ((pointer void) int) char-string "_sqlite_column_string"))
(define sqlite:data-count (c-lambda ((pointer void)) int "_sqlite_data_count"))
(define sqlite:step (c-lambda ((pointer void)) int "_sqlite_step"))
(define sqlite:query (c-lambda ((pointer void) char-string) int "_sqlite_query"))
(define sqlite:finalize (c-lambda ((pointer void)) int "_sqlite_finalize"))

(define (sqlite-open dbname)
  (sqlite:log 1 "sqlite-open " dbname)
  (sqlite:open dbname))

(define (sqlite-close db)
  (sqlite:log 1 "sqlite-close " db)
  (sqlite:close db))

(define (sqlite-query db q)
  (sqlite:log 1 "sqlite-query " db " " q)
  (if (fx= (sqlite:query db q) SQLITE_OK)
     (let loop ((res '()))
        (if (not (fx= (sqlite:step db) SQLITE_ROW)) (begin (sqlite:finalize db) res)
           (loop (append res (list 
             (let ((n (sqlite:data-count db)))
               (let loop2 ((i 0)(col '()))
                  (if (fx= i n) col
                    (let* ((type (sqlite:column-type db i))
                           (data ((if (fx= type SQLITE_INTEGER) 
                                    sqlite:column-integer 
                                    (if (fx= type SQLITE_FLOAT) 
                                      sqlite:column-float 
                                      sqlite:column-string
                                    )) db i)))
                      (loop2 (fx+ i 1) (append col (list data)))))))))))) #f))

;; simple test
(define (sqlite:test filename)
  (let ((db (sqlite-open filename)))
    (sqlite-query db "create table tbl1(one varchar(10), two smallint)")
    (sqlite-query db "insert into tbl1 values('hello!',10)")
    (sqlite-query db "insert into tbl1 values('goodbye', 20)")
    (sqlite-query db "insert into tbl1 values('one more', 2.3)")
    (let ((res (sqlite-query db "select * from tbl1")))
      (sqlite-close db) res)))

;; eof
