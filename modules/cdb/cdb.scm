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

;; wrapper for CDB, a very fast constant database
;; both keys and values can be arbitrary scheme code

(define cdb:debuglevel 0)
(define (cdb:log level . x)
   (if (>= cdb:debuglevel level) (apply log-system (append (list "cdb: ") x))))

(c-declare #<<c-declare-end

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <fcntl.h>

#include <cdb.h>

struct cdb_make *_make_cdb(char *fname)
{
  int fd = open(fname,O_RDWR|O_CREAT,0666);
  if (fd<0) return 0;
  struct cdb_make *cdbm = (struct cdb_make *)malloc(sizeof(struct cdb_make));
  if (cdbm) cdb_make_start(cdbm, fd);
  return cdbm;
}

int _cdb_make_merge(struct cdb_make *cdbm, char *fname)
{
  int tmpfd = open(fname,O_RDONLY);
  if (!tmpfd) return 0;
  struct cdb tmpcdb;
  if (cdb_init(&tmpcdb, tmpfd)<0) return 0;
  unsigned int cpos;
  cdb_seqinit(&cpos,&tmpcdb);
  while (cdb_seqnext(&cpos,&tmpcdb)>0) {
    int klen = cdb_keylen(&tmpcdb);
    char *key = (char*)malloc(klen);
    cdb_read(&tmpcdb, key, klen, cdb_keypos(&tmpcdb));
    int vlen = cdb_datalen(&tmpcdb);
    char *val = (char*)malloc(vlen);
    cdb_read(&tmpcdb, val, vlen, cdb_datapos(&tmpcdb));
    cdb_make_add(cdbm,key,klen,val,vlen);
    free(key);
    free(val);
  }
  cdb_free(&tmpcdb);
  close(tmpfd);
  return 1;
}

int _cdb_make_finish(struct cdb_make *cdbm)
{
  int fd = cdbm->cdb_fd;
  int res = cdb_make_finish(cdbm);
  if (fd) close(fd);
  return res;
} 

struct cdb *_init_cdb(char *fname)
{
  int fd = open(fname,O_RDONLY);
  if (fd<0) return 0;
  struct cdb *cdb = (struct cdb *)malloc(sizeof(struct cdb));
  if (cdb) cdb_init(cdb,fd);
  return cdb; 
}

void _cdb_finish(struct cdb *cdb)
{
  int fd = cdb->cdb_fd;
  if (fd) close(fd);
}

___SCMOBJ _cdb_index(struct cdb *cdb)
{
  ___SCMOBJ s,lst=___NUL,tmp;
  unsigned int cpos;
  cdb_seqinit(&cpos,cdb);
  while (cdb_seqnext(&cpos,cdb)>0) {
    int klen = cdb_keylen(cdb);
    char *key = (char*)malloc(klen);
    cdb_read(cdb, key, klen, cdb_keypos(cdb));
    s = ___EXT(___alloc_scmobj)(___sU8VECTOR, klen, ___STILL);
    memcpy(___BODY(s), key, klen);
    tmp = ___EXT(___make_pair)(s,lst,___STILL);
    ___EXT(___release_scmobj)(lst);
    lst=tmp;
    free(key);
  }
  return lst;
}

c-declare-end
)

;; encode/decode

(define cdb:encode object->u8vector)
(define cdb:decode u8vector->object)

(define (cdb-io enc dec)
  (set! cdb:encode enc)
  (set! cdb:decode dec))

;; create interface

(define CDB_PUT_ADD ((c-lambda () int "___result=CDB_PUT_ADD;")))
(define CDB_PUT_REPLACE ((c-lambda () int "___result=CDB_PUT_REPLACE;")))
(define CDB_PUT_INSERT ((c-lambda () int "___result=CDB_PUT_INSERT;")))

(define make-cdb (c-lambda (char-string) (pointer void) "_make_cdb"))

(define (cdb-make-add cdb key val)
  (cdb:log 2 "cdb-make-add " cdb  " " key " " val)
  (let ((u8key (cdb:encode key))
        (u8val (cdb:encode val)))
    (fx= 0 ((c-lambda ((pointer void) scheme-object int scheme-object int) int
       "___result=cdb_make_add(___arg1, 
           ___CAST(void*,___BODY_AS(___arg2,___tSUBTYPED)), ___arg3,
           ___CAST(void*,___BODY_AS(___arg4,___tSUBTYPED)), ___arg5);")
       cdb u8key (u8vector-length u8key) u8val (u8vector-length u8val)))))

(define (cdb-make-put cdb key val flag)
  (cdb:log 2 "cdb-make-put " cdb  " " key " " val " " flag)
  (let ((u8key (cdb:encode key))
        (u8val (cdb:encode val)))
    (fx= 0 ((c-lambda ((pointer void) scheme-object int scheme-object int int) int
       "___result=cdb_make_put(___arg1, 
           ___CAST(void*,___BODY_AS(___arg2,___tSUBTYPED)), ___arg3,
           ___CAST(void*,___BODY_AS(___arg4,___tSUBTYPED)), ___arg5, ___arg6);")
       cdb u8key (u8vector-length u8key) u8val (u8vector-length u8val) flag))))

(define (cdb-make-exists cdb key)
    (cdb:log 2 "cdb-make-exists " cdb  " " key)
    (let ((u8key (cdb:encode key)))
      (not (fx= ((c-lambda ((pointer void) scheme-object int) int
         "___result=cdb_make_exists(___arg1, ___CAST(void*,___BODY_AS(___arg2,___tSUBTYPED)), ___arg3);")
         cdb u8key (u8vector-length u8key)) 0))))

(define (cdb-make-merge cdb file)
  (cdb:log 2 "cdb-make-merge " cdb  " " file)
  (fx= 1 ((c-lambda ((pointer void) char-string) int "_cdb_make_merge") cdb file)))

(define cdb-make-finish (c-lambda ((pointer void)) int "_cdb_make_finish"))

;; query interface

(define init-cdb (c-lambda (char-string) (pointer void) "_init_cdb"))

(define cdb:find (c-lambda ((pointer void) scheme-object int) int  
  "___result=cdb_find(___arg1, ___CAST(void*,___BODY_AS(___arg2,___tSUBTYPED)), ___arg3);"))

(define cdb:datapos (c-lambda ((pointer void)) unsigned-int "___result=cdb_datapos((struct cdb*)___arg1);"))
(define cdb:datalen (c-lambda ((pointer void)) unsigned-int "___result=cdb_datalen((struct cdb*)___arg1);"))

(define cdb:read (c-lambda ((pointer void) scheme-object int unsigned-int) int
   "___result=cdb_read(___arg1, ___CAST(void*,___BODY_AS(___arg2,___tSUBTYPED)), ___arg3, ___arg4);"))

(define (cdb-find cdb key)
  (cdb:log 2 "cdb-find" cdb " " key)
  (let* ((u8key (cdb:encode key))
         (res (cdb:find cdb u8key (u8vector-length u8key)))
         (vpos (if (> res 0) (cdb:datapos cdb) #f))
         (vlen (if (> res 0) (cdb:datalen cdb) #f))
         (u8val (if vlen (make-u8vector vlen) #f)))
    (if u8val (begin (cdb:read cdb u8val vlen vpos) (cdb:decode u8val)) #f)))

(define (cdb-index cdb) 
  (cdb:log 2 "cdb-index " cdb)
  (map cdb:decode ((c-lambda ((pointer void)) scheme-object "_cdb_index") cdb)))

(define cdb-finish (c-lambda ((pointer void)) void "_cdb_finish"))

;; unit test

(unit-test "cdb" "constant database creation and lookup"
  (lambda ()
    (let* ((fname (string-append (system-directory) (system-pathseparator) "unittest.cdb")))
      (if (file-exists? fname) (delete-file fname))
      (let* ((keys (list "a" 'a '(test) (lambda (x) x) 1 0.5 1/2))
             (values (list '1 2 "three" 'four 5. '(six) (lambda (seven) 1)))
             (cdbm (make-cdb fname)))
        (let loop ((ks keys)(vs values))
          (if (> (length ks) 0) (begin
            (cdb-make-add cdbm (car ks) (car vs))
            (loop (cdr ks) (cdr vs)))))
        (cdb-make-finish cdbm)
        (let* ((cdb (init-cdb fname))
               (res (let loop ((ks keys)(vs values))
                  (if (fx= (length ks) 0) #t
                    (if (not (equal? (car vs) (cdb-find cdb (car ks))))  #f
                       (loop (cdr ks) (cdr vs)))))))
           (cdb-finish cdb)
           (if (file-exists? fname) (delete-file fname))
           res)))))

;; eof
