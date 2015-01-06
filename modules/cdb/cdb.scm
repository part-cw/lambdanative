#|
LambdaNative - a cross-platform Scheme framework
Copyright (c) 2009-2015, University of British Columbia
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
#include <unistd.h>

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
  if (cdb) {
    int res = cdb_init(cdb,fd);
    if (res<0) { free(cdb); return 0; } else return cdb; 
  } else {
    return 0;
  }
}

void _cdb_finish(struct cdb *cdb)
{
  int fd = cdb->cdb_fd;
  if (fd) close(fd);
}

static struct cdb_find cdbf;

c-declare-end
)

;; encode/decode

(define cdb:makectx rabbit-make)

(define cdb:destroyctx rabbit-destroy)

(define (cdb:encoder keyctx)
  (lambda (obj) (rabbit-encode keyctx (object->u8vector obj))))

(define (cdb:decoder keyctx)
  (lambda (u8v) (with-exception-handler (lambda (e) 
    (log-error "cdb:decoder: failed to deserialize: " (exception->string e)) #f)
      (lambda () (u8vector->object (rabbit-decode keyctx u8v))))))

;; create interface

(define CDB_PUT_ADD ((c-lambda () int "___result=CDB_PUT_ADD;")))
(define CDB_PUT_REPLACE ((c-lambda () int "___result=CDB_PUT_REPLACE;")))
(define CDB_PUT_INSERT ((c-lambda () int "___result=CDB_PUT_INSERT;")))

(define (make-cdb fname . key)
  (let ((ctx (if (null? key) #f (cdb:makectx (car key)))))
    (list ((c-lambda (char-string) (pointer void) "_make_cdb") fname)
      (if (not ctx) object->u8vector (cdb:encoder ctx)) ctx
     )))

(define (cdb-make-add cdb key val)
  (cdb:log 2 "cdb-make-add " cdb  " " key " " val)
  (let* ((cdb:ptr (car cdb))
         (cdb:encode (cadr cdb))
         (u8key (cdb:encode key))
         (u8val (cdb:encode val)))
    (fx= 0 ((c-lambda ((pointer void) scheme-object int scheme-object int) int
       "___result=cdb_make_add(___arg1, 
           ___CAST(void*,___BODY_AS(___arg2,___tSUBTYPED)), ___arg3,
           ___CAST(void*,___BODY_AS(___arg4,___tSUBTYPED)), ___arg5);")
       cdb:ptr u8key (u8vector-length u8key) u8val (u8vector-length u8val)))))

(define (cdb-make-put cdb key val flag)
  (cdb:log 2 "cdb-make-put " cdb  " " key " " val " " flag)
  (let* ((cdb:ptr (car cdb))
         (cdb:encode (cadr cdb))
         (u8key (cdb:encode key))
         (u8val (cdb:encode val)))
    (fx= 0 ((c-lambda ((pointer void) scheme-object int scheme-object int int) int
       "___result=cdb_make_put(___arg1, 
           ___CAST(void*,___BODY_AS(___arg2,___tSUBTYPED)), ___arg3,
           ___CAST(void*,___BODY_AS(___arg4,___tSUBTYPED)), ___arg5, ___arg6);")
       cdb:ptr u8key (u8vector-length u8key) u8val (u8vector-length u8val) flag))))

(define (cdb-make-exists cdb key)
    (cdb:log 2 "cdb-make-exists " cdb  " " key)
    (let* ((cdb:ptr (car cdb))
           (cdb:encode (cadr cdb))
           (u8key (cdb:encode key)))
      (not (fx= ((c-lambda ((pointer void) scheme-object int) int
         "___result=cdb_make_exists(___arg1, ___CAST(void*,___BODY_AS(___arg2,___tSUBTYPED)), ___arg3);")
         cdb:ptr u8key (u8vector-length u8key)) 0))))

(define (cdb-make-merge cdb file)
  (cdb:log 2 "cdb-make-merge " cdb  " " file)
  (fx= 1 ((c-lambda ((pointer void) char-string) int "_cdb_make_merge") (car cdb) file)))

(define (cdb-make-finish cdb) 
  (let ((ctx (caddr cdb)))
    ((c-lambda ((pointer void)) int "_cdb_make_finish") (car cdb))
    (if ctx (cdb:destroyctx ctx))))

;; query interface

(define (init-cdb fname . key)
  (let ((ctx (if (null? key) #f (cdb:makectx (car key)))))
    (list ((c-lambda (char-string) (pointer void) "_init_cdb") fname)
      (if (not ctx) object->u8vector (cdb:encoder ctx)) 
      (if (not ctx) u8vector->object (cdb:decoder ctx)) 
      ctx
   )))

(define cdb:find (c-lambda ((pointer void) scheme-object int) int  
  "___result=cdb_find(___arg1, ___CAST(void*,___BODY_AS(___arg2,___tSUBTYPED)), ___arg3);"))

(define cdb:findinit (c-lambda ((pointer void) scheme-object int) int
  "___result=cdb_findinit(&cdbf,___arg1, ___CAST(void*,___BODY_AS(___arg2,___tSUBTYPED)), ___arg3);"))

(define cdb:findnext (c-lambda () int "___result=cdb_findnext(&cdbf);"))

(define cdb:datapos (c-lambda ((pointer void)) unsigned-int "___result=cdb_datapos((struct cdb*)___arg1);"))
(define cdb:datalen (c-lambda ((pointer void)) unsigned-int "___result=cdb_datalen((struct cdb*)___arg1);"))

(define cdb:keypos (c-lambda ((pointer void)) unsigned-int "___result=cdb_keypos((struct cdb*)___arg1);"))
(define cdb:keylen (c-lambda ((pointer void)) unsigned-int "___result=cdb_keylen((struct cdb*)___arg1);"))

(define cdb:read (c-lambda ((pointer void) scheme-object int unsigned-int) int
   "___result=cdb_read(___arg1, ___CAST(void*,___BODY_AS(___arg2,___tSUBTYPED)), ___arg3, ___arg4);"))

(define (cdb-find cdb key)
  (cdb:log 2 "cdb-find" cdb " " key)
  (let* ((cdb:ptr (car cdb))
         (cdb:encode (cadr cdb))
         (cdb:decode (caddr cdb))
         (u8key (cdb:encode key))
         (res (begin (cdb:findinit cdb:ptr u8key (u8vector-length u8key)) (cdb:findnext)))
         (vpos (if (> res 0) (cdb:datapos cdb:ptr) #f))
         (vlen (if (> res 0) (cdb:datalen cdb:ptr) #f))
         (u8val (if vlen (make-u8vector vlen) #f)))
    (if u8val (begin (cdb:read cdb:ptr u8val vlen vpos) (cdb:decode u8val)) #f)))

(define (cdb-findnext cdb)
  (cdb:log 2 "cdb-findnext" cdb)
  (let* ((cdb:ptr (car cdb))
         (cdb:decode (caddr cdb))
         (res (cdb:findnext))
         (vpos (if (> res 0) (cdb:datapos cdb:ptr) #f))
         (vlen (if (> res 0) (cdb:datalen cdb:ptr) #f))
         (u8val (if vlen (make-u8vector vlen) #f)))
    (if u8val (begin (cdb:read cdb:ptr u8val vlen vpos) (cdb:decode u8val)) #f)))

(define cdb:seqinit (c-lambda (scheme-object (pointer void)) int
  "___result=cdb_seqinit(___CAST(int*,___BODY_AS(___arg1,___tSUBTYPED)), ___arg2);"))

(define cdb:seqnext (c-lambda (scheme-object (pointer void)) int
  "___result=cdb_seqnext(___CAST(int*,___BODY_AS(___arg1,___tSUBTYPED)), ___arg2);"))

(define (cdb-index cdb)
  (cdb:log 2 "cdb-index " cdb)
  (let* ((cdb:ptr (car cdb))
         (cdb:decode (caddr cdb))
         (tmp (u32vector 0)))
    (cdb:seqinit tmp cdb:ptr)
    (let loop ((idx '()))
      (let* ((res (cdb:seqnext tmp cdb:ptr))
             (kpos (if (> res 0) (cdb:keypos cdb:ptr) #f))
             (klen (if (> res 0) (cdb:keylen cdb:ptr) #f))
             (u8key (if klen (make-u8vector klen) #f)))
         (if (= res 0) idx
           (loop (append idx (list 
             (begin (cdb:read cdb:ptr u8key klen kpos) (cdb:decode u8key))))))))
   ))

(define (cdb->table cdbfile . key)
  (cdb:log 2 "cdb->table " cdbfile " " key)
  (if (not (file-exists? cdbfile)) #f
    (let* ((cdb (apply init-cdb (append (list cdbfile) key)))
           (cdb:ptr (car cdb))
           (cdb:decode (caddr cdb))
           (tmp (u32vector 0))
           (t (make-table)))
      (if cdb:ptr (begin
      (cdb:seqinit tmp cdb:ptr)
      (let ((fnl (let loop ()
        (let* ((res (cdb:seqnext tmp cdb:ptr))
              (kpos (if (> res 0) (cdb:keypos cdb:ptr) #f))
              (klen (if (> res 0) (cdb:keylen cdb:ptr) #f))
              (vpos (if (> res 0) (cdb:datapos cdb:ptr) #f))
              (vlen (if (> res 0) (cdb:datalen cdb:ptr) #f))
              (u8val (if vlen (make-u8vector vlen) #f))
              (u8key (if klen (make-u8vector klen) #f)))
           (if (= res 0) t (begin
              (cdb:read cdb:ptr u8key klen kpos) 
              (cdb:read cdb:ptr u8val vlen vpos) 
              (table-set! t (cdb:decode u8key) (cdb:decode u8val))
              (loop)))))))
       (cdb-finish cdb) fnl
      )) #f)
   )))
            
(define (table->cdb t cdbfile . key)
  (cdb:log 2 "table->cdb " t " " cdbfile " " key)
  (let ((cdbm (apply make-cdb (append (list cdbfile) key))))
    (table-for-each (lambda (k v) (cdb-make-add cdbm k v)) t)
    (cdb-make-finish cdbm) #t))

(define (cdb-finish cdb) 
  (let ((ctx (cadddr cdb)))
    ((c-lambda ((pointer void)) void "_cdb_finish") (car cdb))
    (if ctx (cdb:destroyctx ctx))))

;; unit test

(unit-test "cdb" "constant database creation and lookup"
  (lambda ()
    (let* ((fname (string-append (system-directory) (system-pathseparator) "unittest.cdb")))
      (if (file-exists? fname) (delete-file fname))
      (let* ((keys (list "a" 'a '(test) (lambda (x) x) 1 0.5 1/2))
             (values (list '1 2 "three" 'four 5. '(six) (lambda (seven) 1)))
             (cryptokey (random-u8vector 24))
             (cdbm (make-cdb fname cryptokey)))
        (let loop ((ks keys)(vs values))
          (if (> (length ks) 0) (begin
            (cdb-make-add cdbm (car ks) (car vs))
            (loop (cdr ks) (cdr vs)))))
        (cdb-make-finish cdbm)
        (let* ((cdb (init-cdb fname cryptokey))
               (res (let loop ((ks keys)(vs values))
                  (if (fx= (length ks) 0) #t
                    (if (not (equal? (car vs) (cdb-find cdb (car ks))))  #f
                       (loop (cdr ks) (cdr vs)))))))
           (cdb-finish cdb)
           (if (file-exists? fname) (delete-file fname))
           res)))))

;; eof
