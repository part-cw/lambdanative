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

;; wrapper for lmdb, fast key value database

(define lmdb:debuglevel 0)
(define (lmdb:log level . x)
   (if (>= lmdb:debuglevel level) (apply log-system (append (list "lmdb: ") x))))

(c-declare #<<c-declare-end

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <lmdb.h>

struct _mdb {
  MDB_env *env;
  MDB_dbi dbi;
  MDB_val key, value;
  MDB_txn *txn;
  MDB_cursor *cursor;
};

struct _mdb *_mdb_init(char *fname)
{
  int rc;
  struct _mdb *m = (struct _mdb *)malloc(sizeof(struct _mdb));
  rc = mdb_env_create(&m->env);
  rc = mdb_env_open(m->env, fname, 0, 0664);
  rc = mdb_txn_begin(m->env, NULL, 0, &m->txn);
  rc = mdb_open(m->txn, NULL, 0, &m->dbi);
  m->cursor=NULL;
  return m;
}

int _mdb_write(struct _mdb *m, unsigned char *k, int klen, unsigned char *v, int vlen)
{
  int rc;
  m->key.mv_size = klen;
  m->key.mv_data = k;
  m->value.mv_size = vlen;
  m->value.mv_data = v;
  rc = mdb_put(m->txn, m->dbi, &m->key, &m->value, 0);
  if (!rc) { 
    rc = mdb_txn_commit(m->txn);
    rc = mdb_txn_begin(m->env, NULL, 0, &m->txn);
  }
  return rc;
}

int _mdb_read(struct _mdb *m, unsigned char *k, int klen)
{
  int rc;
  m->key.mv_size = klen;
  m->key.mv_data = k;
  rc = mdb_get(m->txn,m->dbi,&m->key, &m->value);
  return rc;
}

int _mdb_index_first(struct _mdb *m)
{
  int rc;
  if (m->cursor) { mdb_cursor_close(m->cursor); }
  rc = mdb_cursor_open(m->txn, m->dbi, &m->cursor);
  return rc;
}

int _mdb_index_next(struct _mdb *m)
{
  int rc;
  rc = mdb_cursor_get(m->cursor, &m->key, &m->value, MDB_NEXT);
  return rc;
}

int _mdb_del(struct _mdb *m, unsigned char *k, int klen)
{
  int rc;
  m->key.mv_size = klen;
  m->key.mv_data = k;
  rc = mdb_del(m->txn, m->dbi, &m->key, &m->value);
  if (!rc) { rc = mdb_txn_commit(m->txn); }
  return rc;
}

int _mdb_key_len(struct _mdb *m) { return m->key.mv_size; }
void _mdb_key(struct _mdb *m, unsigned char *buf) { memcpy(buf,m->key.mv_data,m->key.mv_size); }
int _mdb_value_len(struct _mdb *m) { return m->value.mv_size; }
void _mdb_value(struct _mdb *m, unsigned char *buf) { memcpy(buf,m->value.mv_data,m->value.mv_size); }

void _mdb_cleanup(struct _mdb *m)
{
  mdb_txn_commit(m->txn);
  mdb_close(m->env, m->dbi);
  mdb_env_close(m->env);
  free(m);
}

int _mdb_count(struct _mdb *m)
{
  MDB_stat s;
  int rc;
  rc = mdb_stat(m->txn, m->dbi, &s);
  return s.ms_entries;
}

c-declare-end
)

;; encode/decode

(define lmdb:makectx rabbit-make)

(define lmdb:destroyctx rabbit-destroy)

(define (lmdb:encoder keyctx)
  (lambda (obj) (rabbit-encode keyctx (object->u8vector obj))))

(define (lmdb:decoder keyctx)
  (lambda (u8v) (with-exception-catcher (lambda (e) 
    (log-error "lmdb:decoder: failed to deserialize: " (exception->string e)) 'LMDB_FAILURE)
      (lambda () (u8vector->object (rabbit-decode keyctx u8v))))))

;; ffi

(define lmdb:init (c-lambda (char-string) (pointer void) "_mdb_init"))
(define lmdb:cleanup (c-lambda ((pointer void)) void "_mdb_cleanup"))

(define (lmdb:write m u8key u8val) ((c-lambda ((pointer void) scheme-object int scheme-object int) int
   "___result=_mdb_write(___arg1,___CAST(void*,___BODY_AS(___arg2,___tSUBTYPED)), ___arg3,
                          ___CAST(void*,___BODY_AS(___arg4,___tSUBTYPED)), ___arg5);") 
     m u8key (u8vector-length u8key) u8val (u8vector-length u8val)))

(define (lmdb:read m u8key) ((c-lambda ((pointer void) scheme-object int) int
    "___result=_mdb_read(___arg1,___CAST(void*,___BODY_AS(___arg2,___tSUBTYPED)), ___arg3);")
       m u8key (u8vector-length u8key)))
   
(define lmdb:key-len (c-lambda ((pointer void)) unsigned-int "_mdb_key_len"))
(define lmdb:value-len (c-lambda ((pointer void)) unsigned-int "_mdb_value_len"))
(define lmdb:key (c-lambda ((pointer void) scheme-object) void
  "_mdb_key(___arg1, ___CAST(void*,___BODY_AS(___arg2,___tSUBTYPED)));"))
(define lmdb:value (c-lambda ((pointer void) scheme-object) void
  "_mdb_value(___arg1, ___CAST(void*,___BODY_AS(___arg2,___tSUBTYPED)));"))

(define lmdb:count (c-lambda ((pointer void)) int "_mdb_count"))

(define lmdb:index-first (c-lambda ((pointer void)) int "_mdb_index_first"))
(define lmdb:index-next (c-lambda ((pointer void)) int "_mdb_index_next"))

(define (lmdb:delete! m u8key)
  ((c-lambda ((pointer void) scheme-object int) int
   "___result=_mdb_del(___arg1, ___CAST(void*,___BODY_AS(___arg2,___tSUBTYPED)), ___arg3);")
   m u8key (u8vector-length u8key)))

;; main interface

(define (lmdb-delete fname)
  (lmdb:log 2 "lmdb-delete " fname)
  (if (file-exists? fname) (begin
     (delete-file (string-append fname (system-pathseparator) "data.mdb"))
     (delete-file (string-append fname (system-pathseparator) "lock.mdb"))
     (delete-directory fname))))

(define (lmdb-open fname . key)
  (lmdb:log 2 "lmdb-open " fname " " key)
  (let ((ctx (if (null? key) #f (lmdb:makectx (car key)))))
    (if (not (file-exists? fname)) (create-directory fname))
    (list (lmdb:init fname) (if (not ctx) object->u8vector (lmdb:encoder ctx))
      (if (not ctx) u8vector->object (lmdb:decoder ctx)) ctx)))
(define make-lmdb lmdb-open)

(define (lmdb-set! mm key val)
  (lmdb:log 2 "lmdb-set! " mm " " key " " val)
  (let* ((lmdb:ptr (car mm))
         (lmdb:encode (cadr mm))
         (u8key (lmdb:encode key))
         (u8val (lmdb:encode val)))
    (fx= 0 (lmdb:write lmdb:ptr u8key u8val))))

(define (lmdb-ref mm key)
  (lmdb:log 2 "lmdb-get" mm " " key)
  (let* ((m (car mm))
         (encode (cadr mm))
         (decode (caddr mm))
         (u8key (encode key))
         (res (lmdb:read m u8key))
         (vlen (if (fx= res 0) (lmdb:value-len m) #f))
         (u8val (if vlen (make-u8vector vlen) #f)))
    (if u8val (begin (lmdb:value m u8val) (decode u8val)) #f)))

(define (lmdb-delete! mm key)
  (lmdb:log 2 "lmdb-delete!" mm " " key)
  (let* ((m (car mm))
         (encode (cadr mm))
         (u8key (encode key)))
    (lmdb:delete! m u8key)))

(define (lmdb-close mm) 
  (lmdb:log 2 "lmdb-close " mm)
  (let ((ctx (cadddr mm)))
    (lmdb:cleanup (car mm))
    (if ctx (lmdb:destroyctx ctx))))

(define (lmdb-count mm)
  (lmdb:log 2 "lmdb-count" mm)
  (lmdb:count (car mm)))

(define (table->lmdb t mfile . key)
  (lmdb:log 2 "table->lmdb " t " " mfile " " key)
  (lmdb-delete mfile)
  (let ((mm (apply lmdb-open (append (list mfile) key))))
    (table-for-each (lambda (k v) (lmdb-set! mm k v)) t)
    (lmdb-close mm) #t))

(define (lmdb-index mm)
  (lmdb:log 2 "lmdb-index " mm)
  (let* ((m (car mm))
         (decode (caddr mm)))
    (lmdb:index-first m)
    (let loop ((idx '()))
      (let ((res (lmdb:index-next m)))
        (if (not (fx= res 0)) idx
          (let* ((klen (lmdb:key-len m))
                 (k (make-u8vector klen)))
            (lmdb:key m k) 
            (loop (append idx (list (decode k))))))))))

(define (lmdb->table mfile . key)
  (lmdb:log 2 "lmdb->table " mfile " " key)
  (if (not (file-exists? mfile)) #f
    (let* ((mm (apply lmdb-open (append (list mfile) key)))
           (m (car mm))
           (decode (caddr mm))
           (t (make-table)))
      (if m (begin
      (lmdb:index-first m) 
      (let ((fnl (let loop ()
        (let* ((res (lmdb:index-next m))
               (klen (if (fx= res 0) (lmdb:key-len m) #f))
               (vlen (if (fx= res 0) (lmdb:value-len m) #f))
               (u8val (if vlen (make-u8vector vlen) #f))
               (u8key (if klen (make-u8vector klen) #f))
               (k (if u8key (begin (lmdb:key m u8key) (decode u8key)) #f))
               (v (if u8val (begin (lmdb:value m u8val) (decode u8val)) #f)))
           (if (or (not (fx= res 0)) (equal? k 'LMDB_FAILURE) (equal? v 'LMDB_FAILURE)) t (begin
             (table-set! t k v)
             (loop)))))))
       (lmdb-close mm) fnl
      )) #f))))

;; unit test

(unit-test "lmdb" "key value database creation and lookup"
  (lambda ()
    (let* ((fname (string-append (system-directory) (system-pathseparator) "unittest.mdb")))
      (lmdb-delete fname)
      (let* ((keys (list "a" 'a '(test) (lambda (x) x) 1 0.5 1/2))
             (values (list '1 2 "three" 'four 5. '(six) (lambda (seven) 1)))
             (cryptokey (random-u8vector 24))
             (mm (lmdb-open fname cryptokey)))
        (let loop ((ks keys)(vs values))
          (if (> (length ks) 0) (begin
            (lmdb-set! mm (car ks) (car vs))
            (loop (cdr ks) (cdr vs)))))
        (lmdb-close mm)
        (let* ((mm (lmdb-open fname cryptokey))
               (res (let loop ((ks keys)(vs values))
                  (if (fx= (length ks) 0) #t
                    (if (not (equal? (car vs) (lmdb-ref mm (car ks))))  #f
                       (loop (cdr ks) (cdr vs)))))))
           (lmdb-close mm)
           (lmdb-delete fname)
           res)))))


;; eof
