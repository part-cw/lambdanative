#|
LambdaNative - a cross-platform Scheme framework
Copyright (c) 2009-2013, University of British Columbia
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

;; plug-in instance related

(define (instance-setvar! store instance name val)
  (store-set! store (string-append instance "#" name) val))
(define (instance-refvar store instance name . fback)
  (apply store-ref (append (list store (string-append instance "#" name)) fback)))

(define (instance:pluginname store instance)
  (instance-refvar store instance "PluginName" #f))
(define (instance:plugintype store instance)
  (instance-refvar store instance "PluginType" #f))

;; hooks to the plugin procedures
(define (instance:proc store instance proc . x)
  (let ((theproc (proc (instance:pluginname store instance))))
    (if (procedure? theproc)
      (apply theproc (append (list store instance) x))
      (log-error (string-append "instance: plugin procedure missing in " instance))
    )
  ))
(define (instance:init store instance . x)
  (apply instance:proc (append (list store instance plugin:start) x)))
(define (instance:caseinit store instance . x)
  (apply instance:proc (append (list store instance plugin:casestart) x)))
(define (instance:run store instance . x)
  (apply instance:proc (append (list store instance plugin:run) x)))
(define (instance:caseend store instance . x)
  (apply instance:proc (append (list store instance plugin:caseend) x)))
(define (instance:end store instance . x)
  (apply instance:proc (append (list store instance plugin:end) x)))

(define (make-instance store instance plugin . config)
  (let ((is (store-ref store "InstanceList" '())))
    (if (not (member instance is))
      (begin
        (log-system (string-append "instance: creating " instance))
        (store-set! store "InstanceList" (append is (list instance)))
        (instance-setvar! store instance "PluginName" plugin)
        (instance-setvar! store instance "PluginType" (plugin:type plugin))
        (let loop ((c config))
          (if (> (length c) 0) (begin
            (instance-setvar! store instance (car (car c)) (cadr (car c)))
            (loop (cdr c))
          ))
        )
        ;; usually defer initialization to scheduler-init, unless it's already been done
        (if (scheduler-initialized?) (begin
          (instance:init store instance)
          (if (store-ref store "CaseID" #f) (instance:caseinit store instance))
        ))
      )
      (log-warning (string-append "instance: " instance " already exists."))
    )
  ))

(define (instance-delete store instance)
  (let ((is (store-ref store "InstanceList" '())))
    (if (and (member instance is) (not (store-ref store "CaseID" #f)))
      (let ((newlist (let loop ((l is)(res '()))
                       (if (fx= (length l) 0) res
                       (loop (cdr l) (append res (if (equal? instance (car l)) '() (list (car l)))))))))
        (store-set! store "InstanceList" newlist)
        (instance:end store instance)
        #t
      )
      #f
    )
  ))

(define (instance:all store)
  (store-ref store "InstanceList" '()))

(define (instance:allspecific store itype)
  (let loop ((is (instance:all store))(os '()))
    (if (= (length is) 0)
      os
      (loop (cdr is) (append os
        (if (equal? (instance:plugintype store (car is)) itype) (list (car is)) '())
      ))
    )
  ))
(define (instance:allinputs store)
  (instance:allspecific store 'input))
(define (instance:alloutputs store)
  (instance:allspecific store 'output))
(define (instance:alldses store)
  (instance:allspecific store 'dse))
(define (instance:allalgs store)
  (instance:allspecific store 'algorithm))

;; eof