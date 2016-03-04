#|
LambdaNative - a cross-platform Scheme framework
Copyright (c) 2009-2016, University of British Columbia
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

;; minimal wrapper around the hidapi library for communicating with USB and Bluetooth HID devices

(##namespace ("hidapi#"))
(##include "~~lib/gambit#.scm")
(##include "hidapi#.scm")

(##namespace ("" log-system))

(define debuglevel 0)
(define (hidapi:log level . x)
   (if (>= debuglevel level) (apply log-system (append (list "hidapi: ") x))))

(c-declare  #<<end-of-c-declare

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <hidapi/hidapi.h>

typedef struct hid_device_info hid_dev_info;

end-of-c-declare
)

(c-define-type hid_device "hid_device")
(c-define-type hid_device* (pointer hid_device))
(c-define-type hid_device_info "hid_dev_info")
(c-define-type hid_device_info* (pointer hid_device_info))

(define (hid-init) (hidapi:log 1 "hid-init") ((c-lambda () int "hid_init")))
(define (hid-exit) (hidapi:log 1 "hid-exit") ((c-lambda () int "hid_exit")))

;;hid_device *  hid_open(unsigned short vendor_id, unsigned short product_id, const wchar_t *serial_number);
(define (hid-open vid pid sn)
  (hidapi:log 1 "hid-open " vid " " pid " " sn)
  ((c-lambda (unsigned-short unsigned-short wchar_t-string) hid_device* "hid_open") vid pid sn))

;;hid_device *  hid_open_path(const char *path);
(define (hid-open-path p)
  (hidapi:log 1 "hid-open-path " p) 
  ((c-lambda (char-string) hid_device* "hid_open_path") p))

;;struct hid_device_info  *  hid_enumerate(unsigned short vendor_id, unsigned short product_id);
(define (hid-enumerate vid pid)
  (hidapi:log 1 "hid-enumerate " vid " " pid)
  ((c-lambda (unsigned-short unsigned-short) hid_device_info* "hid_enumerate") vid pid))

(define (hid-device-info->path di) 
  (hidapi:log 1 "hid-device-info->path " di)
  ((c-lambda (hid_device_info*) char-string "___result=___arg1->path;") di))
(define (hid-device-info->vendor_id di)
  (hidapi:log 1 "hid-device-info->vendor_id " di)
   ((c-lambda (hid_device_info*) int "___result=___arg1->vendor_id;") di))
(define (hid-device-info->product_id di) 
  (hidapi:log 1 "hid-device-info->product_id " di)
  ((c-lambda (hid_device_info*) int "___result=___arg1->product_id;") di))
(define (hid-device-info->serial_number di) 
  (hidapi:log 1 "hid-device-info->serial_number " di)
  ((c-lambda (hid_device_info*) wchar_t-string "___result=___arg1->serial_number;") di))
(define (hid-device-info->release_number di) 
  (hidapi:log 1 "hid-device-info->release_number " di)
  ((c-lambda (hid_device_info*) unsigned-short "___result=___arg1->release_number;") di))
(define (hid-device-info->manufacturer_string di) 
  (hidapi:log 1 "hid-device-info->manufacturer_string " di)
  ((c-lambda (hid_device_info*) wchar_t-string "___result=___arg1->manufacturer_string;") di))
(define (hid-device-info->product_string di) 
  (hidapi:log 1 "hid-device-info->product_string " di)
  ((c-lambda (hid_device_info*) wchar_t-string "___result=___arg1->product_string;") di))
(define (hid-device-info->usage_page di) 
  (hidapi:log 1 "hid-device-info->usage_page " di)
  ((c-lambda (hid_device_info*) unsigned-short "___result=___arg1->usage_page;") di))
(define (hid-device-info->usage di) 
  (hidapi:log 1 "hid-device-info->usage " di)
  ((c-lambda (hid_device_info*) unsigned-short "___result=___arg1->usage;") di))
(define (hid-device-info->interface_number di) 
  (hidapi:log 1 "hid-device-info->interface_number " di)
  ((c-lambda (hid_device_info*) int "___result=___arg1->interface_number;") di))
(define (hid-device-info->next di) 
  (hidapi:log 1 "hid-device-info->next " di)
  ((c-lambda (hid_device_info*) hid_device_info* "___result=___arg1->next;") di))

;;void hid_free_enumeration(struct hid_device_info *devs);
(define (hid-free-enumeration devs)
  (hidapi:log 1 "hid-free-enumeration " devs)
  ((c-lambda (hid_device_info*) void "hid_free_enumeration") devs))

;;int  hid_write(hid_device *device, const unsigned char *data, size_t length);
(define (hid-write dev u8v)
  (hidapi:log 2 "hid-write " dev " " u8v)
  ((c-lambda (hid_device* scheme-object int) int "___result=hid_write(___arg1,___CAST(void*,___BODY_AS(___arg2,___tSUBTYPED)),___arg3);")
     dev u8v (u8vector-length u8v)))
 
;;int  hid_read_timeout(hid_device *dev, unsigned char *data, size_t length, int milliseconds);
(define (hid-read-timeout dev data timeout)
  (hidapi:log 2 "hid-read-timeout " dev " " data " " timeout)
  (let* ((len (u8vector-length data))
         (res ((c-lambda (hid_device* scheme-object int int) int
           "___result=hid_read_timeout(___arg1,___CAST(void*,___BODY_AS(___arg2,___tSUBTYPED)),___arg3,___arg4);")
             dev data len timeout)))
     res))

;;int  hid_read(hid_device *device, unsigned char *data, size_t length);
(define (hid-read dev data)
  (hidapi:log 2 "hid-read " dev " " data)
  (let* ((len (u8vector-length data))
         (res ((c-lambda (hid_device* scheme-object int) int
           "___result=hid_read(___arg1,___CAST(void*,___BODY_AS(___arg2,___tSUBTYPED)),___arg3);")
             dev data len)))
    res))

;;int  hid_set_nonblocking(hid_device *device, int nonblock);
(define (hid-set-nonblocking dev nonblock)
  (hidapi:log 1 "hid-set-nonblocking " dev " " nonblock)
  (= 0 ((c-lambda (hid_device* int) int "hid_set_nonblocking") dev nonblock)))

;;int  hid_send_feature_report(hid_device *device, const unsigned char *data, size_t length);
(define (hid-send-feature-report dev u8v)
  (hidapi:log 1 "hid-send-feature-report " dev " " u8v)
  ((c-lambda (hid_device* scheme-object int) int
     "___result=hid_send_feature_report(___arg1,___CAST(void*,___BODY_AS(___arg2,___tSUBTYPED)),___arg3);")
     dev u8v (u8vector-length u8v)))

;;int  hid_get_feature_report(hid_device *device, unsigned char *data, size_t length);
(define (hid-get-feature-report dev len)
  (hidapi:log 1 "hid-get-feature-report " dev " " len)
  (let* ((data (make-u8vector len 0))
         (res ((c-lambda (hid_device* scheme-object int) int
           "___result=hid_get_feature_report(___arg1,___CAST(void*,___BODY_AS(___arg2,___tSUBTYPED)),___arg3);")
             dev data len)))
    (subu8vector data 0 res)))

;;void hid_close(hid_device *device);
(define (hid-close dev)
  (hidapi:log 1 "hid-close " dev)
  ((c-lambda (hid_device*) void "hid_close") dev))

(define u8vector->wcharstring 
  (c-lambda (scheme-object) wchar_t-string "___result=(wchar_t*)___CAST(void*,___BODY_AS(___arg1,___tSUBTYPED));"))

;;int  hid_get_manufacturer_string(hid_device *device, wchar_t *string, size_t maxlen);
(define (hid-get-manufacturer-string dev len)
  (hidapi:log 1 "hid-get-manufacturer-string " dev " " len)
  (let* ((data (make-u8vector len 0))
         (res ((c-lambda (hid_device* scheme-object int) int
           "___result=hid_get_manufacturer_string(___arg1,___CAST(void*,___BODY_AS(___arg2,___tSUBTYPED)),___arg3);")
             dev data len)))
    (if (>= res 0) (u8vector->wcharstring data) "")))

;;int  hid_get_product_string(hid_device *device, wchar_t *string, size_t maxlen);
(define (hid-get-product-string dev len)
  (hidapi:log 1 "hid-get-product-string " dev " " len)
  (let* ((data (make-u8vector len 0))
         (res ((c-lambda (hid_device* scheme-object int) int
           "___result=hid_get_product_string(___arg1,___CAST(void*,___BODY_AS(___arg2,___tSUBTYPED)),___arg3);")
             dev data len)))
    (if (>= res 0) (u8vector->wcharstring  data) "")))

;;int  hid_get_serial_number_string(hid_device *device, wchar_t *string, size_t maxlen);
(define (hid-get-serial-number-string dev len)
  (hidapi:log 1 "hid-get-serial-number-string " dev " " len)
  (let* ((data (make-u8vector len 0))
         (res ((c-lambda (hid_device* scheme-object int) int
           "___result=hid_get_serial_number_string(___arg1,___CAST(void*,___BODY_AS(___arg2,___tSUBTYPED)),___arg3);")
             dev data len)))
    (if (>= res 0) (u8vector->wcharstring data) "")))

;;int  hid_get_indexed_string(hid_device *device, int string_index, wchar_t *string, size_t maxlen);
(define (hid-get-indexed-string dev index len)
  (hidapi:log 1 "hid-get-indexed-string " dev " " index " " len)
  (let* ((data (make-u8vector len 0))
         (res ((c-lambda (hid_device* int scheme-object int) int
           "___result=hid_get_indexed_string(___arg1,___arg2, ___CAST(void*,___BODY_AS(___arg3,___tSUBTYPED)),___arg4);")
             dev index data len)))
    (if (>= res 0) (u8vector->wcharstring data) "")))

;;const wchar_t*  hid_error(hid_device *device);
(define (hid-error dev)
  (hidapi:log 1 "hid-error " dev)
  ((c-lambda (hid_device*) wchar_t-string "hid_error") dev))

(define (hid-device-list)
  (hidapi:log 1 "hid-device-list")
  (let ((devinfo (hid-enumerate 0 0)))
    (let loop ((di devinfo)(res '()))
      (if (not di) (begin (hid-free-enumeration devinfo) res)
        (loop (hid-device-info->next di)
          (append res (list (list
             (cons 'path (hid-device-info->path di))
             (cons 'vendor_id (hid-device-info->vendor_id di))
             (cons 'product_id (hid-device-info->product_id di))
             (cons 'serial_number (hid-device-info->serial_number di))
             (cons 'release_number (hid-device-info->release_number di))
             (cons 'manufacturer_string (hid-device-info->manufacturer_string di))
             (cons 'product_string (hid-device-info->product_string di))
             (cons 'usage_page (hid-device-info->usage_page di))
             (cons 'usage (hid-device-info->usage di))
             (cons 'interface_number (hid-device-info->interface_number di)))))
          )))))

(hid-init)

;; eof
