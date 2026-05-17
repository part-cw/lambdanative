#|
LambdaNative - a cross-platform Scheme framework
Copyright (c) 2009-2014, University of British Columbia
Copyright (c) 2026, Benson Muite
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

;; webp - wrapper for libwebp image library

(define webp:debuglevel 0)
(define (webp:log level . x)
   (if (>= webp:debuglevel level) (apply log-system (append (list "webp: ") x))))

(c-declare  #<<end-of-c-declare

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

#include <webp/decode.h>
#include <webp/encode.h>
#include <webp/types.h>

#define  COLOR_GRAY 1
#define  COLOR_RGB 3
#define  COLOR_RGBA 4

static int ln_webp_info(const char *fname, int infoarg)
{

  FILE *fd=0;
  int res=-1;
  uint8_t *file_data;
  uint8_t *width;
  uint8_t *height;
  WebPBitstreamFeatures data_features;
  size_t file_size;
  fd = fopen(fname, "rb");
  if (!fd) goto info_bail;
  fseek(fd, 0, SEEK_END);
  file_size = ftell(fd);
  if (file_size == (size_t)-1) goto info_bail;
  fseek(fd, 0, SEEK_SET);
  // we allocate one extra byte for the \0 terminator
  file_data = (uint8_t*)WebPMalloc(file_size + 1);
  if (file_data == NULL) goto info_bail;
  if (!(fread(file_data, file_size, 1, fd) == 1)) goto info_bail;
  file_data[file_size] = '\0';  // convenient 0-terminator
  if(WebPGetFeatures(file_data, file_size, &data_features) != VP8_STATUS_OK)  goto info_bail;
  switch (infoarg) {
    case 1: res=(int) data_features.width; break;
    case 2: res=(int) data_features.height; break;
    case 3: res=(int) file_size; break;
    case 4: res=(int) data_features.has_alpha; break;
    case 5: res=(int) data_features.has_animation; break;
    case 6: res=(int) data_features.format; break;
  }
info_bail:
  if (file_data) WebPFree(file_data);
  if (fd) fclose(fd);
  return res;
}

static int ln_webp_from_u8vector(int w, int h, unsigned char *data, int datalen, const char *fname)
{

  FILE *fd=0;
  int res=-1;
  int color_types[] = { -1, COLOR_GRAY, -1, COLOR_RGB, COLOR_RGBA};
  int stride = datalen/(w*h);
  int color_type = (stride<5&&stride>0?color_types[stride]:-1);
  if (color_type<0) goto writer_bail;
  if (stride*w*h!=datalen) goto writer_bail;
  uint8_t ** output = NULL;
  uint8_t * data_webp = NULL;
  data_webp = (uint8_t*)malloc(4*datalen*sizeof(uint8_t));
  if (!data_webp) goto writer_bail;
  size_t output_size=0;
  fd = fopen (fname, "wb");
  if (!fd) goto writer_bail;
  switch (color_type) {
    case COLOR_GRAY:
       for(int i=0; i < datalen; i++) {
	       data_webp[3*i] = (uint8_t)data[i];
	       data_webp[3*i+1] = (uint8_t)data[i];
	       data_webp[3*i+2] = (uint8_t)data[i];
       }
       output_size=WebPEncodeLosslessRGB(data_webp, w, h, stride, output);
    case COLOR_RGB:
       for(int i=0; i < datalen; i++) {
               data_webp[i] = (uint8_t)data[i];
       }    
       output_size=WebPEncodeLosslessRGB(data_webp, w, h, stride, output);
       break;
    case COLOR_RGBA:
       for(int i=0; i < datalen; i++) {
               data_webp[i] = (uint8_t)data[i];
       }
       output_size=WebPEncodeLosslessRGBA(data_webp, w, h, stride, output);
       break;
 }
 int out = fwrite(output,output_size,1,fd); 
 res = 0;
 writer_bail:
  if (data_webp) free(data_webp);
  if (output) WebPFree(*output);
  if (fd) fclose(fd);
  return res;
}

static int ln_webp_to_u8vector(int w0, int h0, unsigned char *data, int file_size, const char *fname)
{ 
  FILE *fd=0;
  int res=-1;
  uint8_t *file_data = NULL;
  uint8_t *buf = NULL;
  size_t width;
  size_t height;
  fd = fopen(fname, "rb");
  if (!fd) goto reader_bail;
  // allocate an extra byte for the \0 terminator
  file_data = (uint8_t*)WebPMalloc(file_size + 1);
  if (file_data == NULL) goto reader_bail;
  if (!(fread(file_data, file_size, 1, fd) == 1)) goto reader_bail;
  file_data[file_size] = '\0';  // convenient 0-terminator
  buf = WebPDecodeRGBA((const uint8_t*)file_data, (size_t)file_size, &width, &height);
  for(int i=0; i < 4*w0*h0; i++) {
      data[i] = (unsigned char) buf[i];
  }
  res=0;
reader_bail:
  if (file_data) WebPFree(file_data);
  if (buf) WebPFree(buf);
  if (fd) fclose(fd);
  return res; 
}

end-of-c-declare
)

(define (webp:info fname idx)
  (webp:log 2 "webp:info " fname " " idx)
  (let ((res ((c-lambda (char-string int) int "ln_webp_info") fname idx)))
    (if (fx= res -1) (begin (log-error "webp:info " idx " failed on " fname) #f) res)))

(define (webp-width fname) (webp:log 1 "webp-width " fname) (webp:info fname 1))
(define (webp-height fname) (webp:log 1 "webp-height " fname) (webp:info fname 2))
(define (webp-file_size fname) (webp:log 1 "webp-file_size " fname) (webp:info fname 3))
(define (webp-has_alpha fname) (webp:log 1 "webp-has_alpha " fname) (webp:info fname 4))
(define (webp-has_animation fname) (webp:log 1 "webp-has_animation " fname) (webp:info fname 5))
(define (webp-compression_format fname) (webp:log 1 "webp-compression_format " fname) (webp:info fname 6))

(define (u8vector->webp data fname w h)
  (webp:log 1 "u8vector->webp " w " " h " [] " fname)
  (fx= ((c-lambda (int int scheme-object int char-string) int
           "___result=ln_webp_from_u8vector(___arg1,___arg2,___CAST(void*,___BODY_AS(___arg3,___tSUBTYPED)),___arg4,___arg5);")
     w h data (u8vector-length data) fname) 0))

(define (webp->u8vector fname . xargs)
  (webp:log 1 "webp->u8vector " fname " " xargs)
  (let* ((w (webp-width fname))
         (h (webp-height fname))
	 (a (webp-has_animation fname))
	 (w0 (if (= (length xargs) 2) (car xargs) w))
         (h0 (if (= (length xargs) 2) (cadr xargs) h))
	 (file_size (webp-file_size fname))
         (data (if (and w h file_size (equal? a 0)) (make-u8vector (* w h 4) 0) #f)))
    (if data (begin
      (if (fx= ((c-lambda (int int scheme-object int char-string) int 
          "___result=ln_webp_to_u8vector(___arg1,___arg2,___CAST(void*,___BODY_AS(___arg3,___tSUBTYPED)),___arg4,___arg5);") 
     w0 h0 data file_size fname) 0) data #f))
          (begin 
	    (log-error "webp->u8vector failed on " fname) #f))))

;; ------
;; opengl related functions 
;; eval is used to delay resolving potentially unavailable calls

(define (webp:webp->texture fname . xargs)
  (webp:log 1 "webp->texture " fname " " xargs)
  (let* ((w (webp-width fname))
         (h (webp-height fname))
	 (a (webp-has_animation fname))
         (w0 (if (= (length xargs) 2) (car xargs) w))
         (h0 (if (= (length xargs) 2) (cadr xargs) h))	 
         (data (webp->u8vector fname w0 h0)))
    (if (and data (equal? a 0)) ((eval 'glCoreTextureCreate) w0 h0 data)
      (begin (log-error "webp:webp->texture failed on " fname) #f))))

(define (webp->img fname)
  (webp:log 1 "webp->img " fname)
  (let* ((w (webp-width fname))
	 (h (webp-height fname))
	 (a (webp-has_animation fname))
	 (w0 (fix (expt 2. (ceiling (/ (log w) (log 2.))))))
	 (h0 (fix (expt 2. (ceiling (/ (log h) (log 2.))))))
	 (t (webp:webp->texture fname)))
    (if (and w h t (equal? a 0))
      (list w h t 0. (- 1. (/ h h0 1.)) (/ w w0 1.) 1.)
        (begin (log-error "webp->img failed on " fname) #f))))

(define (webp:texture->webp t fname)
  (webp:log 1 "texture->webp " t " " fname)
  (let ((w ((eval 'glCoreTextureWidth) t))
        (h ((eval 'glCoreTextureHeight) t))
        (data ((eval 'glCoreTextureData) t)))
   (u8vector->webp data fname w h)))
 
(define (img->webp img fname)
  (webp:texture->webp (caddr img) fname))

(define (screenshot->webp fname)
  (webp:log 1 "screenshot->webp " fname)
  (let* ((w ((eval 'glgui-width-get)))
         (h ((eval 'glgui-height-get)))
         (data ((eval 'glCoreReadPixels) 0 0 w h)))
    (u8vector->webp data fname w h)))

;; ------
;; unit test

(unit-test "webp" "1000 random image encode-decode runs"
  (lambda () 
    (let* ((fname (string-append (system-directory) (system-pathseparator) "unittest.webp"))
           (res (let loop ((n 1000))
                   (if (fx= n 0) #t (if 
                     (let* ((w (+ 1 (random-integer 200)))
                            (h (+ 1 (random-integer 200)))
                            (data (random-u8vector (* 4 w h))))
                       (u8vector->webp data fname w h)
                       (not (and (= w (webp-width fname))
                                 (= h (webp-height fname))
                                 (equal? data (webp->u8vector fname))))) #f (loop (fx- n 1)))))))
        (if (file-exists? fname) (delete-file fname))
        res)))

;; eof
