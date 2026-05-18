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

;; tiff - wrapper for libtiff image library

(define tiff:debuglevel 0)
(define (tiff:log level . x)
   (if (>= tiff:debuglevel level) (apply log-system (append (list "tiff: ") x))))

(c-declare  #<<end-of-c-declare

#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>

#include "tiffio.h"

#define  COLOR_GRAY 1
#define  COLOR_RGB 3
#define  COLOR_RGBA 4

static int ln_tiff_info(const char *fname, int infoarg)
{
  uint32_t h;
  uint32_t w;
  int res=-1;
  TIFF *tif = NULL;
  tif = TIFFOpen(fname, "r");
  if (!tif) goto info_bail;
  if (1 !=  TIFFGetField(tif, TIFFTAG_IMAGEWIDTH, &w)) goto info_bail;
  if (1 !=  TIFFGetField(tif, TIFFTAG_IMAGELENGTH, &h)) goto info_bail;
  switch (infoarg) {
    case 1: res=(int) w; break;
    case 2: res=(int) h; break;
  }
info_bail:
  if (tif) TIFFClose(tif);
  return res;
}

static int ln_tiff_from_u8vector(int w, int h, unsigned char *data, int datalen, const char *fname)
{

  int res=-1;
  int color_types[] = { -1, COLOR_GRAY, -1, COLOR_RGB, COLOR_RGBA};
  int stride = datalen/(w*h);
  unsigned char *buf = NULL;      // buffer used to store the row of pixel information for writing to file
  int color_type = (stride<5&&stride>0?color_types[stride]:-1);
  int linebytes = color_type * w;   // length in memory of one row of pixel in the image.
  if (color_type<0) goto writer_bail;
  if (stride*w*h!=datalen) goto writer_bail;
  buf =(unsigned char *)_TIFFmalloc(linebytes);  
  TIFF *tif = NULL;
  tif = TIFFOpen(fname, "w");
  if (!tif) goto writer_bail;
  TIFFSetField (tif, TIFFTAG_IMAGEWIDTH, w);  // set the width of the image
  TIFFSetField(tif, TIFFTAG_IMAGELENGTH, h);    // set the height of the image
  TIFFSetField(tif, TIFFTAG_SAMPLESPERPIXEL, color_type);   // set number of channels per pixel
  TIFFSetField(tif, TIFFTAG_BITSPERSAMPLE, 8);    // set the size of the channels
  TIFFSetField(tif, TIFFTAG_ORIENTATION, ORIENTATION_TOPLEFT);    // set the origin of the image.
  //   Some other essential fields to set that you do not have to understand for now.
  TIFFSetField(tif, TIFFTAG_PLANARCONFIG, PLANARCONFIG_CONTIG);
  TIFFSetField(tif, TIFFTAG_PHOTOMETRIC, PHOTOMETRIC_RGB);
  // We set the strip size of the file to be size of one row of pixels
  TIFFSetField(tif, TIFFTAG_ROWSPERSTRIP, TIFFDefaultStripSize(tif, linebytes));
  // Write image to the file one strip at a time
  for (int j = 0; j < h; j++) {
    memcpy(buf, &data[j*linebytes], linebytes);
    if (TIFFWriteScanline(tif, buf, j, 0) < 0) break;
  }
 res = 0;
 writer_bail:
  if (buf) _TIFFfree(buf);
  if (tif) TIFFClose(tif);;
  return res;
}

static int ln_tiff_to_u8vector(int w0, int h0, unsigned char *data, const char *fname)
{ 
  int res=-1;
  uint32_t *file_data = NULL;
  TIFF *tif = NULL;
  tif = TIFFOpen(fname, "r");
  if (!tif) goto reader_bail;
  file_data = (uint32_t*) _TIFFmalloc(w0 * h0 * sizeof (uint32_t));
  if (file_data == NULL) goto reader_bail;
  if (0 == TIFFReadRGBAImage(tif, (uint32_t) w0, (uint32_t) h0, file_data, 0)) goto reader_bail;
  for(int i=0; i < w0*h0; i++) {
      data[4*i    ] = (unsigned char) TIFFGetR(file_data[i]);
      data[4*i + 1] = (unsigned char) TIFFGetB(file_data[i]);
      data[4*i + 2] = (unsigned char) TIFFGetG(file_data[i]);
      data[4*i + 3] = (unsigned char) TIFFGetA(file_data[i]);
  }
  res=0;
reader_bail:
  if (file_data) _TIFFfree(file_data);
  if (tif) TIFFClose(tif);  
  return res; 
}

end-of-c-declare
)

(define (tiff:info fname idx)
  (tiff:log 2 "tiff:info " fname " " idx)
  (let ((res ((c-lambda (char-string int) int "ln_tiff_info") fname idx)))
    (if (fx= res -1) (begin (log-error "tiff:info " idx " failed on " fname) #f) res)))

(define (tiff-width fname) (tiff:log 1 "tiff-width " fname) (tiff:info fname 1))
(define (tiff-height fname) (tiff:log 1 "tiff-height " fname) (tiff:info fname 2))

(define (u8vector->tiff data fname w h)
  (tiff:log 1 "u8vector->tiff " w " " h " [] " fname)
  (fx= ((c-lambda (int int scheme-object int char-string) int
           "___result=ln_tiff_from_u8vector(___arg1,___arg2,___CAST(void*,___BODY_AS(___arg3,___tSUBTYPED)),___arg4,___arg5);")
     w h data (u8vector-length data) fname) 0))

(define (tiff->u8vector fname . xargs)
  (tiff:log 1 "tiff->u8vector " fname " " xargs)
  (let* ((w (tiff-width fname))
         (h (tiff-height fname))
	 (w0 (if (= (length xargs) 2) (car xargs) w))
         (h0 (if (= (length xargs) 2) (cadr xargs) h))
         (data (if (and w h) (make-u8vector (* w h 4) 0) #f)))
    (if data (begin
      (if (fx= ((c-lambda (int int scheme-object char-string) int 
          "___result=ln_tiff_to_u8vector(___arg1,___arg2,___CAST(void*,___BODY_AS(___arg3,___tSUBTYPED)),___arg4);") 
     w0 h0 data fname) 0) data #f))
          (begin 
	    (log-error "tiff->u8vector failed on " fname) #f))))

;; ------
;; opengl related functions 
;; eval is used to delay resolving potentially unavailable calls

(define (tiff:tiff->texture fname . xargs)
  (tiff:log 1 "tiff->texture " fname " " xargs)
  (let* ((w (tiff-width fname))
         (h (tiff-height fname))
         (w0 (if (= (length xargs) 2) (car xargs) w))
         (h0 (if (= (length xargs) 2) (cadr xargs) h))	 
         (data (tiff->u8vector fname w0 h0)))
    (if data ((eval 'glCoreTextureCreate) w0 h0 data)
      (begin (log-error "tiff:tiff->texture failed on " fname) #f))))

(define (tiff->img fname)
  (tiff:log 1 "tiff->img " fname)
  (let* ((w (tiff-width fname))
	 (h (tiff-height fname))
	 (w0 (fix (expt 2. (ceiling (/ (log w) (log 2.))))))
	 (h0 (fix (expt 2. (ceiling (/ (log h) (log 2.))))))
	 (t (tiff:tiff->texture fname)))
    (if (and w h t)
      (list w h t 0. (- 1. (/ h h0 1.)) (/ w w0 1.) 1.)
        (begin (log-error "tiff->img failed on " fname) #f))))

(define (tiff:texture->tiff t fname)
  (tiff:log 1 "texture->tiff " t " " fname)
  (let ((w ((eval 'glCoreTextureWidth) t))
        (h ((eval 'glCoreTextureHeight) t))
        (data ((eval 'glCoreTextureData) t)))
   (u8vector->tiff data fname w h)))
 
(define (img->tiff img fname)
  (tiff:texture->tiff (caddr img) fname))

(define (screenshot->tiff fname)
  (tiff:log 1 "screenshot->webp " fname)
  (let* ((w ((eval 'glgui-width-get)))
         (h ((eval 'glgui-height-get)))
         (data ((eval 'glCoreReadPixels) 0 0 w h)))
    (u8vector->tiff data fname w h)))

;; ------
;; unit test

(unit-test "tiff" "1000 random image encode-decode runs"
  (lambda () 
    (let* ((fname (string-append (system-directory) (system-pathseparator) "unittest.tiff"))
           (res (let loop ((n 1000))
                   (if (fx= n 0) #t (if 
                     (let* ((w (+ 1 (random-integer 200)))
                            (h (+ 1 (random-integer 200)))
                            (data (random-u8vector (* 4 w h))))
                       (u8vector->tiff data fname w h)
                       (not (and (= w (tiff-width fname))
                                 (= h (tiff-height fname))
                                 (equal? data (tiff->u8vector fname))))) #f (loop (fx- n 1)))))))
        (if (file-exists? fname) (delete-file fname))
        res)))

;; eof
