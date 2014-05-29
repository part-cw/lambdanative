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

;; png - wrapper for libpng image library

(define png:debuglevel 0)
(define (png:log level . x)
   (if (>= png:debuglevel level) (apply log-system (append (list "png: ") x))))

(c-declare  #<<end-of-c-declare

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

#include <png.h>

static int ln_png_info(char *fname, int infoarg)
{
  FILE *fd=0;
  png_structp png_ptr = 0;
  png_infop info_ptr = 0;
  unsigned char header[8];
  int tmp,res=-1;
  fd = fopen(fname, "rb");
  if (!fd) goto info_bail;
  fread(header, 1, 8, fd);
  if (png_sig_cmp(header, 0, 8)) goto info_bail;
  png_ptr = png_create_read_struct(PNG_LIBPNG_VER_STRING, NULL, NULL, NULL);
  if (!png_ptr) goto info_bail;
  info_ptr = png_create_info_struct(png_ptr);
  if (!info_ptr) goto info_bail;
  if (setjmp(png_jmpbuf(png_ptr))) { goto info_bail; }
  png_init_io(png_ptr, fd); 
  png_set_sig_bytes(png_ptr, 8);
  png_read_info(png_ptr, info_ptr);
  switch (infoarg) {
    case 1: res=png_get_image_width(png_ptr, info_ptr); break;
    case 2: res=png_get_image_height(png_ptr, info_ptr); break;
    case 3: res=png_get_bit_depth(png_ptr, info_ptr); break; 
    case 4: 
      tmp=png_get_color_type(png_ptr, info_ptr); 
      switch (tmp) {
        case PNG_COLOR_TYPE_GRAY: res = 1; break;
        case PNG_COLOR_TYPE_RGB: res = 3; break;
        case PNG_COLOR_TYPE_RGBA : res = 4; break;
      }
      break;
  }
info_bail:
  if (info_ptr) png_destroy_info_struct(png_ptr, &info_ptr);
  if (png_ptr) png_destroy_read_struct(&png_ptr, &info_ptr,0);
  if (fd) fclose(fd);
  return res;
}

static int ln_png_from_u8vector(int w, int h, unsigned char *data, int datalen, const char *fname)
{
  FILE *fd=0;
  png_structp png_ptr = 0;
  png_infop info_ptr = 0;
  int res=-1;
  int color_types[] = { -1, PNG_COLOR_TYPE_GRAY, -1, PNG_COLOR_TYPE_RGB, PNG_COLOR_TYPE_RGBA};
  int stride = datalen/(w*h);
  int color_type = (stride<5&&stride>0?color_types[stride]:-1);
  if (color_type<0) goto writer_bail;
  if (stride*w*h!=datalen) goto writer_bail;
  png_byte ** row_pointers = NULL;
  size_t x, y;
  fd = fopen (fname, "wb");
  if (!fd) goto writer_bail;
  png_ptr = png_create_write_struct (PNG_LIBPNG_VER_STRING, NULL, NULL, NULL);
  if (!png_ptr) goto writer_bail;
  info_ptr = png_create_info_struct (png_ptr);
  if (!info_ptr) goto writer_bail;
  if (setjmp (png_jmpbuf (png_ptr)))  goto writer_bail;
  png_set_IHDR (png_ptr, info_ptr, w, h, 8,  // depth
                color_type, PNG_INTERLACE_NONE,
                PNG_COMPRESSION_TYPE_DEFAULT, PNG_FILTER_TYPE_DEFAULT);
  row_pointers = png_malloc (png_ptr, h * sizeof (png_byte *));
  for (y = 0; y < h; ++y) {
    png_byte *row = png_malloc (png_ptr, sizeof (uint8_t) * w * stride);
    row_pointers[y] = row;
    for (x = 0; x < w; ++x) {
      int i; 
      for (i=0;i<stride;i++) {
        *row++ = data[stride*x + stride*(h-y-1)*w + i];
      }
    }
  }
  png_init_io (png_ptr, fd);
  png_set_rows (png_ptr, info_ptr, row_pointers);
  png_write_png (png_ptr, info_ptr, PNG_TRANSFORM_IDENTITY, NULL);
  for (y = 0; y < h; y++) { png_free (png_ptr, row_pointers[y]); }
  png_free (png_ptr, row_pointers);
  res = 0;
 writer_bail:
  if (info_ptr) png_destroy_info_struct(png_ptr, &info_ptr);
  if (png_ptr)  png_destroy_write_struct (&png_ptr, &info_ptr);
  if (fd) fclose(fd);
  return res;
}

static int ln_png_to_u8vector(int w0, int h0, unsigned char *data, int datalen, const char *fname)
{ 
  FILE *fd=0;
  png_structp png_ptr=0;
  png_infop info_ptr=0;
  int x,y,i;
  int res=-1;
  png_bytep * row_pointers;
  unsigned char header[8];
  fd = fopen(fname, "rb");
  if (!fd) goto reader_bail;
  fread(header, 1, 8, fd);
  if (png_sig_cmp(header, 0, 8)) goto reader_bail;
  png_ptr = png_create_read_struct(PNG_LIBPNG_VER_STRING, NULL, NULL, NULL);
  if (!png_ptr) goto reader_bail;
  info_ptr = png_create_info_struct(png_ptr);
  if (!info_ptr) goto reader_bail;
  if (setjmp(png_jmpbuf(png_ptr))) goto reader_bail;
  png_init_io(png_ptr, fd);
  png_set_sig_bytes(png_ptr, 8);
  png_read_info(png_ptr, info_ptr);
  int width = png_get_image_width(png_ptr, info_ptr);
  int height = png_get_image_height(png_ptr, info_ptr);
  int color_type = png_get_color_type(png_ptr, info_ptr);
  int bit_depth = png_get_bit_depth(png_ptr, info_ptr);
  int stride=0;
  switch (color_type) {
    case PNG_COLOR_TYPE_GRAY: stride=1; break;
    case PNG_COLOR_TYPE_RGB: stride=3; break;
    case PNG_COLOR_TYPE_RGBA: stride=4; break;
  } 
  if (!stride) goto reader_bail;
  if (width>w0) goto reader_bail;
  if (height>h0) goto reader_bail;
  png_set_interlace_handling(png_ptr);
  png_read_update_info(png_ptr, info_ptr);
  if (setjmp(png_jmpbuf(png_ptr))) goto reader_bail;
  row_pointers = (png_bytep*) malloc(sizeof(png_bytep) * height);
  for (y=0; y<height; y++)
    row_pointers[y] = (png_byte*) malloc(png_get_rowbytes(png_ptr,info_ptr));
  png_read_image(png_ptr, row_pointers);
  for (y=0; y<height; y++) {
    for (x=0; x<width; x++) { 
      for (i=0;i<stride;i++) {
        data[stride*x + stride*(h0-y-1)*w0 + i] = row_pointers[y][stride*x+i];
      }
    }
  }
  for (y = 0; y < height; y++) { png_free (png_ptr, row_pointers[y]); }
  png_free (png_ptr, row_pointers);
  res=0;
reader_bail:
  if (info_ptr) png_destroy_info_struct(png_ptr, &info_ptr);
  if (png_ptr) png_destroy_read_struct(&png_ptr, &info_ptr,0);
  if (fd) fclose(fd);
  return res; 
}

end-of-c-declare
)

(define (png:info fname idx)
  (png:log 2 "png:info " fname " " idx)
  (let ((res ((c-lambda (char-string int) int "ln_png_info") fname idx)))
    (if (fx= res -1) #f res)))

(define (png-width fname) (png:log 1 "png-width " fname) (png:info fname 1))
(define (png-height fname) (png:log 1 "png-height " fname) (png:info fname 2))
(define (png-depth fname) (png:log 1 "png-depth " fname) (png:info fname 3))
(define (png-stride fname) (png:log 1 "png-stride " fname) (png:info fname 4))

(define (u8vector->png data fname w h)
  (png:log 1 "u8vector->png " w " " h " [] " fname)
  (fx= ((c-lambda (int int scheme-object int char-string) int
           "___result=ln_png_from_u8vector(___arg1,___arg2,___CAST(void*,___BODY_AS(___arg3,___tSUBTYPED)),___arg4,___arg5);")
     w h data (u8vector-length data) fname) 0))

(define (png->u8vector fname . xargs)
  (png:log 1 "png->u8vector " fname " " xargs)
  (let* ((w (png-width fname))
         (h (png-height fname))
         (w0 (if (= (length xargs) 2) (car xargs) w))
         (h0 (if (= (length xargs) 2) (cadr xargs) h))
         (s (png-stride fname))
         (data (if (and w0 h0 s) (make-u8vector (* w0 h0 s) 0) #f)))
    (if data (begin
      (if (fx= ((c-lambda (int int scheme-object int char-string) int 
          "___result=ln_png_to_u8vector(___arg1,___arg2,___CAST(void*,___BODY_AS(___arg3,___tSUBTYPED)),___arg4,___arg5);") 
     w0 h0 data (u8vector-length data) fname) 0) data #f)) #f)))

;; ------
;; opengl related functions 
;; eval is used to delay resolving potentially unavailable calls

(define (png:png->texture fname . xargs)
  (png:log 1 "png->texture " fname " " xargs)
  (let* ((w (png-width fname))
         (h (png-height fname))
         (w0 (if (= (length xargs) 2) (car xargs) w))
         (h0 (if (= (length xargs) 2) (cadr xargs) h))
         (data (png->u8vector fname w0 h0)))
    (if data ((eval 'glCoreTextureCreate) w0 h0 data) #f)))

(define (png->img fname)
  (png:log 1 "png->img " fname)
  (let* ((w (png-width fname))
         (h (png-height fname))
         (w0 (fix (expt 2. (ceiling (/ (log w) (log 2.))))))
         (h0 (fix (expt 2. (ceiling (/ (log h) (log 2.))))))
         (t (png:png->texture fname w0 h0)))
    (if (and w h t)
       (list w h t 0. 0. (/ w w0 1.) (/ h h0 1.)) #f)))

(define (png:texture->png t fname)
  (png:log 1 "texture->png " t " " fname)
  (let ((w ((eval 'glCoreTextureWidth) t))
        (h ((eval 'glCoreTextureHeight) t))
        (data ((eval 'glCoreTextureData) t)))
   (u8vector->png data fname w h)))
 
(define (img->png img fname)
  (png:texture->png (caddr img) fname))

(define (screenshot->png fname)
  (png:log 1 "screenshot->png " fname)
  (let* ((w ((eval 'glgui-width-get)))
         (h ((eval 'glgui-height-get)))
         (data ((eval 'glCoreReadPixels) 0 0 w h)))
    (u8vector->png data fname w h)))

;; ------
;; unit test

(unit-test "png" "1000 random image encode-decode runs"
  (lambda () 
    (let* ((fname (string-append (system-directory) (system-pathseparator) "unittest.png"))
           (res (let loop ((n 1000))
                   (if (fx= n 0) #t (if 
                     (let* ((w (+ 1 (random-integer 200)))
                            (h (+ 1 (random-integer 200)))
                            (stride (list-ref '(1 3 4) (random-integer 3)))
                            (data (random-u8vector (* stride w h))))
                       (u8vector->png data fname w h)
                       (not (and (= w (png-width fname))
                                 (= h (png-height fname))
                                 (= stride (png-stride fname))
                                 (equal? data (png->u8vector fname))))) #f (loop (fx- n 1)))))))
        (if (file-exists? fname) (delete-file fname))
        res)))

;; eof
