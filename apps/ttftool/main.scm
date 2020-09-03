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

;; this is a tool used by the lambdanative build system

(c-declare  #<<end-of-c-declare

#include <stdio.h>
#include <stdlib.h>
#include <gd.h>

int stringfile(char *fnt, char *fntsize, char *str, char *fname)
{
  double size = (double)atol(fntsize)*0.75; // To match Latex size scale down

  int brect[8];
  char *err = gdImageStringFT(NULL,&brect[0],0,fnt,size,0.,0,0,str);
  if (err){
    fprintf(stderr,"Error: %s\n",err);
    return 1;
  }

  // Add 1px of space around everywhere.
  int x = brect[2]-brect[6] + 2;
  int y = brect[3]-brect[7] + 2;
  gdImagePtr im = gdImageCreate(x,y);
  // Need to allocate black as background color and transparent
  int black = gdImageColorResolveAlpha(im, 0, 0, 0, 127);
  int white = gdImageColorResolve(im, 255, 255, 255);
  x = 1 - brect[6];
  y = 1 - brect[7];
  err = gdImageStringFT(im,&brect[0],white,fnt,size,0.0,x,y,str);
  if (err){
    fprintf(stderr,"Error: %s\n",err);
    return 1;
  }

  // Save the file and exit
  FILE *out = fopen(fname, "wb");
  if (out == NULL){ 
    fprintf(stderr,"Error: Cannot open %s\n",fname);
    return 1;
  }
  gdImagePng(im, out);
  fclose(out);
  gdImageDestroy(im);
  return 0;
}

end-of-c-declare
)

(define (usage)
  (for-each display (list
    "Usage: ttftool [stringfile <ttffont> <fontsize> <string> <pngfile> | fontname <ttffont> |
                fnt2scm <ttffont> <bits> <sizes> <fontname>]\n"
    "stringfile: generate pngfile with string rendered with specified font & size\n"
    "fontname: output name of font\n"
    "fnt2scm: output scheme font atlas representation\n"))
  (exit))

(define (glyph-set file)
  (with-input-from-file file (lambda ()
    (let loop ((res '()))
      (let ((line (read-line)))
        (if (not (string? line)) res 
          (let ((newentry 
                  (if (and (> (string-length line) 2) 
                           (char=? (string-ref line 0) #\U))
                    (let ((data (string-split (substring line 2 (string-length line)) #\-)))
                       (if (= (length data) 1) (list (string->number (substring (car data) 0 4) 16))
                          (let ((a (string->number (car data) 16))
                                (b (string->number (substring (cadr data) 0 4) 16)))
                            (let loop2 ((n a)(lst '()))
                              (if (> n b) lst (loop2 (+ n 1) (append lst (list n))))))
                       )) '())))
            (loop (append res newentry)))))))))

(cond
  ((and (= (system-cmdargc) 3)
        (string=? (system-cmdargv 1) "fontname"))
     (display (ttf-name (system-cmdargv 2))) (newline))
  ((and (= (system-cmdargc) 6)
        (string=? (system-cmdargv 1) "stringfile"))
      ((c-lambda (char-string char-string char-string char-string) int "stringfile")
         (system-cmdargv 2) (system-cmdargv 3) (system-cmdargv 4) (system-cmdargv 5)))
  ((and (= (system-cmdargc) 6)
         (string=? (system-cmdargv 1) "fnt2scm"))
      (let ((font (system-cmdargv 2))
            (bits (glyph-set (system-cmdargv 3)))
            (sizes (map string->number (string-split (system-cmdargv 4) #\,)))
            (name (system-cmdargv 5)))
        (ttf-compile font sizes bits name)
      ))
  (else (usage)))

;; eof
