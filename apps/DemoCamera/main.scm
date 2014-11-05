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

;; camera example

(define gui #f)

(define background #f)
(define default:background (list 4 4 (glCoreTextureCreate 4 4 (make-u8vector 16 #xff)) 0.1 0.1 .9 .9))

(define camera-image (string-append (system-directory) (system-pathseparator) "camera.jpg"))
(define camera-image2 (string-append (system-directory) (system-pathseparator) "camera.png"))

(define lastmodtime 0.)

;; look for a new jpeg from the camera, create a downsampled png, and load that as a texture

(define (autoload)
  (let* ((fileinfo (if (file-exists? camera-image) (file-info camera-image) #f))
         (modtime  (if fileinfo (time->seconds (file-info-last-modification-time fileinfo)) #f)))
    (if (and gui background modtime (> modtime lastmodtime))
        (let* ((gdf (gdFileOpen camera-image "r"))
               (gd  (gdImageCreateFromJpeg gdf))
               (w   (gdImageSX gd))
               (h   (gdImageSY gd))
               (w2  256)
               (h2  (fix (* w2 (/ h w))))
               (gdf2 (gdFileOpen camera-image2 "w"))
               (gd2 (gdImageCreateTrueColor w2 h2)))
         (gdImageCopyResampled gd2 gd 0 0 0 0 w2 h2 w h)
         (gdImagePng gd2 gdf2)
         (gdImageDestroy gd)
         (gdImageDestroy gd2)
         (gdFileClose gdf)
         (gdFileClose gdf2)
         (let ((img (if (file-exists? camera-image2) (png->img camera-image2) #f)))
           (glgui-widget-set! gui background 'image (if img img default:background)))
         (set! lastmodtime modtime)
      ))))

(main
;; initialization
  (lambda (w h)
    (make-window 320 480)
    (glgui-orientation-set! GUI_PORTRAIT)
    (set! gui (make-glgui))
    (let ((w (glgui-width-get))
          (h (glgui-height-get)))
      (set! background (glgui-pixmap gui 0 0 default:background w h))
      (let* ((bw 150) (bh 50)
            (bx (/ (- w bw) 2.))
            (by (/ (- h bh) 2.)))
        (glgui-button-string gui bx by bw bh "Take Picture" ascii_18.fnt 
          (lambda (un . used) (camera-start camera-image))))
    )
    (if (file-exists? camera-image) (delete-file camera-image))
    (if (file-exists? camera-image2) (delete-file camera-image2))
    (let ((logdir (string-append (system-directory) "/log")))
      (if (not (file-exists? logdir)) (create-directory logdir)))
  )
;; events
  (lambda (t x y) 
    (autoload)
    (if (= t EVENT_KEYPRESS) (begin 
      (if (= x EVENT_KEYESCAPE) (terminate))))
    (glgui-event gui t x y))
;; termination
  (lambda () #t)
;; suspend
  (lambda () (glgui-suspend))
;; resume
  (lambda () (glgui-resume))
)

;; eof
