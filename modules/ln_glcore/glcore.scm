#|
LambdaNative - a cross-platform Scheme framework
Copyright (c) 2009-2020, University of British Columbia
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
;; Absolutely minimal OpenGL (ES) interface

;;* Compiletime

#| ;; enable manually in source
(define-cond-expand-feature profile)
;;|#

(cond-expand
 (debug
  (define glcore:debuglevel 0)
  (define (glcore:log level . x)
    (if (>= glcore:debuglevel level) (apply log-system (append (list "glcore: " x))))))
 (else))

(cond-expand
 (profile ;; ignore even when otherwise in `debug` mode
  (define-macro (glcore:log . ignored) #!void))
 (debug) ;; defined by previous `debug` expansion
 (else (define-macro (glcore:log . ignored) #!void)))

;;* Runtime

;; ----------------------------------
;; Initialization

(define glCore:customhook #f)
(define (glCore-registerhook h) (set! glCore:customhook h))

(cond-expand
 (android
  (c-declare
   ;; calls GLState.fromNativeInitDraw()
   "extern void microgl_draw_before();")
  (define-macro (microgl-draw-before)
    '((c-lambda () void "microgl_draw_before"))))
 (else
  (define-macro (microgl-draw-before)
    #!void)))

(define glCore:needsinit #t)
(define (glCoreInit)
  (microgl-draw-before)
  (begin
    ;; This block faithful rebuilds the legacy sequence, which was
    ;; done in main.c/microgl_hook before on any EVENT_REDRAW
    (glClearColor 0. 0. 0. 0.)
    (glMatrixMode GL_PROJECTION)
    (glLoadIdentity) ;; ?? Isn't only the last of these actually effective?
    (glOrtho 0. (exact->inexact app:width) 0. (exact->inexact app:height) -1. 1.)
    (glMatrixMode GL_MODELVIEW)
    (glLoadIdentity)
    (glClear GL_COLOR_BUFFER_BIT))
  (if (and glCore:customhook app:width app:height) (begin
    (glDisable GL_BLEND)
    (glCore:customhook)
    (glDisable GL_CULL_FACE)
    (glDisable GL_DEPTH_TEST)
    (set! glCore:needsinit #t)))
  (if glCore:needsinit (begin
    (if (and app:width app:height) (begin
      (glcore:log 5 "glCoreInit")
      ;; suspend/resume might invalidate the textures
      (glCoreTextureReset)
      (glClearColor 0. 0. 0. 0.)
      (glMatrixMode GL_PROJECTION)
      (glLoadIdentity)
      (glOrtho 0. (flo app:width) 0. (flo app:height) -1. 1.)
      (glMatrixMode GL_MODELVIEW)
      (glLoadIdentity)
      (glEnable GL_TEXTURE_2D)
      (glEnable GL_LINE_SMOOTH)
      (glEnableClientState GL_COLOR_ARRAY)
      (glEnableClientState GL_VERTEX_ARRAY)
      (glEnableClientState GL_TEXTURE_COORD_ARRAY)
      (glEnable GL_BLEND)
      (glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA)
      (set! glCore:needsinit #f)
    ) (glcore:log 0 "glCoreInit failed"))
  ))
  (if (not glCore:customhook)  (glClear GL_COLOR_BUFFER_BIT))
  (set! glCore:curtexture -1)
)

;; ----------------------------------
;; Primitive Begin/End wrapper

;; maximum number of vertices
(define glCore:MAX 200)

(define glCore:use3D #f)
(define glCore:type #f)
(define glCore:tarray (##still-copy (make-f32vector (* 2 glCore:MAX))))
(define glCore:varray (##still-copy (make-f32vector (* 2 glCore:MAX))))
(define glCore:varray3D (##still-copy (make-f32vector (* 3 glCore:MAX))))
(define glCore:carray (##still-copy (make-u8vector  (* 4 glCore:MAX))))
(define glCore:cindex 0)
(define glCore:vindex 0)
(define glCore:tindex 0)
(define glCore:red 255)
(define glCore:green 255)
(define glCore:blue 255)
(define glCore:alpha 255)

(define (glCoreColor c)
  (set! glCore:red (color-red c))
  (set! glCore:green (color-green c))
  (set! glCore:blue  (color-blue c))
  (set! glCore:alpha (color-alpha c)))

(define (glCoreBegin type)
    (set! glCore:cindex 0)
    (set! glCore:vindex 0)
    (set! glCore:tindex 0)
    (set! glCore:type type)
 )

(define (glCoreEnd)
  (glcore:log 5 "glCoreEnd")
  (glVertexPointer (if glCore:use3D 3 2) GL_FLOAT 0 (if glCore:use3D glCore:varray3D glCore:varray))
  (glColorPointer 4 GL_UNSIGNED_BYTE 0 glCore:carray)
  (if (or (fx= glCore:type GL_LINES) (fx= glCore:type GL_LINE_LOOP) (fx= glCore:type GL_LINE_STRIP))
    (begin
      (glDisable GL_TEXTURE_2D)
      (glDisableClientState GL_TEXTURE_COORD_ARRAY)
    )
    (begin
      (glEnable GL_TEXTURE_2D)
      (glEnableClientState GL_TEXTURE_COORD_ARRAY)
      (glTexCoordPointer 2 GL_FLOAT 0 glCore:tarray)
    ))
  (glDrawArrays glCore:type 0 (fix (/ glCore:vindex (if glCore:use3D 3 2))))
)

(define (glCoreVertex2f x0 y0 . xtra)
  (let* ((txx (pair? xtra))
         (tx (if txx (flo (car xtra)) 0.5))
         (ty (cond
              ((not txx) 0.5)
              ((let ((r (cdr xtra)))
                 (and (pair? r) (car r))))
              (else 0.5))))
    (let ((x (flo x0)) (y (flo y0)))
      (f32vector-set! glCore:varray (fx+ glCore:vindex 0) x)
      (f32vector-set! glCore:varray (fx+ glCore:vindex 1) y)
      (set! glCore:vindex (fx+ glCore:vindex 2))
      (f32vector-set! glCore:tarray (fx+ glCore:tindex 0) tx)
      (f32vector-set! glCore:tarray (fx+ glCore:tindex 1) ty)
      (set! glCore:tindex (fx+ glCore:tindex 2))
      (u8vector-set! glCore:carray (fx+ glCore:cindex 0) glCore:red)
      (u8vector-set! glCore:carray (fx+ glCore:cindex 1) glCore:green)
      (u8vector-set! glCore:carray (fx+ glCore:cindex 2) glCore:blue)
      (u8vector-set! glCore:carray (fx+ glCore:cindex 3) glCore:alpha)
      (set! glCore:cindex (fx+ glCore:cindex 4))
      (set! glCore:use3D #f)
      )))

;; ------------------------------------------
;; 3D rendering

(define (glCoreVertex3f x0 y0 z0 . xtra)
  (let* ((txx (pair? xtra))
         (tx (if txx (flo (car xtra)) 0.5))
         (ty (cond
              ((not txx) 0.5)
              ((let ((r (cdr xtra)))
                 (and (pair? r) (car r))))
              (else 0.5))))
    (let ((x (flo x0)) (y (flo y0)) (z (flo z0)))
      (f32vector-set! glCore:varray3D (fx+ glCore:vindex 0) x)
      (f32vector-set! glCore:varray3D (fx+ glCore:vindex 1) y)
      (f32vector-set! glCore:varray3D (fx+ glCore:vindex 2) z)
      (set! glCore:vindex (fx+ glCore:vindex 3))
      (f32vector-set! glCore:tarray (fx+ glCore:tindex 0) tx)
      (f32vector-set! glCore:tarray (fx+ glCore:tindex 1) ty)
      (set! glCore:tindex (fx+ glCore:tindex 2))
      (u8vector-set! glCore:carray (fx+ glCore:cindex 0) glCore:red)
      (u8vector-set! glCore:carray (fx+ glCore:cindex 1) glCore:green)
      (u8vector-set! glCore:carray (fx+ glCore:cindex 2) glCore:blue)
      (u8vector-set! glCore:carray (fx+ glCore:cindex 3) glCore:alpha)
      (set! glCore:cindex (fx+ glCore:cindex 4))
      (set! glCore:use3D #t)
      )))

;; ----------------------------------
;; textures

(cond-expand ;; CONSTRUCTION-CASE
 ((or gambit debug) ;; tentative changes
  ;;; intentions:
  ;;; 1. hide globals glCore:textures and glCore:tidx (at least)
  ;;; 2. (short term) replace vector with distinct type

  ;; (: (%%glCore:textures-ref t d) <<== (table-ref [abstract:glCore:textures] t d))
  (define %%glCore:textures-ref)
  ;; glCoreTextureCreate EXPORTED - ubiquitious
  ;;;
  ;;; (: (glCoreTextureCreate w h data #!optional (interpolation GL_LINEAR) (wrap GL_CLAMP))
  ;;;    -> fixnum)
  (define glCoreTextureCreate)
  ;; glCoreTextureReset -- TBD: unknown usage status
  ;;;
  ;;; (: glCoreTextureReset -> undefined)
  ;;;
  ;;; purpose: clear resources
  (define glCoreTextureReset)

  ;; Implementation (volatile)

  (define-type glCore:texture
    macros: prefix: %MATURITY+3%texture%macro-
    %%valid ;; FIXME: factor out from immutable components
    glidx   ;; index (for opengl and internal table)
    %%-???-u32vector ;; what is this? mutable?
    width
    height
    (%%-???-u8vector:data unprintable:)
    pixeltype
    interpolation
    wrap
    )

  (define (glCore:texture? x) (%MATURITY+3%texture%macro-glCore:texture? x)) ;; avoid eventually!

  (define (glCore:texture-valid? texture)
    (%MATURITY+3%texture%macro-glCore:texture-%%valid texture))

  (define (glCore:texture-invalidate! texture)
    (%MATURITY+3%texture%macro-glCore:texture-%%valid-set! texture #f))

  (define (glCore:texture-valid! texture)
    (%MATURITY+3%texture%macro-glCore:texture-%%valid-set! texture #t))

  (define (glCore:texture-%%-???-u32vector texture) ;; was vector-ref t 1
    (%MATURITY+3%texture%macro-glCore:texture-%%-???-u32vector texture))

  (define (glCore:texture-width texture)
    (%MATURITY+3%texture%macro-glCore:texture-width texture))

  (define (glCore:texture-height texture)
    (%MATURITY+3%texture%macro-glCore:texture-height texture))

  (define (glCore:texture-data texture)
    (%MATURITY+3%texture%macro-glCore:texture-%%-???-u8vector:data texture))

  (define (glCore:texture-pixeltype texture)
    (%MATURITY+3%texture%macro-glCore:texture-pixeltype texture))

  (define (glCore:texture-pixeltype-set! texture)
    (%MATURITY+3%texture%macro-glCore:texture-pixeltype texture))

  (define (glCore:texture-interpolation texture)
    (%MATURITY+3%texture%macro-glCore:texture-interpolation texture))

  (define (glCore:texture-interpolation-set! texture)
    (%MATURITY+3%texture%macro-glCore:texture-interpolation texture))

  (define (glCore:texture-wrap texture)
    (%MATURITY+3%texture%macro-glCore:texture-wrap texture))

  (define (glCore:texture-wrap-set! texture)
    (%MATURITY+3%texture%macro-glCore:texture-wrap texture))

  (let (;; TBD: not thread safe, assert exclusive access at least in debug
        (glCore:textures (make-table))
        (glCore:tidx 0)
        ;; TBD: now never using ##still-copy
        (maturity:use-still-copy/-1 (if #f ##still-copy identity)))

    ;; ?? should we use `(##still-copy (make-table))` for glCore:textures?
    (define (glCore:textures-ref texture default)
      (if (%MATURITY+3%texture%macro-glCore:texture? texture)
          texture
          (table-ref glCore:textures texture default)))

    (define (%%glCoreTextureCreate w h data #!optional (interpolation GL_LINEAR) (wrap GL_CLAMP))
      ;; (glcore:log 5 "glCoreTextureCreate")
      #;(MATURITY -1 "legacy; TBD: ensure resources are actually released" 'glCoreTextureCreate)
      (let ((idx glCore:tidx)
            (pixeltype
             (cond
              ((fx= (u8vector-length data) (* w h)) GL_ALPHA)
              ((fx= (u8vector-length data) (* 3 w h)) GL_RGB)
              ((fx= (u8vector-length data) (* 4 w h)) GL_RGBA)
              (else (log-error "glCoreTextureCreate: Invalid data range") #f))))
        (table-set!
         glCore:textures idx
         (%MATURITY+3%texture%macro-make-glCore:texture
          #f ;; volatile
          idx
          (u32vector 0) ;; unknown
          w h ;; 2d interval
          (maturity:use-still-copy/-1 data)
          pixeltype interpolation wrap))
        (set! glCore:tidx (fx+ glCore:tidx 1))
        idx))

    ;; clear all textures
    (define (%%glCoreTextureReset!)
      (table-for-each
       (lambda (k entry)
         (when (glCore:texture-valid? entry)
           (glDeleteTextures 1 (%MATURITY+3%texture%macro-glCore:texture-%%-???-u32vector entry))
           (glCore:texture-invalidate! entry)))
       glCore:textures)
      (when #f  ;; should we clean references too?
        (set! glCore:textures (make-table))
        (set! glCore:tidx 0)))

    (unless glCore:textures (%%reset!))

    (set! glCoreTextureReset %%glCoreTextureReset!)
    (set! %%glCore:textures-ref glCore:textures-ref)
    (set! glCoreTextureCreate %%glCoreTextureCreate))

  (define glCore:curtexture -1) ;; deprecated but required

  ) ;; end of tentative changes
 (else ;; old version

  ;; each entry is a vector of initflag,texure,w,h,u8data,pixeltype
  (define glCore:textures (##still-copy (make-table)))
  (define glCore:tidx 0)
  (define glCore:curtexture -1)
  ;; forward compatible replacements
  (define (%%glCore:textures-ref texture default)
    (table-ref glCore:textures texture default))

  (define (glCore:texture-valid? texture)
    (vector-ref texture 0))

  (define (glCore:texture-invalidate! texture)
    (vector-set! texture 0 #f))

  (define (glCore:texture-valid! texture)
    (vector-set! texture 0 #t))

  (define (glCore:texture-%%-???-u32vector texture) ;; was vector-ref t 1
    (vector-ref texture 1))

  (define (glCore:texture-width texture)
    (vector-ref texture 2))

  (define (glCore:texture-height texture)
    (vector-ref texture 3))

  (define (glCore:texture-data texture)
    (vector-ref texture 4))

  (define (glCore:texture-pixeltype texture)
    (vector-ref texture 5))

  (define (glCore:texture-interpolation texture)
    (vector-ref texture 6))

  (define (glCore:texture-wrap texture)
    (vector-ref texture 7))

  (define (glCoreTextureCreate w h data . aux)
    (glcore:log 5 "glCoreTextureCreate")
    (let* ((o1x (pair? aux))
           (o2 (and o1x (cdr aux))))
      (let ((idx glCore:tidx)
            (pixeltype
             (cond
              ((fx= (u8vector-length data) (* w h)) GL_ALPHA)
              ((fx= (u8vector-length data) (* 3 w h)) GL_RGB)
              ((fx= (u8vector-length data) (* 4 w h)) GL_RGBA)
              (else (log-error "glCoreTextureCreate: Invalid data range") #f)))
            (interpolation (if o1x (car aux) GL_LINEAR))
            (wrap (if (pair? o2) (car o2) GL_CLAMP)))
        (table-set!
         glCore:textures idx
         (##still-copy
          (vector #f (u32vector 0) w h (##still-copy data) pixeltype interpolation wrap)))
        (set! glCore:tidx (fx+ glCore:tidx 1))
        idx)))
  ;; reset a texture entry
  (define (_glCoreTextureReset t)
    (glcore:log 5 "_glCoreTextureReset")
    (let ((entry (%%glCore:textures-ref t #f)))
      (if (and entry (glCore:texture-valid? entry))
          (begin
            (glDeleteTextures 1 (glCore:texture-%%-???-u32vector entry))
            (glCore:texture-invalidate! entry)))))

  ;; clear all textures
  (define (glCoreTextureReset)
    (glcore:log 5 "glCoreTextureReset")
    (let ((tlist '())) ;; collect list of entries
      ;;;
      ;;; Jikes: by ... no way!
      (table-for-each (lambda (k v) (set! tlist (append tlist (list k)))) glCore:textures)
      (for-each (lambda (t) (_glCoreTextureReset t)) tlist)))

  ) ;; end of old version
 ) ;; end of CONSTRUCTION-CASE


(define (glCore:textures-ref
         num #!optional
         (failure (lambda (num) (error "glCore:textures-ref: unbound index" num))))
  (cond
   ((fixnum? num) (or (%%glCore:textures-ref num #f) (failure num)))
   (else (error "not a fixnum" num glCore:textures-ref))))

;; return texture width
(define (glCoreTextureWidth t)
  (glcore:log 5 "glCoreTextureWidth")
  (let ((entry (%%glCore:textures-ref t #f)))
    (if entry (glCore:texture-width entry) (begin
      (log-error "glCoreTextureWidth: unbound index " t) #f))))

;; return texture height
(define (glCoreTextureHeight t)
  (glcore:log 5 "glCoreTextureWidth")
  (let ((entry (%%glCore:textures-ref t #f)))
    (if entry (glCore:texture-height entry) (begin
      (log-error "glCoreTextureHeight: unbound index " t) #f))))

;; return texture data
(define (glCoreTextureData t)
  (glcore:log 5 "glCoreTextureData")
  (let ((entry (%%glCore:textures-ref t #f)))
    (if entry (glCore:texture-data entry) (begin
      (log-error "glCoreTextureData: unbound index " t) #f))))

;; %%%%%%%%%%%%%%%%%%%%
;; clip stack

(define glcore:cliplist '())
(define glcore:clipx1 0)
(define glcore:clipx2 0)
(define glcore:clipy1 0)
(define glcore:clipy2 0)

;; (glCoreClipPush x1 y1 x2 y2)
(define (glCoreClipPush . coords)
  (let* ((oldlist glcore:cliplist)
         (newcoords
          (if (fx= (length coords) 4)
              (map lambdanative#flo
                   (list (min (car coords) (caddr coords))
                         (min (cadr coords) (cadddr coords))
                         (max (car coords) (caddr coords))
                         (max (cadr coords) (cadddr coords))))
              #f))
         (newlist (if newcoords
                      (append (list newcoords) oldlist)
                      (if (null? oldlist) oldlist (cdr oldlist)))))
    (if (not (null? newlist))
        (begin
          (set! glcore:clipx1 (car (car newlist)))
          (set! glcore:clipy1 (cadr (car newlist)))
          (set! glcore:clipx2 (caddr (car newlist)))
          (set! glcore:clipy2 (cadddr (car newlist)))))
    (set! glcore:cliplist newlist)))

(define glCoreClipPop glCoreClipPush)

;; %%%%%%%%%%%%%%%%%%%%
;; texture draw with support for basic clipping
;; current clipping limitations: rotation & color gradient interpolation
;; 				 polygons are not clipped at all

(define (glCoreTextureDraw x y w0 h0 t x1 y1 x2 y2 r . colors)
  (let ((entry (%%glCore:textures-ref t #f)))
    (if entry
        (let ((w (flo (if (fx= (fix w0) 0) (glCore:texture-width entry) w0)))
              (h (flo (if (fx= (fix h0) 0) (glCore:texture-height entry) h0))))
          (if (null? glcore:cliplist)
              (if (pair? colors)
                  (glCore:TextureDrawUnClipped
                   (flo x) (flo y) w h t (flo x1) (flo y1) (flo x2) (flo y2) (flo r)
                   (car colors))
                  (glCore:TextureDrawUnClipped
                   (flo x) (flo y) w h t (flo x1) (flo y1) (flo x2) (flo y2) (flo r)))
              (if (pair? colors)
                  (glCore:TextureDrawClipped
                   (flo x) (flo y) w h t (flo x1) (flo y1) (flo x2) (flo y2) (flo r)
                   (car colors))
                  (glCore:TextureDrawClipped
                   (flo x) (flo y) w h t (flo x1) (flo y1) (flo x2) (flo y2) (flo r)))))
        (log-error "glCoreTextureDraw: unbound index " t))))

(define (glCore:TextureDrawUnClipped x y w h t @x1 @y1 @x2 @y2 r . colors)
  (glcore:log 5 "glCoreTextureDrawUnclipped enter")
  (let ((w2 (fl/ w 2.)) (h2 (fl/ h 2.)))
    (glPushMatrix)
    (glTranslatef (fl+ x w2) (fl+ y h2) 0.)
    (glRotatef r 0. 0. 1.)
    (_glCoreTextureBind t)
    (glCoreBegin GL_TRIANGLE_STRIP)
    (if (null? colors)
        (begin
          (glCoreVertex2f (fl- w2) h2 @x1 @y2)
          (glCoreVertex2f w2 h2 @x2 @y2)
          (glCoreVertex2f (fl- w2) (fl- h2) @x1 @y1)
          (glCoreVertex2f w2 (fl- h2) @x2 @y1)
          )
        (let ((colors (list->vector (car colors))))
          (glCoreColor (vector-ref colors 0))
          (glCoreVertex2f (fl- w2) h2 @x1 @y2)
          (glCoreColor (vector-ref colors 1))
          (glCoreVertex2f w2 h2 @x2 @y2)
          (glCoreColor (vector-ref colors 2))
          (glCoreVertex2f (fl- w2) (fl- h2) @x1 @y1)
          (glCoreColor (vector-ref colors 3))
          (glCoreVertex2f w2 (fl- h2) @x2 @y1)
          ))
    (glCoreEnd)
    (glPopMatrix)
    )
  (glcore:log 5 "glCoreTextureDrawUnclipped leave")
  )

(define (glCore:TextureDrawClipped x y w h t @x1 @y1 @x2 @y2 r . colors)
  (if (and (fl< x glcore:clipx2) (fl> (fl+ x w) glcore:clipx1)
           (fl< y glcore:clipy2) (fl> (fl+ y h) glcore:clipy1))
      (let* ((cx1 (flmax x glcore:clipx1))
             (cx2 (flmin (fl+ x w) glcore:clipx2))
             (cy1 (flmax y glcore:clipy1))
             (cy2 (flmin (fl+ y h) glcore:clipy2))
             (cw (fl- cx2 cx1))
             (ch (fl- cy2 cy1))
             (cw2 (fl/ cw 2.))
             (ch2 (fl/ ch 2.))
             (c@x1 (fl+ (fl* (fl/ (fl- cx1 x) w) (fl- @x2 @x1)) @x1))
             (c@x2 (fl+ (fl* (fl/ (fl- cx2 x) w) (fl- @x2 @x1)) @x1))
             (c@y1 (fl+ (fl* (fl/ (fl- cy1 y) h) (fl- @y2 @y1)) @y1))
             (c@y2 (fl+ (fl* (fl/ (fl- cy2 y) h) (fl- @y2 @y1)) @y1)))
        (glPushMatrix)
        (glTranslatef (fl+ cx1 cw2) (fl+ cy1 ch2) 0.)
        (glRotatef r 0. 0. 1.)
        (_glCoreTextureBind t)
        (glCoreBegin GL_TRIANGLE_STRIP)
        (if (null? colors)
            (begin
              (glCoreVertex2f (fl- cw2) ch2 c@x1 c@y2)
              (glCoreVertex2f cw2 ch2 c@x2 c@y2)
              (glCoreVertex2f (fl- cw2) (fl- ch2) c@x1 c@y1)
              (glCoreVertex2f cw2 (fl- ch2) c@x2 c@y1)
              )
            (let ((colors (list->vector (car colors))))
              ;; TODO: color interpolation here!
              (glCoreColor (vector-ref colors 0))
              (glCoreVertex2f (fl- cw2) ch2 c@x1 c@y2)
              (glCoreColor (vector-ref colors 1))
              (glCoreVertex2f cw2 ch2 c@x2 c@y2)
              (glCoreColor (vector-ref colors 2))
              (glCoreVertex2f (fl- cw2) (fl- ch2) c@x1 c@y1)
              (glCoreColor (vector-ref colors 3))
              (glCoreVertex2f cw2 (fl- ch2) c@x2 c@y1)
              ))
        (glCoreEnd)
        (glPopMatrix)
        )))

(define glCoreTextureGradientDraw glCoreTextureDraw)

;; %%%%%%%%%%%%%%%%%%%%

;; draw a texture
(define (glCoreTexturePolygonDraw _cx _cy points t _r)
  (glcore:log 5 "glCoreTexturePolygonDraw")
  (let ((entry (%%glCore:textures-ref t #f)))
    (if entry
        (let* ((cx (flo _cx)) (cy (flo _cy)) (r (flo _r)))
          (glPushMatrix)
          (glTranslatef cx cy 0.)
          (glRotatef r 0. 0. 1.)
          (_glCoreTextureBind t)
          (glCoreBegin GL_TRIANGLE_STRIP)
          (for-each
           (lambda (p)
             ;; TBD: should accept vectoralikes as point
             (let* ((p (list->vector p))
                    (x (fl- (vector-ref p 0) cx))
                    (y (fl- (vector-ref p 1) cy))
                    (tx (vector-ref p 2))
                    (ty (vector-ref p 3)))
               (glCoreVertex2f x y tx ty)))
           points)
          (glCoreEnd)
          (glPopMatrix))
        (log-error "glCoreTexturePolygonDraw: unbound index " t))))

;; update texture data (for dynamic textures)
;; to use this, first modify data returned with glCoreTextureData..
(define (glCoreTextureUpdate t)
  (glcore:log 5 "glCoreTextureUpdate")
  (if (fixnum? t) (set! t (%%glCore:textures-ref t #f)))
  (_glCoreTextureBind t) ;; select the texture as current
  (let ((entry t))
    (let ((w (glCore:texture-width entry))
          (h (glCore:texture-height entry))
          (data (glCore:texture-data entry))
          (pixeltype (glCore:texture-pixeltype entry)))
      (glTexSubImage2D GL_TEXTURE_2D 0 0 0 w h pixeltype GL_UNSIGNED_BYTE data))))

(define (%%glCoreTextureInit! texture) ;; texture structure
  (let ((u32t (glCore:texture-%%-???-u32vector texture))
        (w (glCore:texture-width texture))
        (h (glCore:texture-height texture))
        (data (glCore:texture-data texture))
        (pixeltype (glCore:texture-pixeltype texture))
        (interp (glCore:texture-interpolation texture))
        (wrap  (glCore:texture-wrap texture)))
    (glGenTextures 1 u32t)
    (if (or (= (u32vector-ref u32t 0) GL_INVALID_VALUE)
            ;; this is a general check that gl is working in this thread
            (= (glIsEnabled GL_TEXTURE_2D) 0))
        (glcore:log 5 "_glCoreTextureInit: failed to generate texture")
        (begin
          (glCore:texture-valid! texture) ;; mark as initialized
          (glBindTexture GL_TEXTURE_2D (u32vector-ref u32t 0))
          (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER interp)
          (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER interp)
          (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S wrap)
          (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T wrap)
          (glTexImage2D GL_TEXTURE_2D 0 pixeltype w h 0 pixeltype GL_UNSIGNED_BYTE data)))))

(define (_glCoreTextureBind t)
  (glcore:log 5 "_glCoreTextureBind")
  (let ((entry (if (fixnum? t) (%%glCore:textures-ref t #f) t)))
    (if entry
        (begin
          (unless (glCore:texture-valid? entry)
            ;; TBD: maybe move into the reference operation?
            (%%glCoreTextureInit! entry))
          (let ((tx (u32vector-ref (glCore:texture-%%-???-u32vector entry) 0)))
            (if (not (= glCore:curtexture tx))
                (begin
                  (glBindTexture GL_TEXTURE_2D tx)
                  (set! glCore:curtexture tx)))))
        (log-error "glCoreTextureBind: unbound index " t))))

(define (_glCoreTextureInit t)
  (glcore:log 5 "_glCoreTextureInit")
  (unless (fixnum? t) (error "_glCoreTextureInit: wrong argument type"))
  (let ((entry (%%glCore:textures-ref t #f)))
    (if entry (%%glCoreTextureInit! entry)
        (log-error "_glCoreTextureInit: unknown index " t))))

;; take screen shot
(define (glCoreReadPixels x y w h)
  (let* ((data (make-u8vector (* w h 3) 0)))
    (glReadPixels x y w h GL_RGB GL_UNSIGNED_BYTE data)
    data))

;; eof
