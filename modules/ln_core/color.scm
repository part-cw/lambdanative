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
;; color related code

;; ----- ENDIAN DEPENDENT, fix as needed ..
;; this is little endian..
(define (color-alpha x) (bitwise-and (arithmetic-shift x -24) #xff))
(define (color-blue x) (bitwise-and (arithmetic-shift x -16) #xff))
(define (color-green x) (bitwise-and (arithmetic-shift x -8) #xff))
(define (color-red x) (bitwise-and x #xff))
(define (color-rgba r g b a)
  (let* ((fixr (macro-fix r)) (fixg (macro-fix g)) (fixb (macro-fix b)) (fixa (macro-fix a))
         (clipr (if (fx> fixr 255) 255 (if (fx< fixr 0) 0 fixr)))
         (clipg (if (fx> fixg 255) 255 (if (fx< fixg 0) 0 fixg)))
         (clipb (if (fx> fixb 255) 255 (if (fx< fixb 0) 0 fixb)))
         (clipa (if (fx> fixa 255) 255 (if (fx< fixa 0) 0 fixa))))
  (bitwise-ior (arithmetic-shift (bitwise-and clipa #xff) 24)
       (arithmetic-shift (bitwise-and clipb #xff) 16) 
       (arithmetic-shift (bitwise-and clipg #xff) 8) (bitwise-and clipr #xff))))
;; ----- 

;; shuffle a 32bit RGBA to proper endianness 
(define (color:shuffle x)
  (let ((r (bitwise-and (arithmetic-shift x -24) #xff))
        (g (bitwise-and (arithmetic-shift x -16) #xff))
        (b (bitwise-and (arithmetic-shift x -8) #xff))
        (a (bitwise-and x #xff)))
    (color-rgba r g b a)))

;; X11 color names in RGBA format
(define AliceBlue 		(color:shuffle #xf0f8ffff))
(define AntiqueWhite 		(color:shuffle #xfaebd7ff))
(define Aquamarine 		(color:shuffle #x7fffd4ff))
(define Azure 			(color:shuffle #xf0ffffff))
(define Beige 			(color:shuffle #xf5f5dcff))
(define Bisque 			(color:shuffle #xffe4c4ff))
(define Black 			(color:shuffle #x000000ff))
(define BlanchedAlmond 		(color:shuffle #xffebcdff))
(define Blue 			(color:shuffle #x0000ffff))
(define BlueViolet 		(color:shuffle #x8a2be2ff))
(define Brown 			(color:shuffle #xa52a2aff))
(define Burlywood 		(color:shuffle #xdeb887ff))
(define CadetBlue 		(color:shuffle #x5f9ea0ff))
(define Chartreuse 		(color:shuffle #x7fff00ff))
(define Chocolate 		(color:shuffle #xd2691eff))
(define Coral 			(color:shuffle #xff7f50ff))
(define CornflowerBlue 		(color:shuffle #x6495edff))
(define Cornsilk 		(color:shuffle #xfff8dcff))
(define Cyan 			(color:shuffle #x00ffffff))
(define DarkBlue 		(color:shuffle #x00008bff))
(define DarkCyan 		(color:shuffle #x008b8bff))
(define DarkGoldenrod 		(color:shuffle #xb8860bff))
(define DarkGray 		(color:shuffle #xa9a9a9ff))
(define DarkGreen 		(color:shuffle #x006400ff))
(define DarkGrey 		(color:shuffle #xa9a9a9ff))
(define DarkKhaki 		(color:shuffle #xbdb76bff))
(define DarkMagenta 		(color:shuffle #x8b008bff))
(define DarkOliveGreen 		(color:shuffle #x556b2fff))
(define DarkOrange 		(color:shuffle #xff8c00ff))
(define DarkOrchid 		(color:shuffle #x9932ccff))
(define DarkRed 		(color:shuffle #x8b0000ff))
(define DarkSalmon 		(color:shuffle #xe9967aff))
(define DarkSeaGreen 		(color:shuffle #x8fbc8fff))
(define DarkSlateBlue 		(color:shuffle #x483d8bff))
(define DarkSlateGray 		(color:shuffle #x2f4f4fff))
(define DarkSlateGrey 		(color:shuffle #x2f4f4fff))
(define DarkTurquoise 		(color:shuffle #x00ced1ff))
(define DarkViolet 		(color:shuffle #x9400d3ff))
(define DeepPink 		(color:shuffle #xff1493ff))
(define DeepSkyBlue 		(color:shuffle #x00bfffff))
(define DimGray 		(color:shuffle #x696969ff))
(define DimGrey 		(color:shuffle #x696969ff))
(define DodgerBlue 		(color:shuffle #x1e90ffff))
(define Firebrick 		(color:shuffle #xb22222ff))
(define FloralWhite 		(color:shuffle #xfffaf0ff))
(define ForestGreen 		(color:shuffle #x228b22ff))
(define Gainsboro 		(color:shuffle #xdcdcdcff))
(define GhostWhite 		(color:shuffle #xf8f8ffff))
(define Gold 			(color:shuffle #xffd700ff))
(define Goldenrod 		(color:shuffle #xdaa520ff))
(define Gray 			(color:shuffle #xbebebeff))
(define Green 			(color:shuffle #x00ff00ff))
(define GreenYellow 		(color:shuffle #xadff2fff))
(define Grey 			(color:shuffle #xbebebeff))
(define Honeydew 		(color:shuffle #xf0fff0ff))
(define HotPink 		(color:shuffle #xff69b4ff))
(define IndianRed 		(color:shuffle #xcd5c5cff))
(define Indigo 			(color:shuffle #x4b0082ff))
(define Ivory 			(color:shuffle #xfffff0ff))
(define Khaki 			(color:shuffle #xf0e68cff))
(define Lavender 		(color:shuffle #xe6e6faff))
(define LavenderBlush 		(color:shuffle #xfff0f5ff))
(define LawnGreen 		(color:shuffle #x7cfc00ff))
(define LemonChiffon 		(color:shuffle #xfffacdff))
(define LightBlue 		(color:shuffle #xadd8e6ff))
(define LightCoral 		(color:shuffle #xf08080ff))
(define LightCyan 		(color:shuffle #xe0ffffff))
(define LightGoldenrod 		(color:shuffle #xeedd82ff))
(define LightGoldenrodYellow 	(color:shuffle #xfafad2ff))
(define LightGray 		(color:shuffle #xd3d3d3ff))
(define LightGreen 		(color:shuffle #x90ee90ff))
(define LightGrey 		(color:shuffle #xd3d3d3ff))
(define LightPink 		(color:shuffle #xffb6c1ff))
(define LightSalmon 		(color:shuffle #xffa07aff))
(define LightSeaGreen 		(color:shuffle #x20b2aaff))
(define LightSkyBlue 		(color:shuffle #x87cefaff))
(define LightSlateBlue 		(color:shuffle #x8470ffff))
(define LightSlateGray 		(color:shuffle #x778899ff))
(define LightSlateGrey 		(color:shuffle #x778899ff))
(define LightSteelBlue 		(color:shuffle #xb0c4deff))
(define LightYellow 		(color:shuffle #xffffe0ff))
(define LimeGreen 		(color:shuffle #x32cd32ff))
(define Linen 			(color:shuffle #xfaf0e6ff))
(define Magenta 		(color:shuffle #xff00ffff))
(define Maroon 			(color:shuffle #xb03060ff))
(define MediumAquamarine 	(color:shuffle #x66cdaaff))
(define MediumBlue 		(color:shuffle #x0000cdff))
(define MediumOrchid 		(color:shuffle #xba55d3ff))
(define MediumPurple 		(color:shuffle #x9370dbff))
(define MediumSeaGreen 		(color:shuffle #x3cb371ff))
(define MediumSlateBlue	 	(color:shuffle #x7b68eeff))
(define MediumSpringGreen 	(color:shuffle #x00fa9aff))
(define MediumTurquoise		(color:shuffle #x48d1ccff))
(define MediumVioletRed 	(color:shuffle #xc71585ff))
(define MidnightBlue 		(color:shuffle #x191970ff))
(define MintCream 		(color:shuffle #xf5fffaff))
(define MistyRose 		(color:shuffle #xffe4e1ff))
(define Moccasin 		(color:shuffle #xffe4b5ff))
(define NavajoWhite 		(color:shuffle #xffdeadff))
(define Navy 			(color:shuffle #x000080ff))
(define NavyBlue 		(color:shuffle #x000080ff))
(define OldLace 		(color:shuffle #xfdf5e6ff))
(define OliveDrab 		(color:shuffle #x6b8e23ff))
(define Orange 			(color:shuffle #xffa500ff))
(define OrangeRed 		(color:shuffle #xff4500ff))
(define Orchid 			(color:shuffle #xda70d6ff))
(define PaleGoldenrod 		(color:shuffle #xeee8aaff))
(define PaleGreen 		(color:shuffle #x98fb98ff))
(define PaleTurquoise 		(color:shuffle #xafeeeeff))
(define PaleVioletRed 		(color:shuffle #xdb7093ff))
(define PapayaWhip 		(color:shuffle #xffefd5ff))
(define PeachPuff 		(color:shuffle #xffdab9ff))
(define Peru 			(color:shuffle #xcd853fff))
(define Pink 			(color:shuffle #xffc0cbff))
(define Plum 			(color:shuffle #xdda0ddff))
(define PowderBlue 		(color:shuffle #xb0e0e6ff))
(define Purple 			(color:shuffle #xa020f0ff))
(define Red 			(color:shuffle #xff0000ff))
(define RosyBrown 		(color:shuffle #xbc8f8fff))
(define RoyalBlue 		(color:shuffle #x4169e1ff))
(define SaddleBrown 		(color:shuffle #x8b4513ff))
(define Salmon 			(color:shuffle #xfa8072ff))
(define SandyBrown 		(color:shuffle #xf4a460ff))
(define SeaGreen 		(color:shuffle #x2e8b57ff))
(define Seashell 		(color:shuffle #xfff5eeff))
(define Sienna 			(color:shuffle #xa0522dff))
(define SkyBlue 		(color:shuffle #x87ceebff))
(define SlateBlue 		(color:shuffle #x6a5acdff))
(define SlateGray 		(color:shuffle #x708090ff))
(define SlateGrey 		(color:shuffle #x708090ff))
(define Snow			(color:shuffle #xfffafaff))
(define SpringGreen 		(color:shuffle #x00ff7fff))
(define SteelBlue 		(color:shuffle #x4682b4ff))
(define Tan 			(color:shuffle #xd2b48cff))
(define Thistle 		(color:shuffle #xd8bfd8ff))
(define Tomato 			(color:shuffle #xff6347ff))
(define Turquoise 		(color:shuffle #x40e0d0ff))
(define Violet 			(color:shuffle #xee82eeff))
(define VioletRed 		(color:shuffle #xd02090ff))
(define Wheat 			(color:shuffle #xf5deb3ff))
(define White 			(color:shuffle #xffffffff))
(define WhiteSmoke 		(color:shuffle #xf5f5f5ff))
(define Yellow 			(color:shuffle #xffff00ff))
(define YellowGreen 		(color:shuffle #x9acd32ff))

(define (color-rgb r g b) (color-rgba r g b #xff))
(define (color-rgbf r g b) (color-rgb (* 255. r) (* 255. g) (* 255. b)))
(define (color-rgbaf r g b a) (color-rgba (* 255. r) (* 255. g) (* 255. b) (* 255. a)))

(define GRADIENT_GRAY 0)
(define GRADIENT_GREY 0)
(define GRADIENT_THERMAL 1)
(define GRADIENT_RAINBOW 2)
(define GRADIENT_COPPER  3)

;; intermediate for generating rainbow palette
(define (color:rainbow x)
    (let ((v (/ (+ x 1.0) 2.0))
        (d (/ 1.0 6.0)))
    (cond ((> v (* 5.0 d)) 1.0)
          ((> v (* 4.0 d)) (* 6.0 (- v (* 4.0 d))))
          ((> v (* 2.0 d)) 0.0)
          ((> v (* 1.0 d)) (* 6.0 (- (* 2.0 d) v)))
          (else 1.0))))

;; color gradient
(define (color-gradient type value)
  (cond 
     ((= type GRADIENT_GRAY)
        (color-rgbf value value value)) 
     ((= type GRADIENT_THERMAL)
        (color-rgbf (* 3. (+ value 0.03)) 
                    (* 3. (- value 0.33333)) 
                    (* 3. (- value 0.66666))))
     ((= type GRADIENT_RAINBOW)
        (let* ((v (- (* 2.0 (- 1.0 value)) 1.0)) 
               (v1 (+ v (/ 2. 3.)))
               (v2 (+ v (/ 4. 3.)))
               (r (color:rainbow v))
               (g (color:rainbow (if (> v2 1.) (- v2 2.) v2)))
               (b (color:rainbow (if (> v1 1.) (- v1 2.) v1)))) 
          (color-rgbf r g b)))
     ((= type GRADIENT_COPPER)
        (color-rgbf (+ (if (< value (/ 4. 5.)) (* value (/ 5. 4.)) 0.) (if (>= value (/ 4. 5.)) 1. 0.))
                    (/ (* value 4.) 5.)
                    (* value 0.5)))
 ))

;; mix two colors
;; w = 0 : c1
;; w = 1 : c2
(define (color-mix c1 c2 w)
  (let ((r1 (/ (color-red c1) 255.))
        (g1 (/ (color-green c1) 255.))
        (b1 (/ (color-blue c1) 255.))
        (r2 (/ (color-red c2) 255.))
        (g2 (/ (color-green c2) 255.))
        (b2 (/ (color-blue c2) 255.))
        (clipw (if (< w 0.0) 0.0 (if (> w 1.0) 1.0 w))))
    (color-rgbf (+ (* clipw r2) (* (- 1. clipw) r1))
                (+ (* clipw g2) (* (- 1. clipw) g1))
                (+ (* clipw b2) (* (- 1. clipw) b1)))))

;; apply a weight to a color (shading)
;; 0 = total shade (i.e. black)
;; 1 = no shade 
(define (color-shade c w) (color-mix Black c w))

;; apply alpha to a color
;; 0 = totally transparent
;; 1 = opaque
(define (color-fade c f) (color-rgba
  (color-red c) (color-green c) (color-blue c) (macro-fix (fl* 255. (macro-flo f)))))


;; oscillating alarm colors - flutterbugs :)

(define fluttercolors (make-table))

(define (make-colorflutter color1 color2 transistiontime)
  (let ((idx (equal?-hash (list color1 color2))))
    (table-set! fluttercolors idx (vector color1 color2 transistiontime))
    idx))

(define (colorflutter idx)
  (define (fmodulo a b) (let ((v (/ a b))) (- v (floor v))))
  (let* ((entry (table-ref fluttercolors idx #f))
         (c1 (vector-ref entry 0))
         (c2 (vector-ref entry 1))
         (T  (vector-ref entry 2))
         (w (abs (- (* 2. (fmodulo (time->seconds (current-time)) (* 2 T))) 1))))
    (color-mix c1 c2 w)
  ))


(define (rgb->hsv rgb)
  (let* ((red (floor (car rgb)))
         (green (floor (cadr rgb)))
         (blue (floor (caddr rgb)))
         (h 0.0)
         (s 0.0)
         (minv (min red (min green blue)))
         (maxv (max red (max green blue)))
         (v maxv)
         (delta 0))
    (if (not (= 0 maxv))
      (set! s (/ (* (- maxv minv) 255.0) maxv))
      (set! s 0.0))
    (if (= s 0.0) (set! h 0.0) (begin
      (set! delta (- maxv minv))
      (cond 
        ((= maxv red)
           (set! h (/ (- green blue) delta)))
        ((= maxv green)
           (set! h (+ 2.0 (/ (- blue red) delta))))
        ((= maxv blue)
           (set! h (+ 4.0 (/ (- red green) delta))))
      )
      (set! h (* 42.5 h))
      (if (< h 0.0) (set! h (+ h 255.0)))
      (if (< 255 h) (set! h (- h 255.0)))
    ))
    (list (floor h) (floor s) (floor v))
 ))

(define (hsv->rgb hsv)
  (let ((h (car hsv))
        (s (cadr hsv))
        (v (caddr hsv))
        (hue 0)
        (saturation 0)
        (value 0))
    (if (= s 0) (begin
      (set! h v)
      (set! s v)) 
      (let ((f 0)(p 0)(q 0)(t 0))
        (set! hue (/ (* 6 h) 255))
        (if (= hue 6.0) (set! hue 0.0))
        (set! saturation (/ s  255.0))
        (set! value (/ v 255.0))
        (set! f (- hue (floor hue)))
        (set! p (* value (- 1.0 saturation)))
        (set! q (* value (- 1.0 (* saturation f))))
        (set! t (* value (- 1.0 (* saturation (- 1.0 f)))))
        (let ((tmp (floor hue)))
          (cond 
            ((= 0 tmp)
              (set! h (* value 255))
              (set! s (* t 255))
              (set! v (* p 255)))
            ((= 1 tmp)
              (set! h (* q 255))
              (set! s (* value 255))
              (set! v (* p 255)))
            ((= 2 tmp)
              (set! h (* p 255))
              (set! s (* value 255))
              (set! v (* t 255)))
            ((= 3 tmp)
              (set! h (* p 255))
              (set! s (* q 255))
              (set! v (* value 255)))
            ((= 4 tmp)
              (set! h (* t 255))
              (set! s (* p 255))
              (set! v (* value 255)))
            ((= 5 tmp)
              (set! h (* value 255))
              (set! s (* p 255))
              (set! v (* q 255)))
        ))
    ))
    (list h s v)
 ))

(define (color-mixhsv c1 c2 w)
  (let* ((rgb1 (list (color-red c1) (color-green c1) (color-blue c1)))
         (rgb2 (list (color-red c2) (color-green c2) (color-blue c2)))
         (clipw (if (< w 0.0) 0.0 (if (> w 1.0) 1.0 w)))
         (hsv1 (rgb->hsv rgb1))
         (hsv2 (rgb->hsv rgb2))
         (hsvmix (list
           (+ (* clipw (car hsv2)) (* (- 1. clipw) (car hsv1)))
           (+ (* clipw (cadr hsv2)) (* (- 1. clipw) (cadr hsv1)))
           (+ (* clipw (caddr hsv2)) (* (- 1. clipw) (caddr hsv1))))))
    (apply color-rgb (map fix (hsv->rgb hsvmix)))))

;; eof 
