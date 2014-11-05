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

;; wrapper for libgd (supporting png, jpeg and freetype)

(c-declare  #<<end-of-c-declare

#include <stdio.h>
#include <gd.h>

void gdImageTrueColorRGBA(gdImagePtr im, unsigned char *data, int w2, int h2)
{
  int i,j;
  int w = gdImageSX(im);
  int h = gdImageSY(im);
  for (i=0;i<w;i++) {
    for (j=0;j<h;j++) {
      unsigned int c=gdImageGetTrueColorPixel(im,i,j);
      data[4*(i+w2*j)]=gdTrueColorGetRed(c);
      data[4*(i+w2*j)+1]=gdTrueColorGetGreen(c);
      data[4*(i+w2*j)+2]=gdTrueColorGetBlue(c);
      data[4*(i+w2*j)+3]=~gdTrueColorGetAlpha(c);
    }
  }
}

end-of-c-declare
)

(define-macro (gd-constant k) 
  `(define ,k ((c-lambda () int 
     ,(string-append "___result = " (symbol->string k) ";")))))

(gd-constant GD_MAJOR_VERSION)
(gd-constant GD_MINOR_VERSION)
(gd-constant GD_RELEASE_VERSION)

(gd-constant gdAlphaMax)
(gd-constant gdAlphaOpaque)
(gd-constant gdAlphaTransparent)
(gd-constant gdRedMax)
(gd-constant gdGreenMax)
(gd-constant gdBlueMax)

(gd-constant gdEffectReplace)
(gd-constant gdEffectAlphaBlend)
(gd-constant gdEffectNormal)
(gd-constant gdEffectOverlay)

(gd-constant GD_TRUE)
(gd-constant GD_FALSE)

(gd-constant gdAntiAliased)
(gd-constant gdBrushed)
(gd-constant gdMaxColors)
(gd-constant gdStyled)
(gd-constant gdStyledBrushed)
(gd-constant gdDashSize)
(gd-constant gdTiled)
(gd-constant gdTransparent)
(gd-constant gdArc)
(gd-constant gdChord)
(gd-constant gdPie)
(gd-constant gdNoFill)
(gd-constant gdEdged)

(gd-constant GD_CMP_IMAGE)
(gd-constant GD_CMP_NUM_COLORS)
(gd-constant GD_CMP_COLOR)
(gd-constant GD_CMP_SIZE_X)
(gd-constant GD_CMP_SIZE_Y)
(gd-constant GD_CMP_TRANSPARENT)
(gd-constant GD_CMP_BACKGROUND)
(gd-constant GD_CMP_INTERLACE)
(gd-constant GD_CMP_TRUECOLOR)
(gd-constant GD_RESOLUTION)

(gd-constant GD_AFFINE_TRANSLATE)
(gd-constant GD_AFFINE_SCALE)
(gd-constant GD_AFFINE_ROTATE)
(gd-constant GD_AFFINE_SHEAR_HORIZONTAL)
(gd-constant GD_AFFINE_SHEAR_VERTICAL)

(gd-constant GD_CROP_DEFAULT)
(gd-constant GD_CROP_TRANSPARENT)
(gd-constant GD_CROP_BLACK)
(gd-constant GD_CROP_WHITE)
(gd-constant GD_CROP_SIDES)
(gd-constant GD_CROP_THRESHOLD)

(gd-constant GD_FLIP_HORINZONTAL) ;; ?!
(gd-constant GD_FLIP_VERTICAL)
(gd-constant GD_FLIP_BOTH)

(gd-constant GD_PIXELATE_UPPERLEFT)
(gd-constant GD_PIXELATE_AVERAGE)

(gd-constant GD_DEFAULT)
(gd-constant GD_BELL)
(gd-constant GD_BESSEL)
(gd-constant GD_BILINEAR_FIXED)
(gd-constant GD_BICUBIC)
(gd-constant GD_BICUBIC_FIXED)
(gd-constant GD_BLACKMAN)
(gd-constant GD_BOX)
(gd-constant GD_BSPLINE)
(gd-constant GD_CATMULLROM)
(gd-constant GD_GAUSSIAN)
(gd-constant GD_GENERALIZED_CUBIC)
(gd-constant GD_HERMITE)
(gd-constant GD_HAMMING)
(gd-constant GD_HANNING)
(gd-constant GD_MITCHELL)
(gd-constant GD_NEAREST_NEIGHBOUR)
(gd-constant GD_POWER)
(gd-constant GD_QUADRATIC)
(gd-constant GD_SINC)
(gd-constant GD_TRIANGLE)
(gd-constant GD_WEIGHTED4)

(gd-constant GD_QUANT_DEFAULT)
(gd-constant GD_QUANT_JQUANT)
(gd-constant GD_QUANT_NEUQUANT)
(gd-constant GD_QUANT_LIQ)

(define gdFileOpen (c-lambda (char-string char-string) (pointer void) "fopen"))
(define gdFileClose (c-lambda ((pointer void)) int "fclose"))

(define-macro (gd-function f args ret) 
  `(define ,f (c-lambda ,args ,ret ,(symbol->string f))))

(gd-function gdAlphaBlend (int int) int)

(gd-function gdImageCreate (int int) (pointer void))
(gd-function gdImageCreateTrueColor (int int) (pointer void))
(gd-function gdImageCreateFromGif ((pointer void)) (pointer void))
(gd-function gdImageCreateFromPng ((pointer void)) (pointer void))
(gd-function gdImageCreateFromJpeg ((pointer void)) (pointer void))
(gd-function gdImageCreateFromGd ((pointer void)) (pointer void))
(gd-function gdImageCreateFromGd2 ((pointer void)) (pointer void))
(gd-function gdImageGif ((pointer void) (pointer void)) void)
(gd-function gdImagePng ((pointer void) (pointer void)) void)
(gd-function gdImageJpeg ((pointer void) (pointer void) int) void)
(gd-function gdImageGd ((pointer void) (pointer void)) void)
(gd-function gdImageGd2 ((pointer void) (pointer void) int int) void)

(gd-function gdImageDestroy ((pointer void)) void)

(gd-function gdImageLine ((pointer void) int int int int int) void)
(gd-function gdImageDashedLine ((pointer void) int int int int int) void)

(gd-function gdImageSetPixel ((pointer void) int int int) void)
(gd-function gdImageGetPixel ((pointer void) int int) int)
(gd-function gdImageGetTrueColorPixel ((pointer void) int int) int)

(gd-function gdImageAABlend ((pointer void)) void)

(gd-function gdImageRectangle ((pointer void) int int int int int) void)
(gd-function gdImageFilledRectangle ((pointer void) int int int int int) void)

(gd-function gdImageSetClip ((pointer void) int int int int) void)

(gd-function gdImageSetResolution ((pointer void) unsigned-int unsigned-int) void)

(gd-function gdImageBoundsSafe ((pointer void) int int) int)

(gd-function gdImageColorAllocate ((pointer void) int int int) int)
(gd-function gdImageColorAllocateAlpha ((pointer void) int int int int) int)
(gd-function gdImageColorClosest ((pointer void) int int int) int)
(gd-function gdImageColorClosestAlpha ((pointer void) int int int int) int)
(gd-function gdImageColorClosestHWB ((pointer void) int int int) int)
(gd-function gdImageColorExact ((pointer void) int int int) int)
(gd-function gdImageColorExactAlpha ((pointer void) int int int int) int)
(gd-function gdImageColorResolve ((pointer void) int int int) int)
(gd-function gdImageColorResolveAlpha ((pointer void) int int int int) int)
(gd-function gdImageColorDeallocate ((pointer void) int) void)

(define gdImageSX (c-lambda ((pointer void)) int "___result=gdImageSX((gdImagePtr)___arg1);"))
(define gdImageSY (c-lambda ((pointer void)) int "___result=gdImageSY((gdImagePtr)___arg1);"))
(define gdImageRed (c-lambda ((pointer void) int) int "___result=gdImageRed((gdImagePtr)___arg1,___arg2);"))
(define gdImageGreen(c-lambda ((pointer void) int) int "___result=gdImageGreen((gdImagePtr)___arg1,___arg2);"))
(define gdImageBlue (c-lambda ((pointer void) int) int "___result=gdImageBlue((gdImagePtr)___arg1,___arg2);"))
(define gdImageAlpha (c-lambda ((pointer void) int) int "___result=gdImageAlpha((gdImagePtr)___arg1,___arg2);"))
(define gdImageGetTransparent (c-lambda ((pointer void)) int "___result=gdImageGetTransparent((gdImagePtr)___arg1);"))
(define gdImageGetInterlaced (c-lambda ((pointer void)) int "___result=gdImageGetInterlaced((gdImagePtr)___arg1);"))
(define gdImageResolutionX (c-lambda ((pointer void)) int "___result=gdImageResolutionX((gdImagePtr)___arg1);"))
(define gdImageResolutionY (c-lambda ((pointer void)) int "___result=gdImageResolutionY((gdImagePtr)___arg1);"))

(gd-function gdImageColorTransparent ((pointer void) int) void)
(gd-function gdImageColorReplace ((pointer void) int int) int)
(gd-function gdImageColorReplaceThreshold ((pointer void) int int float) int)

(gd-function gdFree ((pointer void)) void)

(gd-function gdImageFilledArc ((pointer void) int int int int int int int int) void)
(gd-function gdImageArc ((pointer void) int  int int  int int int int) void)
(gd-function gdImageEllipse ((pointer void) int int int  int  int) void)
(gd-function gdImageFilledEllipse ((pointer void) int  int  int  int  int) void)
(gd-function gdImageFillToBorder ((pointer void) int int int int) void)
(gd-function gdImageFill ((pointer void) int int int) void)

(gd-function gdImageSetBrush ((pointer void) (pointer void)) void)
(gd-function gdImageSetTile ((pointer void) (pointer void)) void)
(gd-function gdImageSetAntiAliased ((pointer void) int) void)
(gd-function gdImageSetAntiAliasedDontBlend ((pointer void) int int) void)
(gd-function gdImageSetThickness ((pointer void) int) void)
(gd-function gdImageInterlace ((pointer void) int) void)
(gd-function gdImageAlphaBlending ((pointer void) int) void)
(gd-function gdImageSaveAlpha ((pointer void) int) void)
(gd-function gdImageNeuQuant ((pointer void) int int) (pointer void))
(gd-function gdImagePixelate ((pointer void) int int) int)

(gd-function gdImageScatter ((pointer void) int int) int)
(gd-function gdImageSmooth ((pointer void) float) int)
(gd-function gdImageMeanRemoval ((pointer void)) int)
(gd-function gdImageEmboss ((pointer void)) int)
(gd-function gdImageGaussianBlur ((pointer void)) int)
(gd-function gdImageEdgeDetectQuick ((pointer void)) int)
(gd-function gdImageSelectiveBlur ((pointer void)) int)
(gd-function gdImageColor ((pointer void) int int int int) int)
(gd-function gdImageContrast ((pointer void) double) int)
(gd-function gdImageBrightness ((pointer void) int) int)
(gd-function gdImageGrayScale ((pointer void)) int)
(gd-function gdImageNegate ((pointer void)) int)

(gd-function gdImageClone ((pointer void)) (pointer void))
(gd-function gdImageCopy ((pointer void) (pointer void) int int int int int int) void)
(gd-function gdImageCopyMerge ((pointer void) (pointer void) int int int int int int int) void)
(gd-function gdImageCopyMergeGray ((pointer void) (pointer void) int int int int int int int) void)
(gd-function gdImageCopyResized ((pointer void) (pointer void) int int int int int int int int) void)
(gd-function gdImageCopyResampled ((pointer void) (pointer void) int int int int int int int int) void)
(gd-function gdImageCopyRotated ((pointer void) (pointer void) int int int int int int int) void)

(gd-function gdFontCacheSetup () int)
(gd-function gdFontCacheShutdown () void)
(gd-function gdFreeFontCache () void)
(gd-function gdImageStringFT ((pointer void) (pointer void) int char-string double double int int char-string) char-string)

;; ------
;; opengl related functions
;; eval is used to delay resolving potentially unavailable calls

(define (gdImageTrueColorRGBA arg1 arg2 arg3 arg4)
  ((c-lambda ((pointer void) scheme-object int int) void
     "gdImageTrueColorRGBA(___arg1,___CAST(void*,___BODY_AS(___arg2,___tSUBTYPED)),___arg3,___arg4);")
       arg1 arg2 arg3 arg4))

(define (gd->img ptr)
  (let* ((w (gdImageSX ptr))
         (h (gdImageSY ptr))
         (w2 (fix (expt 2. (ceiling (/ (log w) (log 2.))))))
         (h2 (fix (expt 2. (ceiling (/ (log h) (log 2.))))))
         (data (make-u8vector (* w2 h2 4)))
         (t (begin (gdImageTrueColorRGBA ptr data w2 h2) ((eval 'glCoreTextureCreate) w2 h2 data))))
   ;; (list w h t 0. (- 1. (/ h h2 1.)) (/ w w2 1.) 1.)
   ;; (list w h t 0. 0. (/ w w2 1.) (/ h h2 1.))
    (list w h t 0. (/ h h2 1.) (/ w w2 1.) 0.)
  ))


;; eof
