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

;; bindings to libharu / libhpdf

(c-declare  #<<end-of-c-declare

#include <stdio.h>
#include <stdlib.h>

#include "hpdf.h"

void hpdf_error_handler(HPDF_STATUS error_no, HPDF_STATUS detail_no, void *user_data){
// printf ("HPDF: Error: error_no=%04X, detail_no=%u\n", (HPDF_UINT)error_no, (HPDF_UINT)detail_no);
}

HPDF_Doc HPDF_NewX(void){
  return HPDF_New(hpdf_error_handler,NULL);
}

HPDF_Doc HPDF_NewExX(void){
  return HPDF_NewEx(hpdf_error_handler,NULL,NULL,0,NULL);
}

static void HPDF_Page_DrawRGB(HPDF_Doc  pdf,  HPDF_Page page, HPDF_BYTE *imagedata, int w, int h, float x0, float y0){
  HPDF_Image image;
  if (!imagedata) return;
  image = HPDF_LoadRawImageFromMem (pdf, imagedata, w, h, HPDF_CS_DEVICE_RGB, 8);
  HPDF_Page_DrawImage (page, image, x0, y0, w, h);
  free(imagedata);
}

end-of-c-declare
)

;; libharu types
(c-define-type HPDF_INT int)
(c-define-type HPDF_UINT unsigned-int)
(c-define-type HPDF_INT32 int)
(c-define-type HPDF_UINT32 unsigned-int)
(c-define-type HPDF_INT16 short)
(c-define-type HPDF_UINT16 unsigned-short)
(c-define-type HPDF_INT8 int8)
(c-define-type HPDF_UINT8 unsigned-int8)
(c-define-type HPDF_BYTE unsigned-char)
(c-define-type HPDF_REAL float)
(c-define-type HPDF_DOUBLE double)
(c-define-type HPDF_BOOL int)
(c-define-type HPDF_STATUS unsigned-long)
(c-define-type HPDF_CID unsigned-short)
(c-define-type HPDF_UNICODE unsigned-short)

(c-define-type HPDF_BYTE* (pointer HPDF_BYTE))
(c-define-type HPDF_REAL* (pointer HPDF_REAL))
(c-define-type HPDF_UINT* (pointer HPDF_UINT))
(c-define-type HPDF_UINT32* (pointer HPDF_UINT32))
(c-define-type HPDF_UINT16* (pointer HPDF_UINT16))

(c-define-type HPDF_HANDLE (pointer void))
(c-define-type HPDF_Doc (pointer void))
(c-define-type HPDF_Page (pointer void))
(c-define-type HPDF_Pages (pointer void))
(c-define-type HPDF_Stream (pointer void))
(c-define-type HPDF_Image (pointer void))
(c-define-type HPDF_Font (pointer void))
(c-define-type HPDF_Outline (pointer void))
(c-define-type HPDF_Encoder (pointer void))
(c-define-type HPDF_Destination (pointer void))
(c-define-type HPDF_XObject (pointer void))
(c-define-type HPDF_Annotation (pointer void))
(c-define-type HPDF_ExtGState (pointer void))

;; libharu enums
(c-define-type HPDF_InfoType int)
(c-define-type HPDF_PDFVer int)
(c-define-type HPDF_EncryptMode int)
(c-define-type HPDF_ColorSpace int)
(c-define-type HPDF_LineCap int)
(c-define-type HPDF_LineJoin int)
(c-define-type HPDF_TextRenderingMode int)
(c-define-type HPDF_WritingMode int)
(c-define-type HPDF_PageLayout int)
(c-define-type HPDF_PageMode int)
(c-define-type HPDF_PageNumStyle int)
(c-define-type HPDF_DestinationType int)
(c-define-type HPDF_AnnotType int)
(c-define-type HPDF_AnnotFlgs int)
(c-define-type HPDF_AnnotHighlightMode int)
(c-define-type HPDF_AnnotIcon int)
(c-define-type HPDF_BSSubtype int)
(c-define-type HPDF_BlendMode int)
(c-define-type HPDF_TransitionStyle int)
(c-define-type HPDF_PageSizes int)
(c-define-type HPDF_PageDirection int)
(c-define-type HPDF_EncoderType int)
(c-define-type HPDF_ByteType int)
(c-define-type HPDF_TextAlignment int)

;; libharu structs
(c-define-type HPDF_Point "HPDF_Point")
(c-define-type HPDF_Rect "HPDF_Rect")
(c-define-type HPDF_Box "HPDF_Box")
(c-define-type HPDF_Date "HPDF_Date")
(c-define-type HPDF_TextWidth "HPDF_TextWidth")
(c-define-type HPDF_DashMode "HPDF_DashMode")
(c-define-type HPDF_TransMatrix "HPDF_TransMatrix")
(c-define-type HPDF_RGBColor "HPDF_RGBColor")
(c-define-type HPDF_CMYKColor "HPDF_CMYKColor")

(c-define-type HPDF_Point* (pointer HPDF_Point))

;; libharu enums 
(define HPDF_COMP_NONE  ((c-lambda () HPDF_UINT "___result = HPDF_COMP_NONE; ")))
(define HPDF_COMP_TEXT  ((c-lambda () HPDF_UINT "___result = HPDF_COMP_TEXT; ")))
(define HPDF_COMP_IMAGE  ((c-lambda () HPDF_UINT "___result = HPDF_COMP_IMAGE; ")))
(define HPDF_COMP_METADATA  ((c-lambda () HPDF_UINT "___result = HPDF_COMP_METADATA; ")))
(define HPDF_COMP_ALL  ((c-lambda () HPDF_UINT "___result = HPDF_COMP_ALL; ")))
(define HPDF_INFO_CREATION_DATE  ((c-lambda () HPDF_InfoType  "___result = HPDF_INFO_CREATION_DATE; ")))
(define HPDF_INFO_MOD_DATE  ((c-lambda ()  HPDF_InfoType "___result = HPDF_INFO_MOD_DATE; ")))
(define HPDF_INFO_AUTHOR  ((c-lambda ()  HPDF_InfoType "___result = HPDF_INFO_AUTHOR; ")))
(define HPDF_INFO_CREATOR  ((c-lambda ()  HPDF_InfoType "___result = HPDF_INFO_CREATOR; ")))
(define HPDF_INFO_PRODUCER  ((c-lambda ()  HPDF_InfoType "___result = HPDF_INFO_PRODUCER; ")))
(define HPDF_INFO_TITLE  ((c-lambda ()  HPDF_InfoType "___result = HPDF_INFO_TITLE; ")))
(define HPDF_INFO_SUBJECT  ((c-lambda ()  HPDF_InfoType "___result = HPDF_INFO_SUBJECT; ")))
(define HPDF_INFO_KEYWORDS  ((c-lambda ()  HPDF_InfoType "___result = HPDF_INFO_KEYWORDS; ")))
(define HPDF_INFO_EOF  ((c-lambda ()  HPDF_InfoType "___result = HPDF_INFO_EOF; ")))
(define HPDF_VER_12  ((c-lambda () HPDF_PDFVer "___result = HPDF_VER_12; ")))
(define HPDF_VER_13  ((c-lambda () HPDF_PDFVer "___result = HPDF_VER_13; ")))
(define HPDF_VER_14  ((c-lambda () HPDF_PDFVer "___result = HPDF_VER_14; ")))
(define HPDF_VER_15  ((c-lambda () HPDF_PDFVer "___result = HPDF_VER_15; ")))
(define HPDF_VER_16  ((c-lambda () HPDF_PDFVer "___result = HPDF_VER_16; ")))
(define HPDF_VER_EOF  ((c-lambda () HPDF_PDFVer "___result = HPDF_VER_EOF; ")))
(define HPDF_ENCRYPT_R2  ((c-lambda () HPDF_EncryptMode "___result = HPDF_ENCRYPT_R2; ")))
(define HPDF_ENCRYPT_R3  ((c-lambda () HPDF_EncryptMode "___result = HPDF_ENCRYPT_R3; ")))
(define HPDF_CS_DEVICE_GRAY  ((c-lambda () HPDF_ColorSpace "___result = HPDF_CS_DEVICE_GRAY; ")))
(define HPDF_CS_DEVICE_RGB  ((c-lambda () HPDF_ColorSpace "___result = HPDF_CS_DEVICE_RGB; ")))
(define HPDF_CS_DEVICE_CMYK  ((c-lambda () HPDF_ColorSpace "___result = HPDF_CS_DEVICE_CMYK; ")))
(define HPDF_CS_CAL_GRAY  ((c-lambda () HPDF_ColorSpace "___result = HPDF_CS_CAL_GRAY; ")))

(define HPDF_CS_LAB  ((c-lambda () HPDF_ColorSpace "___result = HPDF_CS_LAB; ")))
(define HPDF_CS_ICC_BASED  ((c-lambda () HPDF_ColorSpace "___result = HPDF_CS_ICC_BASED; ")))
(define HPDF_CS_SEPARATION  ((c-lambda () HPDF_ColorSpace "___result = HPDF_CS_SEPARATION; ")))
(define HPDF_CS_DEVICE_N  ((c-lambda () HPDF_ColorSpace "___result = HPDF_CS_DEVICE_N; ")))
(define HPDF_CS_INDEXED  ((c-lambda () HPDF_ColorSpace "___result = HPDF_CS_INDEXED; ")))
(define HPDF_CS_PATTERN  ((c-lambda () HPDF_ColorSpace "___result = HPDF_CS_PATTERN; ")))
(define HPDF_CS_EOF  ((c-lambda () HPDF_ColorSpace "___result = HPDF_CS_EOF; ")))
(define HPDF_BUTT_END  ((c-lambda () HPDF_LineCap "___result = HPDF_BUTT_END; ")))
(define HPDF_ROUND_END  ((c-lambda () HPDF_LineCap "___result = HPDF_ROUND_END; ")))
(define HPDF_PROJECTING_SCUARE_END  ((c-lambda () HPDF_LineCap "___result = HPDF_PROJECTING_SCUARE_END; ")))
(define HPDF_LINECAP_EOF  ((c-lambda () HPDF_LineCap "___result = HPDF_LINECAP_EOF; ")))
(define HPDF_MITER_JOIN  ((c-lambda () HPDF_LineJoin "___result = HPDF_MITER_JOIN; ")))
(define HPDF_ROUND_JOIN  ((c-lambda () HPDF_LineJoin "___result = HPDF_ROUND_JOIN; ")))
(define HPDF_BEVEL_JOIN  ((c-lambda () HPDF_LineJoin "___result = HPDF_BEVEL_JOIN; ")))
(define HPDF_LINEJOIN_EOF  ((c-lambda () HPDF_LineJoin "___result = HPDF_LINEJOIN_EOF; ")))
(define HPDF_FILL  ((c-lambda () HPDF_TextRenderingMode "___result = HPDF_FILL; ")))
(define HPDF_STROKE  ((c-lambda () HPDF_TextRenderingMode "___result = HPDF_STROKE; ")))
(define HPDF_FILL_THEN_STROKE  ((c-lambda () HPDF_TextRenderingMode "___result = HPDF_FILL_THEN_STROKE; ")))
(define HPDF_INVISIBLE  ((c-lambda () HPDF_TextRenderingMode "___result = HPDF_INVISIBLE; ")))
(define HPDF_FILL_CLIPPING  ((c-lambda () HPDF_TextRenderingMode "___result = HPDF_FILL_CLIPPING; ")))
(define HPDF_STROKE_CLIPPING  ((c-lambda () HPDF_TextRenderingMode "___result = HPDF_STROKE_CLIPPING; ")))
(define HPDF_FILL_STROKE_CLIPPING  ((c-lambda () HPDF_TextRenderingMode "___result = HPDF_FILL_STROKE_CLIPPING; ")))
(define HPDF_CLIPPING  ((c-lambda () HPDF_TextRenderingMode "___result = HPDF_CLIPPING; ")))
(define HPDF_RENDERING_MODE_EOF  ((c-lambda () HPDF_TextRenderingMode "___result = HPDF_RENDERING_MODE_EOF; ")))
(define HPDF_WMODE_HORIZONTAL  ((c-lambda () HPDF_WritingMode "___result = HPDF_WMODE_HORIZONTAL; ")))
(define HPDF_WMODE_VERTICAL  ((c-lambda () HPDF_WritingMode "___result = HPDF_WMODE_VERTICAL; ")))
(define HPDF_WMODE_EOF  ((c-lambda () HPDF_WritingMode "___result = HPDF_WMODE_EOF; ")))
(define HPDF_PAGE_LAYOUT_SINGLE  ((c-lambda () HPDF_PageLayout "___result = HPDF_PAGE_LAYOUT_SINGLE; ")))
(define HPDF_PAGE_LAYOUT_ONE_COLUMN  ((c-lambda () HPDF_PageLayout "___result = HPDF_PAGE_LAYOUT_ONE_COLUMN; ")))
(define HPDF_PAGE_LAYOUT_TWO_COLUMN_LEFT  ((c-lambda () HPDF_PageLayout "___result = HPDF_PAGE_LAYOUT_TWO_COLUMN_LEFT; ")))
(define HPDF_PAGE_LAYOUT_TWO_COLUMN_RIGHT  ((c-lambda () HPDF_PageLayout "___result = HPDF_PAGE_LAYOUT_TWO_COLUMN_RIGHT; ")))
(define HPDF_PAGE_LAYOUT_EOF  ((c-lambda () HPDF_PageLayout "___result = HPDF_PAGE_LAYOUT_EOF; ")))
(define HPDF_PAGE_MODE_USE_NONE  ((c-lambda () HPDF_PageMode "___result = HPDF_PAGE_MODE_USE_NONE; ")))
(define HPDF_PAGE_MODE_USE_OUTLINE  ((c-lambda () HPDF_PageMode "___result = HPDF_PAGE_MODE_USE_OUTLINE; ")))
(define HPDF_PAGE_MODE_USE_THUMBS  ((c-lambda () HPDF_PageMode "___result = HPDF_PAGE_MODE_USE_THUMBS; ")))
(define HPDF_PAGE_MODE_FULL_SCREEN  ((c-lambda () HPDF_PageMode "___result = HPDF_PAGE_MODE_FULL_SCREEN; ")))
(define HPDF_PAGE_MODE_EOF  ((c-lambda () HPDF_PageMode "___result = HPDF_PAGE_MODE_EOF; ")))
(define HPDF_PAGE_NUM_STYLE_DECIMAL  ((c-lambda () HPDF_PageNumStyle "___result = HPDF_PAGE_NUM_STYLE_DECIMAL; ")))
(define HPDF_PAGE_NUM_STYLE_UPPER_ROMAN  ((c-lambda () HPDF_PageNumStyle "___result = HPDF_PAGE_NUM_STYLE_UPPER_ROMAN; ")))
(define HPDF_PAGE_NUM_STYLE_LOWER_ROMAN  ((c-lambda () HPDF_PageNumStyle "___result = HPDF_PAGE_NUM_STYLE_LOWER_ROMAN; ")))
(define HPDF_PAGE_NUM_STYLE_UPPER_LETTERS  ((c-lambda () HPDF_PageNumStyle "___result = HPDF_PAGE_NUM_STYLE_UPPER_LETTERS; ")))
(define HPDF_PAGE_NUM_STYLE_LOWER_LETTERS  ((c-lambda () HPDF_PageNumStyle "___result = HPDF_PAGE_NUM_STYLE_LOWER_LETTERS; ")))
(define HPDF_PAGE_NUM_STYLE_EOF  ((c-lambda () HPDF_PageNumStyle "___result = HPDF_PAGE_NUM_STYLE_EOF; ")))
(define HPDF_XYZ  ((c-lambda () HPDF_DestinationType "___result = HPDF_XYZ; ")))
(define HPDF_FIT  ((c-lambda () HPDF_DestinationType "___result = HPDF_FIT; ")))
(define HPDF_FIT_H  ((c-lambda () HPDF_DestinationType "___result = HPDF_FIT_H; ")))
(define HPDF_FIT_V  ((c-lambda () HPDF_DestinationType "___result = HPDF_FIT_V; ")))
(define HPDF_FIT_R  ((c-lambda () HPDF_DestinationType "___result = HPDF_FIT_R; ")))
(define HPDF_FIT_B  ((c-lambda () HPDF_DestinationType "___result = HPDF_FIT_B; ")))
(define HPDF_FIT_BH  ((c-lambda () HPDF_DestinationType "___result = HPDF_FIT_BH; ")))
(define HPDF_FIT_BV  ((c-lambda () HPDF_DestinationType "___result = HPDF_FIT_BV; ")))
(define HPDF_DST_EOF  ((c-lambda () HPDF_DestinationType "___result = HPDF_DST_EOF; ")))
(define HPDF_ANNOT_TEXT_NOTES  ((c-lambda () HPDF_AnnotType "___result = HPDF_ANNOT_TEXT_NOTES; ")))
(define HPDF_ANNOT_LINK  ((c-lambda () HPDF_AnnotType "___result = HPDF_ANNOT_LINK; ")))
(define HPDF_ANNOT_SOUND  ((c-lambda () HPDF_AnnotType "___result = HPDF_ANNOT_SOUND; ")))
(define HPDF_ANNOT_FREE_TEXT  ((c-lambda () HPDF_AnnotType "___result = HPDF_ANNOT_FREE_TEXT; ")))
(define HPDF_ANNOT_STAMP  ((c-lambda () HPDF_AnnotType "___result = HPDF_ANNOT_STAMP; ")))
(define HPDF_ANNOT_SQUARE  ((c-lambda () HPDF_AnnotType "___result = HPDF_ANNOT_SQUARE; ")))
(define HPDF_ANNOT_CIRCLE  ((c-lambda () HPDF_AnnotType "___result = HPDF_ANNOT_CIRCLE; ")))
(define HPDF_ANNOT_STRIKE_OUT  ((c-lambda () HPDF_AnnotType "___result = HPDF_ANNOT_STRIKE_OUT; ")))
(define HPDF_ANNOT_HIGHTLIGHT  ((c-lambda () HPDF_AnnotType "___result = HPDF_ANNOT_HIGHTLIGHT; ")))
(define HPDF_ANNOT_UNDERLINE  ((c-lambda () HPDF_AnnotType "___result = HPDF_ANNOT_UNDERLINE; ")))
(define HPDF_ANNOT_INK  ((c-lambda () HPDF_AnnotType "___result = HPDF_ANNOT_INK; ")))
(define HPDF_ANNOT_FILE_ATTACHMENT  ((c-lambda () HPDF_AnnotType "___result = HPDF_ANNOT_FILE_ATTACHMENT; ")))
(define HPDF_ANNOT_POPUP  ((c-lambda () HPDF_AnnotType "___result = HPDF_ANNOT_POPUP; ")))
(define HPDF_ANNOT_INVISIBLE  ((c-lambda () HPDF_AnnotFlgs "___result = HPDF_ANNOT_INVISIBLE; ")))
(define HPDF_ANNOT_HIDDEN  ((c-lambda () HPDF_AnnotFlgs "___result = HPDF_ANNOT_HIDDEN; ")))
(define HPDF_ANNOT_PRINT  ((c-lambda () HPDF_AnnotFlgs "___result = HPDF_ANNOT_PRINT; ")))
(define HPDF_ANNOT_NOZOOM  ((c-lambda () HPDF_AnnotFlgs "___result = HPDF_ANNOT_NOZOOM; ")))
(define HPDF_ANNOT_NOROTATE  ((c-lambda () HPDF_AnnotFlgs "___result = HPDF_ANNOT_NOROTATE; ")))
(define HPDF_ANNOT_NOVIEW  ((c-lambda () HPDF_AnnotFlgs "___result = HPDF_ANNOT_NOVIEW; ")))
(define HPDF_ANNOT_READONLY  ((c-lambda () HPDF_AnnotFlgs "___result = HPDF_ANNOT_READONLY; ")))
(define HPDF_ANNOT_NO_HIGHTLIGHT  ((c-lambda () HPDF_AnnotHighlightMode "___result = HPDF_ANNOT_NO_HIGHTLIGHT; ")))
(define HPDF_ANNOT_INVERT_BOX  ((c-lambda () HPDF_AnnotHighlightMode "___result = HPDF_ANNOT_INVERT_BOX; ")))
(define HPDF_ANNOT_INVERT_BORDER  ((c-lambda () HPDF_AnnotHighlightMode "___result = HPDF_ANNOT_INVERT_BORDER; ")))
(define HPDF_ANNOT_DOWN_APPEARANCE  ((c-lambda () HPDF_AnnotHighlightMode "___result = HPDF_ANNOT_DOWN_APPEARANCE; ")))
(define HPDF_ANNOT_HIGHTLIGHT_MODE_EOF  ((c-lambda () HPDF_AnnotHighlightMode "___result = HPDF_ANNOT_HIGHTLIGHT_MODE_EOF; ")))
(define HPDF_ANNOT_ICON_COMMENT  ((c-lambda () HPDF_AnnotIcon "___result = HPDF_ANNOT_ICON_COMMENT; ")))
(define HPDF_ANNOT_ICON_KEY  ((c-lambda () HPDF_AnnotIcon "___result = HPDF_ANNOT_ICON_KEY; ")))
(define HPDF_ANNOT_ICON_NOTE  ((c-lambda () HPDF_AnnotIcon "___result = HPDF_ANNOT_ICON_NOTE; ")))
(define HPDF_ANNOT_ICON_HELP  ((c-lambda () HPDF_AnnotIcon "___result = HPDF_ANNOT_ICON_HELP; ")))
(define HPDF_ANNOT_ICON_NEW_PARAGRAPH  ((c-lambda () HPDF_AnnotIcon "___result = HPDF_ANNOT_ICON_NEW_PARAGRAPH; ")))
(define HPDF_ANNOT_ICON_PARAGRAPH  ((c-lambda () HPDF_AnnotIcon "___result = HPDF_ANNOT_ICON_PARAGRAPH; ")))
(define HPDF_ANNOT_ICON_INSERT  ((c-lambda () HPDF_AnnotIcon "___result = HPDF_ANNOT_ICON_INSERT; ")))
(define HPDF_ANNOT_ICON_EOF  ((c-lambda () HPDF_AnnotIcon "___result = HPDF_ANNOT_ICON_EOF; ")))
(define HPDF_BS_SOLID  ((c-lambda () HPDF_BSSubtype "___result = HPDF_BS_SOLID; ")))
(define HPDF_BS_DASHED  ((c-lambda () HPDF_BSSubtype "___result = HPDF_BS_DASHED; ")))
(define HPDF_BS_BEVELED  ((c-lambda () HPDF_BSSubtype "___result = HPDF_BS_BEVELED; ")))
(define HPDF_BS_INSET  ((c-lambda () HPDF_BSSubtype "___result = HPDF_BS_INSET; ")))
(define HPDF_BS_UNDERLINED  ((c-lambda () HPDF_BSSubtype "___result = HPDF_BS_UNDERLINED; ")))
(define HPDF_BM_NORMAL  ((c-lambda () HPDF_BlendMode "___result = HPDF_BM_NORMAL; ")))
(define HPDF_BM_MULTIPLY  ((c-lambda () HPDF_BlendMode "___result = HPDF_BM_MULTIPLY; ")))
(define HPDF_BM_SCREEN  ((c-lambda () HPDF_BlendMode "___result = HPDF_BM_SCREEN; ")))
(define HPDF_BM_OVERLAY  ((c-lambda () HPDF_BlendMode "___result = HPDF_BM_OVERLAY; ")))
(define HPDF_BM_DARKEN  ((c-lambda () HPDF_BlendMode "___result = HPDF_BM_DARKEN; ")))
(define HPDF_BM_LIGHTEN  ((c-lambda () HPDF_BlendMode "___result = HPDF_BM_LIGHTEN; ")))
(define HPDF_BM_COLOR_DODGE  ((c-lambda () HPDF_BlendMode "___result = HPDF_BM_COLOR_DODGE; ")))
(define HPDF_BM_COLOR_BUM  ((c-lambda () HPDF_BlendMode "___result = HPDF_BM_COLOR_BUM; ")))
(define HPDF_BM_HARD_LIGHT  ((c-lambda () HPDF_BlendMode "___result = HPDF_BM_HARD_LIGHT; ")))
(define HPDF_BM_SOFT_LIGHT  ((c-lambda () HPDF_BlendMode "___result = HPDF_BM_SOFT_LIGHT; ")))
(define HPDF_BM_DIFFERENCE  ((c-lambda () HPDF_BlendMode "___result = HPDF_BM_DIFFERENCE; ")))
(define HPDF_BM_EXCLUSHON  ((c-lambda () HPDF_BlendMode "___result = HPDF_BM_EXCLUSHON; ")))
(define HPDF_BM_EOF  ((c-lambda () HPDF_BlendMode "___result = HPDF_BM_EOF; ")))
(define HPDF_TS_WIPE_RIGHT  ((c-lambda () HPDF_TransitionStyle "___result = HPDF_TS_WIPE_RIGHT; ")))
(define HPDF_TS_WIPE_UP  ((c-lambda () HPDF_TransitionStyle "___result = HPDF_TS_WIPE_UP; ")))
(define HPDF_TS_WIPE_LEFT  ((c-lambda () HPDF_TransitionStyle "___result = HPDF_TS_WIPE_LEFT; ")))
(define HPDF_TS_WIPE_DOWN  ((c-lambda () HPDF_TransitionStyle "___result = HPDF_TS_WIPE_DOWN; ")))
(define HPDF_TS_BARN_DOORS_HORIZONTAL_OUT  ((c-lambda () HPDF_TransitionStyle "___result = HPDF_TS_BARN_DOORS_HORIZONTAL_OUT; ")))
(define HPDF_TS_BARN_DOORS_HORIZONTAL_IN  ((c-lambda () HPDF_TransitionStyle "___result = HPDF_TS_BARN_DOORS_HORIZONTAL_IN; ")))
(define HPDF_TS_BARN_DOORS_VERTICAL_OUT  ((c-lambda () HPDF_TransitionStyle "___result = HPDF_TS_BARN_DOORS_VERTICAL_OUT; ")))
(define HPDF_TS_BARN_DOORS_VERTICAL_IN  ((c-lambda () HPDF_TransitionStyle "___result = HPDF_TS_BARN_DOORS_VERTICAL_IN; ")))
(define HPDF_TS_BOX_OUT  ((c-lambda () HPDF_TransitionStyle "___result = HPDF_TS_BOX_OUT; ")))
(define HPDF_TS_BOX_IN  ((c-lambda () HPDF_TransitionStyle "___result = HPDF_TS_BOX_IN; ")))
(define HPDF_TS_BLINDS_HORIZONTAL  ((c-lambda () HPDF_TransitionStyle "___result = HPDF_TS_BLINDS_HORIZONTAL; ")))
(define HPDF_TS_BLINDS_VERTICAL  ((c-lambda () HPDF_TransitionStyle "___result = HPDF_TS_BLINDS_VERTICAL; ")))
(define HPDF_TS_DISSOLVE  ((c-lambda () HPDF_TransitionStyle "___result = HPDF_TS_DISSOLVE; ")))
(define HPDF_TS_GLITTER_RIGHT  ((c-lambda () HPDF_TransitionStyle "___result = HPDF_TS_GLITTER_RIGHT; ")))
(define HPDF_TS_GLITTER_DOWN  ((c-lambda () HPDF_TransitionStyle "___result = HPDF_TS_GLITTER_DOWN; ")))
(define HPDF_TS_GLITTER_TOP_LEFT_TO_BOTTOM_RIGHT  ((c-lambda () HPDF_TransitionStyle "___result = HPDF_TS_GLITTER_TOP_LEFT_TO_BOTTOM_RIGHT; ")))
(define HPDF_TS_REPLACE  ((c-lambda () HPDF_TransitionStyle "___result = HPDF_TS_REPLACE; ")))
(define HPDF_TS_EOF  ((c-lambda () HPDF_TransitionStyle "___result = HPDF_TS_EOF; ")))
(define HPDF_PAGE_SIZE_LETTER  ((c-lambda () HPDF_PageSizes "___result = HPDF_PAGE_SIZE_LETTER; ")))
(define HPDF_PAGE_SIZE_LEGAL  ((c-lambda () HPDF_PageSizes "___result = HPDF_PAGE_SIZE_LEGAL; ")))
(define HPDF_PAGE_SIZE_A3  ((c-lambda () HPDF_PageSizes "___result = HPDF_PAGE_SIZE_A3; ")))
(define HPDF_PAGE_SIZE_A4  ((c-lambda () HPDF_PageSizes "___result = HPDF_PAGE_SIZE_A4; ")))
(define HPDF_PAGE_SIZE_A5  ((c-lambda () HPDF_PageSizes "___result = HPDF_PAGE_SIZE_A5; ")))
(define HPDF_PAGE_SIZE_B4  ((c-lambda () HPDF_PageSizes "___result = HPDF_PAGE_SIZE_B4; ")))
(define HPDF_PAGE_SIZE_B5  ((c-lambda () HPDF_PageSizes "___result = HPDF_PAGE_SIZE_B5; ")))
(define HPDF_PAGE_SIZE_EXECUTIVE  ((c-lambda () HPDF_PageSizes "___result = HPDF_PAGE_SIZE_EXECUTIVE; ")))
(define HPDF_PAGE_SIZE_US4x6  ((c-lambda () HPDF_PageSizes "___result = HPDF_PAGE_SIZE_US4x6; ")))
(define HPDF_PAGE_SIZE_US4x8  ((c-lambda () HPDF_PageSizes "___result = HPDF_PAGE_SIZE_US4x8; ")))
(define HPDF_PAGE_SIZE_US5x7  ((c-lambda () HPDF_PageSizes "___result = HPDF_PAGE_SIZE_US5x7; ")))
(define HPDF_PAGE_SIZE_COMM10  ((c-lambda () HPDF_PageSizes "___result = HPDF_PAGE_SIZE_COMM10; ")))
(define HPDF_PAGE_SIZE_EOF  ((c-lambda () HPDF_PageSizes "___result = HPDF_PAGE_SIZE_EOF; ")))
(define HPDF_PAGE_PORTRAIT  ((c-lambda () HPDF_PageDirection "___result = HPDF_PAGE_PORTRAIT; ")))
(define HPDF_PAGE_LANDSCAPE  ((c-lambda () HPDF_PageDirection "___result = HPDF_PAGE_LANDSCAPE; ")))
(define HPDF_ENCODER_TYPE_SINGLE_BYTE  ((c-lambda () HPDF_EncoderType "___result = HPDF_ENCODER_TYPE_SINGLE_BYTE; ")))
(define HPDF_ENCODER_TYPE_DOUBLE_BYTE  ((c-lambda () HPDF_EncoderType "___result = HPDF_ENCODER_TYPE_DOUBLE_BYTE; ")))
(define HPDF_ENCODER_TYPE_UNINITIALIZED  ((c-lambda () HPDF_EncoderType "___result = HPDF_ENCODER_TYPE_UNINITIALIZED; ")))
(define HPDF_ENCODER_UNKNOWN  ((c-lambda () HPDF_EncoderType "___result = HPDF_ENCODER_UNKNOWN; ")))
(define HPDF_BYTE_TYPE_SINGLE  ((c-lambda () HPDF_ByteType "___result = HPDF_BYTE_TYPE_SINGLE; ")))
(define HPDF_BYTE_TYPE_LEAD  ((c-lambda () HPDF_ByteType "___result = HPDF_BYTE_TYPE_LEAD; ")))
(define HPDF_BYTE_TYPE_TRIAL  ((c-lambda () HPDF_ByteType "___result = HPDF_BYTE_TYPE_TRIAL; ")))
(define HPDF_BYTE_TYPE_UNKNOWN  ((c-lambda () HPDF_ByteType "___result = HPDF_BYTE_TYPE_UNKNOWN; ")))
(define HPDF_TALIGN_LEFT  ((c-lambda () HPDF_TextAlignment "___result = HPDF_TALIGN_LEFT; ")))
(define HPDF_TALIGN_RIGHT  ((c-lambda () HPDF_TextAlignment "___result = HPDF_TALIGN_RIGHT; ")))
(define HPDF_TALIGN_CENTER  ((c-lambda () HPDF_TextAlignment "___result = HPDF_TALIGN_CENTER; ")))
(define HPDF_TALIGN_JUSTIFY  ((c-lambda () HPDF_TextAlignment "___result = HPDF_TALIGN_JUSTIFY; ")))

(define HPDF_GetVersion (c-lambda  () char-string "HPDF_GetVersion"))
(define HPDF_New (c-lambda () HPDF_Doc "HPDF_NewX"))
(define HPDF_NewEx (c-lambda () HPDF_Doc "HPDF_NewExX"))
;; HPDF_SetErrorHandler

(define HPDF_Free (c-lambda  (HPDF_Doc) void "HPDF_Free"))
(define HPDF_NewDoc (c-lambda  (HPDF_Doc) HPDF_STATUS "HPDF_NewDoc"))
(define HPDF_FreeDoc (c-lambda  (HPDF_Doc) void "HPDF_FreeDoc"))
(define HPDF_HasDoc (c-lambda  (HPDF_Doc) HPDF_BOOL "HPDF_HasDoc"))
(define HPDF_FreeDocAll (c-lambda  (HPDF_Doc) void "HPDF_FreeDocAll"))
(define HPDF_SaveToStream (c-lambda  (HPDF_Doc) HPDF_STATUS "HPDF_SaveToStream"))
(define HPDF_GetStreamSize (c-lambda  (HPDF_Doc) HPDF_UINT32 "HPDF_GetStreamSize"))
(define HPDF_ReadFromStream (c-lambda  (HPDF_Doc HPDF_BYTE* HPDF_UINT32*) HPDF_STATUS "HPDF_ReadFromStream"))
(define HPDF_ResetStream (c-lambda  (HPDF_Doc) HPDF_STATUS "HPDF_ResetStream"))
(define HPDF_SaveToFile (c-lambda (HPDF_Doc char-string) HPDF_STATUS "HPDF_SaveToFile"))
(define HPDF_GetError (c-lambda  (HPDF_Doc) HPDF_STATUS "HPDF_GetError"))
(define HPDF_GetErrorDetail (c-lambda  (HPDF_Doc) HPDF_STATUS "HPDF_GetErrorDetail"))
(define HPDF_ResetError (c-lambda  (HPDF_Doc) void "HPDF_ResetError"))
(define HPDF_SetPagesConfiguration (c-lambda (HPDF_Doc HPDF_UINT) HPDF_STATUS "HPDF_SetPagesConfiguration"))
(define HPDF_GetPageByIndex (c-lambda (HPDF_Doc HPDF_UINT) HPDF_Page "HPDF_GetPageByIndex"))

(define HPDF_GetPageLayout (c-lambda (HPDF_Doc) HPDF_PageLayout "HPDF_GetPageLayout"))
(define HPDF_SetPageLayout (c-lambda (HPDF_Doc HPDF_PageLayout) HPDF_STATUS "HPDF_SetPageLayout"))
(define HPDF_GetPageMode (c-lambda (HPDF_Doc) HPDF_PageMode "HPDF_GetPageMode"))
(define HPDF_SetPageMode (c-lambda (HPDF_Doc HPDF_PageMode) HPDF_STATUS "HPDF_SetPageMode"))
(define HPDF_GetViewerPreference (c-lambda (HPDF_Doc) HPDF_UINT "HPDF_GetViewerPreference"))
(define HPDF_SetViewerPreference (c-lambda (HPDF_Doc HPDF_UINT) HPDF_STATUS  "HPDF_SetViewerPreference"))
(define HPDF_SetOpenAction (c-lambda (HPDF_Doc HPDF_Destination) HPDF_STATUS "HPDF_SetOpenAction"))

(define HPDF_GetCurrentPage (c-lambda (HPDF_Doc) HPDF_Page "HPDF_GetCurrentPage"))
(define HPDF_AddPage (c-lambda (HPDF_Doc) HPDF_Page "HPDF_AddPage"))
(define HPDF_InsertPage (c-lambda (HPDF_Doc HPDF_Page) HPDF_Page "HPDF_InsertPage"))
(define HPDF_Page_SetWidth (c-lambda (HPDF_Page HPDF_REAL) HPDF_STATUS "HPDF_Page_SetWidth"))
(define HPDF_Page_SetHeight (c-lambda (HPDF_Page HPDF_REAL) HPDF_STATUS "HPDF_Page_SetHeight"))
(define HPDF_Page_SetSize (c-lambda (HPDF_Page HPDF_PageSizes HPDF_PageDirection) HPDF_STATUS "HPDF_Page_SetSize"))
(define HPDF_Page_SetRotate (c-lambda (HPDF_Page HPDF_UINT16) HPDF_STATUS "HPDF_Page_SetRotate"))

(define HPDF_GetFont (c-lambda (HPDF_Doc char-string char-string) HPDF_Font "HPDF_GetFont"))

(define HPDF_LoadType1FontFromFile (c-lambda (HPDF_Doc char-string char-string) char-string "HPDF_LoadType1FontFromFile"))
(define HPDF_LoadTTFontFromFile (c-lambda (HPDF_Doc char-string HPDF_BOOL) char-string "HPDF_LoadTTFontFromFile"))
(define HPDF_LoadTTFontFromFile2 (c-lambda (HPDF_Doc char-string HPDF_UINT HPDF_BOOL) char-string "HPDF_LoadTTFontFromFile2"))
(define HPDF_AddPageLabel (c-lambda (HPDF_Doc HPDF_UINT HPDF_PageNumStyle HPDF_UINT char-string) HPDF_STATUS "HPDF_AddPageLabel"))
(define HPDF_UseJPFonts (c-lambda (HPDF_Doc) HPDF_STATUS "HPDF_UseJPFonts"))
(define HPDF_UseKRFonts (c-lambda (HPDF_Doc) HPDF_STATUS "HPDF_UseKRFonts"))
(define HPDF_UseCNSFonts (c-lambda (HPDF_Doc) HPDF_STATUS "HPDF_UseCNSFonts"))
(define HPDF_UseCNTFonts (c-lambda (HPDF_Doc) HPDF_STATUS "HPDF_UseCNTFonts"))

(define HPDF_CreateOutline (c-lambda (HPDF_Doc HPDF_Outline char-string HPDF_Encoder) HPDF_Outline "HPDF_CreateOutline"))
(define HPDF_Outline_SetOpened (c-lambda (HPDF_Outline HPDF_BOOL) HPDF_STATUS "HPDF_Outline_SetOpened"))
(define HPDF_Outline_SetDestination (c-lambda (HPDF_Outline HPDF_Destination) HPDF_STATUS "HPDF_Outline_SetDestination"))


(define HPDF_Page_CreateDestination (c-lambda (HPDF_Page) HPDF_Destination "HPDF_Page_CreateDestination"))
(define HPDF_Destination_SetXYZ (c-lambda (HPDF_Destination HPDF_REAL HPDF_REAL HPDF_REAL) HPDF_STATUS "HPDF_Destination_SetXYZ"))
(define HPDF_Destination_SetFit (c-lambda (HPDF_Destination) HPDF_STATUS "HPDF_Destination_SetFit"))
(define HPDF_Destination_SetFitH (c-lambda (HPDF_Destination HPDF_REAL) HPDF_STATUS "HPDF_Destination_SetFitH"))
(define HPDF_Destination_SetFitV (c-lambda (HPDF_Destination HPDF_REAL) HPDF_STATUS "HPDF_Destination_SetFitV"))
(define HPDF_Destination_SetFitR (c-lambda (HPDF_Destination HPDF_REAL HPDF_REAL HPDF_REAL HPDF_REAL) HPDF_STATUS "HPDF_Destination_SetFitR"))
(define HPDF_Destination_SetFitB (c-lambda (HPDF_Destination) HPDF_STATUS "HPDF_Destination_SetFitB"))
(define HPDF_Destination_SetFitBH (c-lambda (HPDF_Destination HPDF_REAL) HPDF_STATUS "HPDF_Destination_SetFitBH"))
(define HPDF_Destination_SetFitBV (c-lambda (HPDF_Destination HPDF_REAL) HPDF_STATUS "HPDF_Destination_SetFitBV"))

(define HPDF_GetEncoder (c-lambda (HPDF_Doc char-string) HPDF_Encoder "HPDF_GetEncoder"))
(define HPDF_GetCurrentEncoder (c-lambda (HPDF_Doc) HPDF_Encoder "HPDF_GetCurrentEncoder"))
(define HPDF_SetCurrentEncoder (c-lambda (HPDF_Doc char-string) HPDF_STATUS "HPDF_SetCurrentEncoder"))
(define HPDF_Encoder_GetType (c-lambda (HPDF_Encoder) HPDF_EncoderType "HPDF_Encoder_GetType"))
(define HPDF_Encoder_GetByteType (c-lambda (HPDF_Encoder char-string HPDF_UINT) HPDF_ByteType "HPDF_Encoder_GetByteType"))
(define HPDF_Encoder_GetUnicode (c-lambda (HPDF_Encoder HPDF_UINT16) HPDF_UNICODE "HPDF_Encoder_GetUnicode"))
(define HPDF_Encoder_GetWritingMode (c-lambda (HPDF_Encoder) HPDF_WritingMode "HPDF_Encoder_GetWritingMode"))
(define HPDF_UseJPEncodings (c-lambda (HPDF_Doc) HPDF_STATUS "HPDF_UseJPEncodings"))
(define HPDF_UseKREncodings (c-lambda (HPDF_Doc) HPDF_STATUS "HPDF_UseKREncodings"))
(define HPDF_UseCNSEncodings (c-lambda (HPDF_Doc) HPDF_STATUS "HPDF_UseCNSEncodings"))
(define HPDF_UseCNTEncodings (c-lambda (HPDF_Doc) HPDF_STATUS "HPDF_UseCNTEncodings"))

(define HPDF_Page_CreateTextAnnot (c-lambda (HPDF_Page HPDF_Rect char-string HPDF_Encoder) HPDF_Annotation "HPDF_Page_CreateTextAnnot"))
(define HPDF_Page_CreateLinkAnnot (c-lambda (HPDF_Page HPDF_Rect HPDF_Destination) HPDF_Annotation "HPDF_Page_CreateLinkAnnot"))
(define HPDF_Page_CreateURILinkAnnot (c-lambda (HPDF_Page HPDF_Rect char-string) HPDF_Annotation "HPDF_Page_CreateURILinkAnnot"))
(define HPDF_LinkAnnot_SetHighlightMode (c-lambda (HPDF_Annotation HPDF_AnnotHighlightMode) HPDF_STATUS "HPDF_LinkAnnot_SetHighlightMode"))
(define HPDF_LinkAnnot_SetBorderStyle (c-lambda (HPDF_Annotation HPDF_REAL HPDF_UINT16 HPDF_UINT16) HPDF_STATUS "HPDF_LinkAnnot_SetBorderStyle"))
(define HPDF_TextAnnot_SetIcon (c-lambda (HPDF_Annotation HPDF_AnnotIcon) HPDF_STATUS "HPDF_TextAnnot_SetIcon"))
(define HPDF_TextAnnot_SetOpened (c-lambda (HPDF_Annotation HPDF_BOOL) HPDF_STATUS "HPDF_TextAnnot_SetOpened"))

(define HPDF_LoadPngImageFromFile (c-lambda (HPDF_Doc char-string) HPDF_Image "HPDF_LoadPngImageFromFile"))
(define HPDF_LoadPngImageFromFile2 (c-lambda (HPDF_Doc char-string) HPDF_Image "HPDF_LoadPngImageFromFile2"))
(define HPDF_LoadJpegImageFromFile (c-lambda (HPDF_Doc char-string) HPDF_Image "HPDF_LoadJpegImageFromFile"))
(define HPDF_LoadRawImageFromFile (c-lambda (HPDF_Doc char-string HPDF_UINT HPDF_UINT HPDF_ColorSpace) HPDF_Image "HPDF_LoadRawImageFromFile"))
(define HPDF_LoadRawImageFromMem (c-lambda (HPDF_Doc HPDF_BYTE* HPDF_UINT HPDF_UINT HPDF_ColorSpace HPDF_UINT) HPDF_Image "HPDF_LoadRawImageFromMem"))
(define HPDF_Image_GetSize (c-lambda (HPDF_Image) HPDF_Point "HPDF_Image_GetSize"))
(define HPDF_Image_GetSize2 (c-lambda (HPDF_Image HPDF_Point*) HPDF_STATUS "HPDF_Image_GetSize2"))
(define HPDF_Image_GetWidth (c-lambda (HPDF_Image) HPDF_UINT "HPDF_Image_GetWidth"))
(define HPDF_Image_GetHeight (c-lambda (HPDF_Image) HPDF_UINT "HPDF_Image_GetHeight"))
(define HPDF_Image_GetBitsPerComponent (c-lambda (HPDF_Image) HPDF_UINT "HPDF_Image_GetBitsPerComponent"))
(define HPDF_Image_GetColorSpace (c-lambda (HPDF_Image) char-string "HPDF_Image_GetColorSpace"))
(define HPDF_Image_SetColorMask (c-lambda (HPDF_Image HPDF_UINT HPDF_UINT HPDF_UINT HPDF_UINT HPDF_UINT HPDF_UINT) HPDF_STATUS"HPDF_Image_SetColorMask"))
(define HPDF_Image_SetMaskImage (c-lambda (HPDF_Image HPDF_Image) HPDF_STATUS "HPDF_Image_SetMaskImage"))

(define HPDF_SetInfoAttr (c-lambda (HPDF_Doc HPDF_InfoType char-string) HPDF_STATUS "HPDF_SetInfoAttr"))
(define HPDF_GetInfoAttr (c-lambda (HPDF_Doc HPDF_InfoType) char-string "HPDF_GetInfoAttr"))
(define HPDF_SetInfoDateAttr (c-lambda (HPDF_Doc HPDF_InfoType HPDF_Date) HPDF_STATUS "HPDF_SetInfoDateAttr"))

(define HPDF_SetPassword (c-lambda (HPDF_Doc char-string char-string) HPDF_STATUS "HPDF_SetPassword"))
(define HPDF_SetPermission (c-lambda (HPDF_Doc HPDF_UINT) HPDF_STATUS "HPDF_SetPermission"))
(define HPDF_SetEncryptionMode (c-lambda (HPDF_Doc HPDF_EncryptMode HPDF_UINT) HPDF_STATUS "HPDF_SetEncryptionMode"))

(define HPDF_SetCompressionMode (c-lambda (HPDF_Doc HPDF_UINT) HPDF_STATUS "HPDF_SetCompressionMode"))

(define HPDF_Font_GetFontName (c-lambda (HPDF_Font) char-string "HPDF_Font_GetFontName"))
(define HPDF_Font_GetEncodingName (c-lambda (HPDF_Font) char-string "HPDF_Font_GetEncodingName"))
(define HPDF_Font_GetUnicodeWidth (c-lambda (HPDF_Font HPDF_UNICODE) HPDF_INT "HPDF_Font_GetUnicodeWidth"))
(define HPDF_Font_GetBBox (c-lambda (HPDF_Font) HPDF_Box "HPDF_Font_GetBBox"))
(define HPDF_Font_GetAscent (c-lambda (HPDF_Font) HPDF_INT "HPDF_Font_GetAscent"))
(define HPDF_Font_GetDescent (c-lambda (HPDF_Font) HPDF_INT "HPDF_Font_GetDescent"))
(define HPDF_Font_GetXHeight (c-lambda (HPDF_Font) HPDF_UINT "HPDF_Font_GetXHeight"))
(define HPDF_Font_GetCapHeight (c-lambda (HPDF_Font) HPDF_UINT "HPDF_Font_GetCapHeight"))
(define HPDF_Font_TextWidth (c-lambda (HPDF_Font HPDF_BYTE* HPDF_UINT) HPDF_TextWidth "HPDF_Font_TextWidth"))
(define HPDF_Font_MeasureText (c-lambda (HPDF_Font HPDF_BYTE* HPDF_UINT HPDF_REAL HPDF_REAL HPDF_REAL HPDF_REAL HPDF_BOOL HPDF_REAL*) HPDF_UINT "HPDF_Font_MeasureText"))

(define HPDF_CreateExtGState (c-lambda (HPDF_Doc) HPDF_ExtGState "HPDF_CreateExtGState"))
(define HPDF_ExtGState_SetAlphaStroke (c-lambda (HPDF_ExtGState HPDF_REAL) HPDF_STATUS "HPDF_ExtGState_SetAlphaStroke"))
(define HPDF_ExtGState_SetAlphaFill (c-lambda (HPDF_ExtGState HPDF_REAL) HPDF_STATUS "HPDF_ExtGState_SetAlphaFill"))
(define HPDF_ExtGState_SetBlendMode (c-lambda (HPDF_ExtGState HPDF_BlendMode) HPDF_STATUS "HPDF_ExtGState_SetBlendMode"))

(define HPDF_Page_TextWidth (c-lambda (HPDF_Page char-string) HPDF_REAL "HPDF_Page_TextWidth"))
(define HPDF_Page_MeasureText (c-lambda (HPDF_Page char-string HPDF_REAL HPDF_BOOL HPDF_REAL*) HPDF_UINT "HPDF_Page_MeasureText"))
(define HPDF_Page_GetWidth (c-lambda (HPDF_Page) HPDF_REAL "HPDF_Page_GetWidth"))
(define HPDF_Page_GetHeight (c-lambda (HPDF_Page) HPDF_REAL "HPDF_Page_GetHeight"))
(define HPDF_Page_GetGMode (c-lambda (HPDF_Page) HPDF_UINT16 "HPDF_Page_GetGMode"))
(define HPDF_Page_GetCurrentPos (c-lambda (HPDF_Page) HPDF_Point "HPDF_Page_GetCurrentPos"))
(define HPDF_Page_GetCurrentPos2 (c-lambda (HPDF_Page HPDF_Point*) HPDF_STATUS "HPDF_Page_GetCurrentPos2"))
(define HPDF_Page_GetCurrentTextPos (c-lambda (HPDF_Page) HPDF_Point "HPDF_Page_GetCurrentTextPos"))
(define HPDF_Page_GetCurrentTextPos2 (c-lambda (HPDF_Page HPDF_Point*) HPDF_STATUS "HPDF_Page_GetCurrentTextPos2"))
(define HPDF_Page_GetCurrentFont (c-lambda (HPDF_Page) HPDF_Font "HPDF_Page_GetCurrentFont"))
(define HPDF_Page_GetCurrentFontSize (c-lambda (HPDF_Page) HPDF_REAL "HPDF_Page_GetCurrentFontSize"))
(define HPDF_Page_GetTransMatrix (c-lambda (HPDF_Page) HPDF_TransMatrix "HPDF_Page_GetTransMatrix"))
(define HPDF_Page_GetLineWidth (c-lambda (HPDF_Page) HPDF_REAL "HPDF_Page_GetLineWidth"))
(define HPDF_Page_GetLineCap (c-lambda (HPDF_Page) HPDF_LineCap "HPDF_Page_GetLineCap"))
(define HPDF_Page_GetLineJoin (c-lambda (HPDF_Page) HPDF_LineJoin "HPDF_Page_GetLineJoin"))
(define HPDF_Page_GetMiterLimit (c-lambda (HPDF_Page) HPDF_REAL "HPDF_Page_GetMiterLimit"))
(define HPDF_Page_GetDash (c-lambda (HPDF_Page) HPDF_DashMode "HPDF_Page_GetDash"))
(define HPDF_Page_GetFlat (c-lambda (HPDF_Page) HPDF_REAL "HPDF_Page_GetFlat"))
(define HPDF_Page_GetCharSpace (c-lambda (HPDF_Page) HPDF_REAL "HPDF_Page_GetCharSpace"))
(define HPDF_Page_GetWordSpace (c-lambda (HPDF_Page) HPDF_REAL "HPDF_Page_GetWordSpace"))
(define HPDF_Page_GetHorizontalScalling (c-lambda (HPDF_Page) HPDF_REAL "HPDF_Page_GetHorizontalScalling"))
(define HPDF_Page_GetTextLeading (c-lambda (HPDF_Page) HPDF_REAL "HPDF_Page_GetTextLeading"))
(define HPDF_Page_GetTextRenderingMode (c-lambda (HPDF_Page) HPDF_TextRenderingMode "HPDF_Page_GetTextRenderingMode"))
(define HPDF_Page_GetTextRaise (c-lambda (HPDF_Page) HPDF_REAL "HPDF_Page_GetTextRaise"))
(define HPDF_Page_GetTextRise (c-lambda (HPDF_Page) HPDF_REAL "HPDF_Page_GetTextRise"))
(define HPDF_Page_GetRGBFill (c-lambda (HPDF_Page) HPDF_RGBColor "HPDF_Page_GetRGBFill"))
(define HPDF_Page_GetRGBStroke (c-lambda (HPDF_Page) HPDF_RGBColor "HPDF_Page_GetRGBStroke"))
(define HPDF_Page_GetCMYKFill (c-lambda (HPDF_Page) HPDF_CMYKColor "HPDF_Page_GetCMYKFill"))
(define HPDF_Page_GetCMYKStroke (c-lambda (HPDF_Page) HPDF_CMYKColor "HPDF_Page_GetCMYKStroke"))
(define HPDF_Page_GetGrayFill (c-lambda (HPDF_Page) HPDF_REAL "HPDF_Page_GetGrayFill"))
(define HPDF_Page_GetGrayStroke (c-lambda (HPDF_Page) HPDF_REAL "HPDF_Page_GetGrayStroke"))
(define HPDF_Page_GetStrokingColorSpace (c-lambda (HPDF_Page) HPDF_ColorSpace "HPDF_Page_GetStrokingColorSpace"))
(define HPDF_Page_GetFillingColorSpace (c-lambda (HPDF_Page) HPDF_ColorSpace "HPDF_Page_GetFillingColorSpace"))
(define HPDF_Page_GetTextMatrix (c-lambda (HPDF_Page) HPDF_TransMatrix "HPDF_Page_GetTextMatrix"))
(define HPDF_Page_GetGStateDepth (c-lambda (HPDF_Page) HPDF_UINT "HPDF_Page_GetGStateDepth"))

(define HPDF_Page_SetLineWidth (c-lambda (HPDF_Page HPDF_REAL) HPDF_STATUS "HPDF_Page_SetLineWidth"))
(define HPDF_Page_SetLineCap (c-lambda (HPDF_Page HPDF_LineCap) HPDF_STATUS "HPDF_Page_SetLineCap"))
(define HPDF_Page_SetLineJoin (c-lambda (HPDF_Page HPDF_LineJoin) HPDF_STATUS "HPDF_Page_SetLineJoin"))
(define HPDF_Page_SetMiterLimit (c-lambda (HPDF_Page HPDF_REAL) HPDF_STATUS "HPDF_Page_SetMiterLimit"))
(define HPDF_Page_SetDash (c-lambda (HPDF_Page HPDF_UINT16* HPDF_UINT HPDF_UINT) HPDF_STATUS "HPDF_Page_SetDash"))
(define HPDF_Page_SetFlat (c-lambda (HPDF_Page HPDF_REAL) HPDF_STATUS "HPDF_Page_SetFlat"))
(define HPDF_Page_SetExtGState (c-lambda (HPDF_Page HPDF_ExtGState) HPDF_STATUS "HPDF_Page_SetExtGState"))

(define HPDF_Page_GSave (c-lambda (HPDF_Page) HPDF_STATUS "HPDF_Page_GSave"))
(define HPDF_Page_GRestore (c-lambda (HPDF_Page) HPDF_STATUS "HPDF_Page_GRestore"))
(define HPDF_Page_Concat (c-lambda (HPDF_Page HPDF_REAL HPDF_REAL HPDF_REAL HPDF_REAL HPDF_REAL HPDF_REAL) HPDF_STATUS "HPDF_Page_Concat"))
(define HPDF_Page_MoveTo (c-lambda (HPDF_Page HPDF_REAL HPDF_REAL) HPDF_STATUS "HPDF_Page_MoveTo"))
(define HPDF_Page_LineTo (c-lambda (HPDF_Page HPDF_REAL HPDF_REAL) HPDF_STATUS "HPDF_Page_LineTo"))
(define HPDF_Page_CurveTo (c-lambda (HPDF_Page HPDF_REAL HPDF_REAL HPDF_REAL HPDF_REAL HPDF_REAL HPDF_REAL) HPDF_STATUS "HPDF_Page_CurveTo"))
(define HPDF_Page_CurveTo2 (c-lambda (HPDF_Page HPDF_REAL HPDF_REAL HPDF_REAL HPDF_REAL) HPDF_STATUS "HPDF_Page_CurveTo2"))
(define HPDF_Page_CurveTo3 (c-lambda (HPDF_Page HPDF_REAL HPDF_REAL HPDF_REAL HPDF_REAL) HPDF_STATUS "HPDF_Page_CurveTo3"))
(define HPDF_Page_ClosePath (c-lambda (HPDF_Page) HPDF_STATUS "HPDF_Page_ClosePath"))
(define HPDF_Page_Rectangle (c-lambda (HPDF_Page HPDF_REAL HPDF_REAL HPDF_REAL HPDF_REAL) HPDF_STATUS "HPDF_Page_Rectangle"))

(define HPDF_Page_Stroke (c-lambda (HPDF_Page) HPDF_STATUS "HPDF_Page_Stroke"))
(define HPDF_Page_ClosePathStroke (c-lambda (HPDF_Page) HPDF_STATUS "HPDF_Page_ClosePathStroke"))
(define HPDF_Page_Fill (c-lambda (HPDF_Page) HPDF_STATUS "HPDF_Page_Fill"))
(define HPDF_Page_Eofill (c-lambda (HPDF_Page) HPDF_STATUS "HPDF_Page_Eofill"))
(define HPDF_Page_FillStroke (c-lambda (HPDF_Page) HPDF_STATUS "HPDF_Page_FillStroke"))
(define HPDF_Page_EofillStroke (c-lambda (HPDF_Page) HPDF_STATUS "HPDF_Page_EofillStroke"))
(define HPDF_Page_ClosePathFillStroke (c-lambda (HPDF_Page) HPDF_STATUS "HPDF_Page_ClosePathFillStroke"))
(define HPDF_Page_ClosePathEofillStroke (c-lambda (HPDF_Page) HPDF_STATUS "HPDF_Page_ClosePathEofillStroke"))
(define HPDF_Page_EndPath (c-lambda (HPDF_Page) HPDF_STATUS "HPDF_Page_EndPath"))

(define HPDF_Page_Clip (c-lambda (HPDF_Page) HPDF_STATUS "HPDF_Page_Clip"))
(define HPDF_Page_Eoclip (c-lambda (HPDF_Page) HPDF_STATUS "HPDF_Page_Eoclip"))

(define HPDF_Page_BeginText (c-lambda (HPDF_Page) HPDF_STATUS "HPDF_Page_BeginText"))
(define HPDF_Page_EndText (c-lambda (HPDF_Page) HPDF_STATUS "HPDF_Page_EndText"))

(define HPDF_Page_SetCharSpace (c-lambda (HPDF_Page HPDF_REAL) HPDF_STATUS "HPDF_Page_SetCharSpace"))
(define HPDF_Page_SetWordSpace (c-lambda (HPDF_Page HPDF_REAL) HPDF_STATUS "HPDF_Page_SetWordSpace"))
(define HPDF_Page_SetHorizontalScalling (c-lambda (HPDF_Page HPDF_REAL) HPDF_STATUS "HPDF_Page_SetHorizontalScalling"))
(define HPDF_Page_SetTextLeading (c-lambda (HPDF_Page HPDF_REAL) HPDF_STATUS "HPDF_Page_SetTextLeading"))
(define HPDF_Page_SetFontAndSize (c-lambda (HPDF_Page HPDF_Font HPDF_REAL) HPDF_STATUS "HPDF_Page_SetFontAndSize"))
(define HPDF_Page_SetTextRenderingMode (c-lambda (HPDF_Page HPDF_TextRenderingMode) HPDF_STATUS "HPDF_Page_SetTextRenderingMode"))
(define HPDF_Page_SetTextRise (c-lambda (HPDF_Page HPDF_REAL) HPDF_STATUS "HPDF_Page_SetTextRise"))
(define HPDF_Page_SetTextRaise (c-lambda (HPDF_Page HPDF_REAL) HPDF_STATUS "HPDF_Page_SetTextRaise"))

(define HPDF_Page_MoveTextPos (c-lambda (HPDF_Page HPDF_REAL HPDF_REAL) HPDF_STATUS "HPDF_Page_MoveTextPos"))
(define HPDF_Page_MoveTextPos2 (c-lambda (HPDF_Page HPDF_REAL HPDF_REAL) HPDF_STATUS "HPDF_Page_MoveTextPos2"))
(define HPDF_Page_SetTextMatrix (c-lambda (HPDF_Page HPDF_REAL HPDF_REAL HPDF_REAL HPDF_REAL HPDF_REAL HPDF_REAL) HPDF_STATUS "HPDF_Page_SetTextMatrix"))
(define HPDF_Page_MoveToNextLine (c-lambda (HPDF_Page) HPDF_STATUS "HPDF_Page_MoveToNextLine"))

(define HPDF_Page_ShowText (c-lambda (HPDF_Page char-string) HPDF_STATUS "HPDF_Page_ShowText"))
(define HPDF_Page_ShowTextNextLine (c-lambda (HPDF_Page char-string) HPDF_STATUS "HPDF_Page_ShowTextNextLine"))
(define HPDF_Page_ShowTextNextLineEx (c-lambda (HPDF_Page HPDF_REAL HPDF_REAL char-string) HPDF_STATUS "HPDF_Page_ShowTextNextLineEx"))

(define HPDF_Page_SetGrayFill (c-lambda (HPDF_Page HPDF_REAL) HPDF_STATUS "HPDF_Page_SetGrayFill"))
(define HPDF_Page_SetGrayStroke (c-lambda (HPDF_Page HPDF_REAL) HPDF_STATUS "HPDF_Page_SetGrayStroke"))
(define HPDF_Page_SetRGBFill (c-lambda (HPDF_Page HPDF_REAL HPDF_REAL HPDF_REAL) HPDF_STATUS "HPDF_Page_SetRGBFill"))
(define HPDF_Page_SetRGBStroke (c-lambda (HPDF_Page HPDF_REAL HPDF_REAL HPDF_REAL) HPDF_STATUS "HPDF_Page_SetRGBStroke"))
(define HPDF_Page_SetCMYKFill (c-lambda (HPDF_Page HPDF_REAL HPDF_REAL HPDF_REAL HPDF_REAL) HPDF_STATUS "HPDF_Page_SetCMYKFill"))
(define HPDF_Page_SetCMYKStroke (c-lambda (HPDF_Page HPDF_REAL HPDF_REAL HPDF_REAL HPDF_REAL) HPDF_STATUS "HPDF_Page_SetCMYKStroke"))

(define HPDF_Page_ExecuteXObject (c-lambda (HPDF_Page HPDF_XObject) HPDF_STATUS "HPDF_Page_ExecuteXObject"))

(define HPDF_Page_DrawImage (c-lambda (HPDF_Page HPDF_Image HPDF_REAL HPDF_REAL HPDF_REAL HPDF_REAL) HPDF_STATUS "HPDF_Page_DrawImage"))
(define HPDF_Page_Circle (c-lambda (HPDF_Page HPDF_REAL HPDF_REAL HPDF_REAL) HPDF_STATUS "HPDF_Page_Circle"))
(define HPDF_Page_Ellipse (c-lambda (HPDF_Page HPDF_REAL HPDF_REAL HPDF_REAL HPDF_REAL) HPDF_STATUS "HPDF_Page_Ellipse"))
(define HPDF_Page_Arc (c-lambda (HPDF_Page HPDF_REAL HPDF_REAL HPDF_REAL HPDF_REAL HPDF_REAL) HPDF_STATUS "HPDF_Page_Arc"))
(define HPDF_Page_TextOut (c-lambda (HPDF_Page HPDF_REAL HPDF_REAL char-string) HPDF_STATUS "HPDF_Page_TextOut"))
(define HPDF_Page_TextRect (c-lambda (HPDF_Page HPDF_REAL HPDF_REAL HPDF_REAL HPDF_REAL char-string HPDF_TextAlignment HPDF_UINT*) HPDF_STATUS "HPDF_Page_TextRect"))
(define HPDF_Page_SetSlideShow (c-lambda (HPDF_Page HPDF_TransitionStyle HPDF_REAL HPDF_REAL) HPDF_STATUS "HPDF_Page_SetSlideShow"))

(define HPDF_Page_DrawRGB 
  (c-lambda (HPDF_Doc HPDF_Page scheme-object HPDF_INT HPDF_INT HPDF_REAL HPDF_REAL) void 
    "HPDF_Page_DrawRGB(___arg1,___arg2,___CAST(void*,___BODY_AS(___arg3,___tSUBTYPED)),___arg4,___arg5,___arg6,___arg7);"))

;; eof
