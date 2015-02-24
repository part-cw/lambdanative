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

;; minimal bindings for the libzint barcode generator

;; Note: libzint (zint backend) is licensed under BSD-3 as of May 2013

(define zint:debuglevel 0)
(define (zint:log level . x)
   (if (>= zint:debuglevel level) (apply log-system (append (list "zint: ") x))))

(c-declare  #<<end-of-c-declare

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <zint.h>

end-of-c-declare
)

(define BARCODE_CODE11 ((c-lambda () int "___result = BARCODE_CODE11;")))
(define BARCODE_C25MATRIX ((c-lambda () int "___result = BARCODE_C25MATRIX;")))
(define BARCODE_C25INTER ((c-lambda () int "___result = BARCODE_C25INTER;")))
(define BARCODE_C25IATA ((c-lambda () int "___result = BARCODE_C25IATA;")))
(define BARCODE_C25LOGIC ((c-lambda () int "___result = BARCODE_C25LOGIC;")))
(define BARCODE_C25IND ((c-lambda () int "___result = BARCODE_C25IND;")))
(define BARCODE_CODE39 ((c-lambda () int "___result = BARCODE_CODE39;")))
(define BARCODE_EXCODE39 ((c-lambda () int "___result = BARCODE_EXCODE39;")))
(define BARCODE_EANX ((c-lambda () int "___result = BARCODE_EANX;")))
(define BARCODE_EAN128 ((c-lambda () int "___result = BARCODE_EAN128;")))
(define BARCODE_CODABAR ((c-lambda () int "___result = BARCODE_CODABAR;")))
(define BARCODE_CODE128 ((c-lambda () int "___result = BARCODE_CODE128;")))
(define BARCODE_DPLEIT ((c-lambda () int "___result = BARCODE_DPLEIT;")))
(define BARCODE_DPIDENT ((c-lambda () int "___result = BARCODE_DPIDENT;")))
(define BARCODE_CODE16K ((c-lambda () int "___result = BARCODE_CODE16K;")))
(define BARCODE_CODE49 ((c-lambda () int "___result = BARCODE_CODE49;")))
(define BARCODE_CODE93 ((c-lambda () int "___result = BARCODE_CODE93;")))
(define BARCODE_FLAT ((c-lambda () int "___result = BARCODE_FLAT;")))
(define BARCODE_RSS14 ((c-lambda () int "___result = BARCODE_RSS14;")))
(define BARCODE_RSS_LTD ((c-lambda () int "___result = BARCODE_RSS_LTD;")))
(define BARCODE_RSS_EXP ((c-lambda () int "___result = BARCODE_RSS_EXP;")))
(define BARCODE_TELEPEN ((c-lambda () int "___result = BARCODE_TELEPEN;")))
(define BARCODE_UPCA ((c-lambda () int "___result = BARCODE_UPCA;")))
(define BARCODE_UPCE ((c-lambda () int "___result = BARCODE_UPCE;")))
(define BARCODE_POSTNET ((c-lambda () int "___result = BARCODE_POSTNET;")))
(define BARCODE_MSI_PLESSEY ((c-lambda () int "___result = BARCODE_MSI_PLESSEY;")))
(define BARCODE_FIM ((c-lambda () int "___result = BARCODE_FIM;")))
(define BARCODE_LOGMARS ((c-lambda () int "___result = BARCODE_LOGMARS;")))
(define BARCODE_PHARMA ((c-lambda () int "___result = BARCODE_PHARMA;")))
(define BARCODE_PZN ((c-lambda () int "___result = BARCODE_PZN;")))
(define BARCODE_PHARMA_TWO ((c-lambda () int "___result = BARCODE_PHARMA_TWO;")))
(define BARCODE_PDF417 ((c-lambda () int "___result = BARCODE_PDF417;")))
(define BARCODE_PDF417TRUNC ((c-lambda () int "___result = BARCODE_PDF417TRUNC;")))
(define BARCODE_MAXICODE ((c-lambda () int "___result = BARCODE_MAXICODE;")))
(define BARCODE_QRCODE ((c-lambda () int "___result = BARCODE_QRCODE;")))
(define BARCODE_CODE128B ((c-lambda () int "___result = BARCODE_CODE128B;")))
(define BARCODE_AUSPOST ((c-lambda () int "___result = BARCODE_AUSPOST;")))
(define BARCODE_AUSREPLY ((c-lambda () int "___result = BARCODE_AUSREPLY;")))
(define BARCODE_AUSROUTE ((c-lambda () int "___result = BARCODE_AUSROUTE;")))
(define BARCODE_AUSREDIRECT ((c-lambda () int "___result = BARCODE_AUSREDIRECT;")))
(define BARCODE_ISBNX ((c-lambda () int "___result = BARCODE_ISBNX;")))
(define BARCODE_RM4SCC ((c-lambda () int "___result = BARCODE_RM4SCC;")))
(define BARCODE_DATAMATRIX ((c-lambda () int "___result = BARCODE_DATAMATRIX;")))
(define BARCODE_EAN14 ((c-lambda () int "___result = BARCODE_EAN14;")))
(define BARCODE_CODABLOCKF ((c-lambda () int "___result = BARCODE_CODABLOCKF;")))
(define BARCODE_NVE18 ((c-lambda () int "___result = BARCODE_NVE18;")))
(define BARCODE_JAPANPOST ((c-lambda () int "___result = BARCODE_JAPANPOST;")))
(define BARCODE_KOREAPOST ((c-lambda () int "___result = BARCODE_KOREAPOST;")))
(define BARCODE_RSS14STACK ((c-lambda () int "___result = BARCODE_RSS14STACK;")))
(define BARCODE_RSS14STACK_OMNI ((c-lambda () int "___result = BARCODE_RSS14STACK_OMNI;")))
(define BARCODE_RSS_EXPSTACK ((c-lambda () int "___result = BARCODE_RSS_EXPSTACK;")))
(define BARCODE_PLANET ((c-lambda () int "___result = BARCODE_PLANET;")))
(define BARCODE_MICROPDF417 ((c-lambda () int "___result = BARCODE_MICROPDF417;")))
(define BARCODE_ONECODE ((c-lambda () int "___result = BARCODE_ONECODE;")))
(define BARCODE_PLESSEY ((c-lambda () int "___result = BARCODE_PLESSEY;")))

(define BARCODE_TELEPEN_NUM ((c-lambda () int "___result = BARCODE_TELEPEN_NUM;")))
(define BARCODE_ITF14 ((c-lambda () int "___result = BARCODE_ITF14;")))
(define BARCODE_KIX ((c-lambda () int "___result = BARCODE_KIX;")))
(define BARCODE_AZTEC ((c-lambda () int "___result = BARCODE_AZTEC;")))
(define BARCODE_DAFT ((c-lambda () int "___result = BARCODE_DAFT;")))
(define BARCODE_MICROQR ((c-lambda () int "___result = BARCODE_MICROQR;")))

(define BARCODE_HIBC_128 ((c-lambda () int "___result = BARCODE_HIBC_128;")))
(define BARCODE_HIBC_39 ((c-lambda () int "___result = BARCODE_HIBC_39;")))
(define BARCODE_HIBC_DM ((c-lambda () int "___result = BARCODE_HIBC_DM;")))
(define BARCODE_HIBC_QR ((c-lambda () int "___result = BARCODE_HIBC_QR;")))
(define BARCODE_HIBC_PDF ((c-lambda () int "___result = BARCODE_HIBC_PDF;")))
(define BARCODE_HIBC_MICPDF ((c-lambda () int "___result = BARCODE_HIBC_MICPDF;")))
(define BARCODE_HIBC_BLOCKF ((c-lambda () int "___result = BARCODE_HIBC_BLOCKF;")))
(define BARCODE_HIBC_AZTEC ((c-lambda () int "___result = BARCODE_HIBC_AZTEC;")))

(define BARCODE_AZRUNE ((c-lambda () int "___result = BARCODE_AZRUNE;")))
(define BARCODE_CODE32 ((c-lambda () int "___result = BARCODE_CODE32;")))
(define BARCODE_EANX_CC ((c-lambda () int "___result = BARCODE_EANX_CC;")))
(define BARCODE_EAN128_CC ((c-lambda () int "___result = BARCODE_EAN128_CC;")))
(define BARCODE_RSS14_CC ((c-lambda () int "___result = BARCODE_RSS14_CC;")))
(define BARCODE_RSS_LTD_CC ((c-lambda () int "___result = BARCODE_RSS_LTD_CC;")))
(define BARCODE_RSS_EXP_CC ((c-lambda () int "___result = BARCODE_RSS_EXP_CC;")))
(define BARCODE_UPCA_CC ((c-lambda () int "___result = BARCODE_UPCA_CC;")))
(define BARCODE_UPCE_CC ((c-lambda () int "___result = BARCODE_UPCE_CC;")))
(define BARCODE_RSS14STACK_CC ((c-lambda () int "___result = BARCODE_RSS14STACK_CC;")))
(define BARCODE_RSS14_OMNI_CC ((c-lambda () int "___result = BARCODE_RSS14_OMNI_CC;")))
(define BARCODE_RSS_EXPSTACK_CC ((c-lambda () int "___result = BARCODE_RSS_EXPSTACK_CC;")))
(define BARCODE_CHANNEL ((c-lambda () int "___result = BARCODE_CHANNEL;")))
(define BARCODE_CODEONE ((c-lambda () int "___result = BARCODE_CODEONE;")))
(define BARCODE_GRIDMATRIX ((c-lambda () int "___result = BARCODE_GRIDMATRIX;")))

(define BARCODE_NO_ASCII ((c-lambda () int "___result = BARCODE_NO_ASCII;")))
(define BARCODE_BIND ((c-lambda () int "___result = BARCODE_BIND;")))
(define BARCODE_BOX ((c-lambda () int "___result = BARCODE_BOX;")))
(define BARCODE_STDOUT ((c-lambda () int "___result = BARCODE_STDOUT;")))

(define READER_INIT ((c-lambda () int "___result = READER_INIT;")))
(define SMALL_TEXT ((c-lambda () int "___result = SMALL_TEXT;")))

(define DATA_MODE ((c-lambda () int "___result = DATA_MODE;")))
(define UNICODE_MODE ((c-lambda () int "___result = UNICODE_MODE;")))
(define GS1_MODE ((c-lambda () int "___result = GS1_MODE;")))
(define KANJI_MODE ((c-lambda () int "___result = KANJI_MODE;")))
(define SJIS_MODE ((c-lambda () int "___result = SJIS_MODE;")))

(define DM_SQUARE ((c-lambda () int "___result = DM_SQUARE;")))

(define WARN_INVALID_OPTION ((c-lambda () int "___result = WARN_INVALID_OPTION;")))

(define ERROR_TOO_LONG ((c-lambda () int "___result = ERROR_TOO_LONG;")))
(define ERROR_INVALID_DATA ((c-lambda () int "___result = ERROR_INVALID_DATA;")))
(define ERROR_INVALID_CHECK ((c-lambda () int "___result = ERROR_INVALID_CHECK;")))
(define ERROR_INVALID_OPTION ((c-lambda () int "___result = ERROR_INVALID_OPTION;")))
(define ERROR_ENCODING_PROBLEM ((c-lambda () int "___result = ERROR_ENCODING_PROBLEM;")))
(define ERROR_FILE_ACCESS ((c-lambda () int "___result = ERROR_FILE_ACCESS;")))
(define ERROR_MEMORY ((c-lambda () int "___result = ERROR_MEMORY;")))

(define ZBarcode_Create (c-lambda () (pointer void) "ZBarcode_Create"))
(define ZBarcode_Clear (c-lambda ((pointer void)) void "ZBarcode_Clear"))
(define ZBarcode_Delete (c-lambda ((pointer void)) void "ZBarcode_Delete"))

(define (ZBarcode_Encode ptr u8data)
  ((c-lambda ((pointer void) scheme-object int) int 
      "___result=ZBarcode_Encode(___arg1,___CAST(void*,___BODY_AS(___arg2,___tSUBTYPED)),___arg3);")
     ptr u8data (u8vector-length u8data)))
(define ZBarcode_Encode_File (c-lambda ((pointer void) char-string) int "ZBarcode_Encode_File"))

(define ZBarcode_Print (c-lambda ((pointer void) int) int "ZBarcode_Print"))
(define (ZBarcode_Encode_and_Print ptr u8data angle)
  ((c-lambda ((pointer void) scheme-object int int) int 
      "___result=ZBarcode_Encode_and_Print(___arg1,___CAST(void*,___BODY_AS(___arg2,___tSUBTYPED)),___arg3,___arg4);")
     ptr u8data (u8vector-length u8data) angle))
(define ZBarcode_Encode_File_and_Print (c-lambda ((pointer void) char-string int) int "ZBarcode_Encode_File_and_Print"))

(define ZBarcode_Render (c-lambda ((pointer void) float float) int "ZBarcode_Render"))

(define ZBarcode_Buffer (c-lambda ((pointer void) int) int "ZBarcode_Buffer"))
(define (ZBarcode_Encode_and_Buffer ptr u8data angle)
  ((c-lambda ((pointer void) scheme-object int int) int
      "___result=ZBarcode_Encode_and_Buffer(___arg1,___CAST(void*,___BODY_AS(___arg2,___tSUBTYPED)),___arg3,___arg4);")
     ptr u8data (u8vector-length u8data) angle))
(define ZBarcode_Encode_File_and_Buffer (c-lambda ((pointer void) char-string int) int "ZBarcode_Encode_File_and_Buffer"))

(define ZBarcode_ValidID (c-lambda (int) int "ZBarcode_ValidID"))

;; calls to modify the symbol structure

(define ZBarcode_symbology (c-lambda ((pointer void) int) void
   "((struct zint_symbol*)___arg1)->symbology=___arg2;"))

(define ZBarcode_height (c-lambda ((pointer void) int) void
   "((struct zint_symbol*)___arg1)->height=___arg2;"))

(define ZBarcode_whitespace_width (c-lambda ((pointer void) int) void
   "((struct zint_symbol*)___arg1)->whitespace_width=___arg2;"))

(define ZBarcode_border_width (c-lambda ((pointer void) int) void
   "((struct zint_symbol*)___arg1)->border_width=___arg2;"))

(define ZBarcode_output_options (c-lambda ((pointer void) int) void
   "((struct zint_symbol*)___arg1)->output_options=___arg2;"))

(define ZBarcode_fgcolor ZBarcode_fgcolour)
(define ZBarcode_fgcolour (c-lambda ((pointer void) char-string) void
   "strcpy(((struct zint_symbol*)___arg1)->fgcolour,___arg2);"))

(define ZBarcode_bgcolor ZBarcode_bgcolour)
(define ZBarcode_bgcolour (c-lambda ((pointer void) char-string) void
   "strcpy(((struct zint_symbol*)___arg1)->bgcolour,___arg2);"))

(define ZBarcode_outfile (c-lambda ((pointer void) char-string) void
   "strcpy(((struct zint_symbol*)___arg1)->outfile,___arg2);"))

(define ZBarcode_text (c-lambda ((pointer void) char-string) void
   "strcpy(((struct zint_symbol*)___arg1)->text,___arg2);"))

(define ZBarcode_primary (c-lambda ((pointer void) char-string) void
   "strcpy(((struct zint_symbol*)___arg1)->primary,___arg2);"))

;; eof
