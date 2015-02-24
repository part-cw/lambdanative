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

;; libdmtx data matrix barcode encoder/decoder

(c-declare  #<<end-of-c-declare

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <dmtx.h>
#include <gd.h>

gdImagePtr dmtx_enc_gd(unsigned char *str)
{ 
  int i, j;
  DmtxEncode *enc = dmtxEncodeCreate();
  if (!enc) return 0;
  dmtxEncodeDataMatrix(enc, strlen(str), str);
  int w = dmtxImageGetProp(enc->image, DmtxPropWidth);
  int h = dmtxImageGetProp(enc->image, DmtxPropHeight);
  int bpp = dmtxImageGetProp(enc->image, DmtxPropBytesPerPixel);
  gdImagePtr gd = gdImageCreateTrueColor(w,h);
  for (i=0;i<w;i++) {
    for (j=0;j<h;j++) {
      int v = enc->image->pxl[ bpp*(i+w*j) ];
      int c = gdImageColorAllocate(gd,v,v,v);
      gdImageSetPixel(gd,i,j,c);
    }
  }
  dmtxEncodeDestroy(&enc);
  return gd;
}

char *dmtx_dec_gd(gdImagePtr gd0)
{
  int i,j;
  int w0 = gdImageSX(gd0);
  int h0 = gdImageSY(gd0);
  int w = 400;
  int h = ( h0 * w )/ w0;
  gdImagePtr gd = gdImageCreateTrueColor(w,h);
  gdImageCopyResampled(gd,gd0,0,0,0,0,w,h,w0,h0);
  static char data[2335+1];
  data[0]=0;
  unsigned char *pxl = (unsigned char *)malloc(w*h);
  for (i=0;i<w;i++) {
    for (j=0;j<h;j++) {
      unsigned int c = gdImageGetTrueColorPixel(gd,i,j);
      int gs = (gdTrueColorGetRed(c) + gdTrueColorGetGreen(c) + gdTrueColorGetBlue(c))/3;
      pxl[i+w*j]=(unsigned char)gs;
    }
  }
  DmtxImage *img = dmtxImageCreate(pxl, w, h, DmtxPack8bppK);
  DmtxDecode *dec = dmtxDecodeCreate(img, 1);
  DmtxTime timeout = dmtxTimeAdd(dmtxTimeNow(),1000);
  DmtxRegion *reg = dmtxRegionFindNext(dec, &timeout);
  DmtxMessage *msg;
  if (reg != NULL) {
    msg = dmtxDecodeMatrixRegion(dec, reg, DmtxUndefined);
    if(msg != NULL) {
      memcpy(data,msg->output, msg->outputIdx);
      data[msg->outputIdx]=0; 
      dmtxMessageDestroy(&msg);
    }
    dmtxRegionDestroy(&reg);
  }
  dmtxDecodeDestroy(&dec);
  dmtxImageDestroy(&img);
  gdImageDestroy(gd);
  free(pxl);
  return data;
}

end-of-c-declare
)

(define dmtx->gd (c-lambda (char-string) (pointer void) "dmtx_enc_gd"))
(define gd->dmtx (c-lambda ((pointer void)) char-string "dmtx_dec_gd"))

;; eof
