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

;; CRC8 and CRC32 functions for u8vectors

(c-declare  #<<end-of-c-declare

unsigned char reg=0;

unsigned char crc8_bytecalc(unsigned char byte){
  int i;
  char flag;
  unsigned char polynom = 0xd5;

  for(i=0; i<8; i++){
    if (reg&0x80){ flag=1; }else{ flag=0; }
    reg <<= 1;
    if (byte&0x80){ reg|=1; }
    byte <<= 1;
    if (flag){ reg ^= polynom; }
  }
  return reg;
}

unsigned char crc8(unsigned char *data, int len){
  int i;
  reg=0;
  for(i=0; i<len; i++) {
    crc8_bytecalc(data[i]);
  }
  return crc8_bytecalc(0);
}

static unsigned int crctab[256];
#define QUOTIENT 0x04c11db7

void crc32_init(void){
  int i, j;
  unsigned int crc;
  for (i = 0; i < 256; i++) {
    crc = i << 24;
    for (j = 0; j < 8; j++) {
      if (crc & 0x80000000)
        crc = (crc << 1) ^ QUOTIENT;
      else
        crc = crc << 1;
    }
    crctab[i] = crc;
  }
}

unsigned int crc32(unsigned char *data, int len){
  unsigned int result;
  int i;
  unsigned char octet;
  if (len < 4) return 0;
  result = *data++ << 24;
  result |= *data++ << 16;
  result |= *data++ << 8;
  result |= *data++;
  result = ~ result;
  len -=4;
  for (i=0; i<len; i++) {
    result = (result << 8 | *data++) ^ crctab[result >> 24];
  }
  return ~result;
}

unsigned int crc16_ccitt(unsigned char *data, int len){
  int i;
  unsigned short crc=0xffff;
  for (i=0;i<len;i++){
    unsigned short crc_new = (unsigned char)(crc >> 8) | (crc << 8);
    crc_new ^= data[i];
    crc_new ^= (unsigned char)(crc_new & 0xff) >> 4;
    crc_new ^= crc_new << 12;
    crc_new ^= (crc_new & 0xff) << 5;
    crc = crc_new;
  }
  return crc;
}

end-of-c-declare
)

;; initialize the lookup table
(c-initialize "crc32_init();")

(define (u8vector-crc32 v) 
  ((c-lambda (scheme-object int) unsigned-int 
      "___result=crc32(___CAST(void*,___BODY_AS(___arg1,___tSUBTYPED)), ___arg2);")
     v (u8vector-length v)))

(define (u8vector-crc16-ccitt v)
  ((c-lambda (scheme-object int) unsigned-int
     "___result=crc16_ccitt(___CAST(void*,___BODY_AS(___arg1,___tSUBTYPED)), ___arg2);")
     v (u8vector-length v)))

(define (u8vector-crc8 v)
  ((c-lambda (scheme-object int) unsigned-int
      "___result=crc8(___CAST(void*,___BODY_AS(___arg1,___tSUBTYPED)), ___arg2);")
     v (u8vector-length v)))

;; eof