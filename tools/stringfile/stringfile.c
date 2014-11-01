/*
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
*/

#include <stdio.h>
#include <gd.h>

int main(int argc, char *argv[]){
  if (argc != 5){
    return 1;
  }
  // Command line arguments are Font, Fontsize, String, and PNG file name
  char *fnt = argv[1];
  double size = (double)atol(argv[2])*0.75; // To match Latex size scale down
  char *str = argv[3];
  char *fname = argv[4];

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
