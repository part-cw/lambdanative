/*
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
*/
// tool to generate texture from string

#include <math.h>
#include <wchar.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>
#include <assert.h>

#include "vec234.h"
#include "vector.h"
#include "texture-atlas.h"
#include "texture-font.h"

extern size_t stringwidth, stringheight;

// for testing
void printpgmatlas(char *name, texture_atlas_t *a)
{
  unsigned char ppmline[256];
  FILE *fd = fopen(name,"wb");
  if (fd) {
    sprintf(ppmline, "P5\n%u %u\n255\n", (int)a->width, (int)a->height);
    fputs(ppmline,fd);
    fwrite(a->data,1L,a->width*a->height, fd);
    fclose(fd);
  }
}

void printscmatlas(char *name, texture_atlas_t *a)
{
  int i;
  int w = a->width; 
  int h = a->height;
  int depth = a->depth;
  unsigned char *data = a->data;
  printf("(define %s.raw (glCoreTextureCreate %i %i '#u8(",name, w ,h );
  if (depth==4) {
    for (i=0;i<4*w*h;i++) { printf("%i",(int)data[i]); if (i<4*w*h-1) printf(" "); }
  }
  if (depth==3) {
    for (i=0;i<3*w*h;i++) { printf("%i",(int)data[i]); if (i<3*w*h-1) printf(" "); } 
  } 
  if (depth==1) {
    for (i=0;i<w*h;i++) { printf("%i",(int)data[i]); if (i<w*h-1) printf(" "); } 
  }
  printf(")))\n");
  printf("(define %s.img (list %i %i %s.raw 0. %f %f 0.))\n", name,
    (int)stringwidth, (int)stringheight,name,
    ((double)stringheight-1)/(double)h,
    (double)stringwidth/(double)w
  );
}

size_t tryatlas(size_t w, size_t h, char *fontname, size_t ptsize, wchar_t *msg, char *tag)
{
  size_t i, missed=0;
  texture_atlas_t * atlas;
  atlas=texture_atlas_new( w, h, 1 );
  texture_font_t * font = texture_font_new( atlas, fontname, ptsize );
  missed = texture_font_load_glyphs( font, msg);
  if (missed==0) printscmatlas(tag,atlas); 
  texture_font_delete( font );
  texture_atlas_delete(atlas); 
  return missed;
}

void makeatlas(char *fontname, size_t ptsize, wchar_t *msg, char *tag)
{
  size_t sizes[]={8,16,32,64,128,256,512,1024};
  int i,j;
  for (j=0;j<8;j++) {
    for (i=0;i<8;i++) {
     if (tryatlas(sizes[i],sizes[j],fontname,ptsize,msg,tag)==0) goto success;
    }
  }
  fprintf(stderr, "FATAL: Maximum texture size exceeded\n");
  return;
success:
  return;
}

void usage(void)
{
  printf("Usage: ttfstr2scm <ttf file> <pointsize> <string> <string tag>\n");
  printf("Generates texture string on standard out\n");
  exit(0);
} 

int main( int argc, char **argv )
{
  wchar_t wbuf[256];
  if (argc!=5) usage();
  swprintf(wbuf,100,L"%s",argv[3]);
  makeatlas(argv[1], atoi(argv[2]),wbuf,argv[4]);
  return 0;
}

// eof
