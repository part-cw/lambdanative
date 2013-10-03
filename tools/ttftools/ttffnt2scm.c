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
// tool to generate texture mapped font from truetype

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

wchar_t *glyph_set=0;

int hex_digit(char c)
{
  int res=0;
  if (c>='0'&&c<='9') res=(int)c-'0';
  if (c>='a'&&c<='f') res=(int)c-'a'+10;
  if (c>='A'&&c<='F') res=(int)c-'A'+10;
  return res;
}

int _load_glyph_set(char *fname, wchar_t* gset)
{
  unsigned int i,n=0,l,a,b,factor;
  char buf[1024];
  FILE *fd=fopen(fname,"r");
  if (!fd) return -1;
  while (!feof(fd)) {
    buf[0]=0;
    fgets(buf,1024,fd);
    if (buf&&buf[0]!='#') {
      l=strlen(buf); 
      for (i=0;i<l;i++) { if (buf[i]<32) buf[i]=0; }
      l=strlen(buf); 
      if (buf[0]=='U'&&buf[1]=='+') {
        a=0;
        if (l==6||l==11) {
          factor=1;
          for (i=6-1;i>1;i--) {
            int digit=hex_digit(buf[i]);
            a+=factor*digit;
            factor*=16;
          }
        }
        b=0;
        if (l==11&&buf[6]=='-') {
          factor=1;
          for (i=11-1;i>6;i--) {
            int digit=hex_digit(buf[i]);
            b+=factor*digit;
            factor*=16;
          }
        } else b=a;
        for (i=a;i<=b;i++) {
          if (gset) gset[n]=(wchar_t)i;  
          n++;
        }
      }
    }
  }
  fclose(fd); 
  return n;
}

// load a glyph set from a file
void load_glyph_set(char *setarg)
{ 
  int nglyphs=_load_glyph_set(setarg,0);
  if (nglyphs>=0) {
    glyph_set=(wchar_t*)malloc((nglyphs+1)*sizeof(wchar_t));
    _load_glyph_set(setarg,glyph_set);
    glyph_set[nglyphs]=0;
  } else {
    fprintf(stderr,"ERROR: glyph set %s not valid. Font will be empty.\n",setarg);
  }
}

int fastlz_compress(const void*, int, void*);

int clen;

void printcompressedtexture(unsigned char *data, int len)
{
  int i, maxlen = 67+(int)(1.05*len);
  unsigned char *cdata = (unsigned char *)malloc(maxlen);
  clen = fastlz_compress(data,len,cdata);
  printf("// length=%i [%i]\n",clen,len);
  printf("1, ");
  for (i=0;i<clen;i++) {
    printf("%i",(int)cdata[i]);
    if (i<clen-1) printf(", ");
    if (i%32==0) printf("\n");
  }
  free(cdata);
}


void printscmatlas(char *name, texture_atlas_t *a)
{
  int i;
  int w = a->width; 
  int h = a->height;
  int depth = a->depth;
  unsigned char *data = a->data;

  char sane_name[1024];
  for (i=0;i<strlen(name);i++) {
    sane_name[i]='_';
    if (name[i]>='a'&&name[i]<='z') sane_name[i]=name[i];
    if (name[i]>='A'&&name[i]<='Z') sane_name[i]=name[i];
    if (name[i]>='0'&&name[i]<='9') sane_name[i]=name[i];
  }
  sane_name[strlen(name)]=0;
  printf("(c-declare  #<<end-of-c-declare\n#include <string.h>\nstatic unsigned char %s_font[]={\n",sane_name);
  printcompressedtexture(data,depth*w*h);
  printf("};\nend-of-c-declare\n)\n");
  printf("(define %s.z  (let ((u8v (make-u8vector %i))) ((c-lambda (scheme-object int) void \"memcpy(___CAST(void*,___BODY_AS(___arg1,___tSUBTYPED)),%s_font,___arg2);\") u8v %i) u8v))\n", name, clen+1, sane_name, clen+1);
  printf("(define %s.raw (glCoreTextureCreate %i %i (u8vector-decompress %s.z) GL_LINEAR GL_REPEAT))\n",name, w ,h, name );
}

void printscmfont(char *tag, texture_font_t *font, int w, int h)
{
  int i, glyph_count = font->glyphs->size;

// special 0 character encodes the total font height
  printf("(list 0 (list 0 %i %s.raw 0. 0. 0. 0.) 0 0 0)\n", (int)ceil(font->ascender-font->descender),tag);

  for( i=0; i < glyph_count; ++i ) {
    texture_glyph_t *glyph = *(texture_glyph_t **) vector_get( font->glyphs, i );
    int c = (int)glyph->charcode;
    if (c>0) {
      printf("(list %i ", (int)glyph->charcode );
      printf("(list %i %i %s.raw ", (int)glyph->width, (int)glyph->height, tag);
//    printf("%f %f %f %f)", glyph->s0, glyph->t1, glyph->s1, glyph->t0 );
      printf("%f %f %f %f)", glyph->s0, glyph->t1+.5/(double)h, glyph->s1+.5/(double)w, glyph->t0 );
      printf(" %i %f %i", (int)glyph->offset_x, glyph->advance_x, (int)glyph->offset_y);
      printf(")\n");
    }
  }
}

size_t tryatlas(size_t w, size_t h, char *fname, int *pointlist,char *tag)
{
  int ptidx=0;
  size_t i, missed=0;
  static texture_atlas_t * atlas = 0;
  if (tag&&atlas) printscmatlas(tag,atlas);
  if (atlas) { texture_atlas_delete(atlas); }
  atlas=texture_atlas_new( w, h, 1 );
 
  while (1) {
    char buf[128];
    i = pointlist[ptidx++];
    if (i==0) break;
    texture_font_t * font = texture_font_new( atlas, fname, i );
    if (tag) {
      sprintf(buf,"%s_%i.fnt", tag, (int)i);
      printf("(define %s (list\n", buf);
      sprintf(buf,"%s.raw", tag);
    }
    missed += texture_font_load_glyphs( font, glyph_set);
    if (tag) printscmfont(tag,font,w,h);
    texture_font_delete( font );
    if (tag) printf("))\n");
  }
  return missed;
}

void makeatlas(char *fname, int *pointlist, char *tag)
{
  size_t sizes[]={64,128,256,512,1024};
  int i,j;
  for (j=0;j<5;j++) {
    for (i=0;i<5;i++) {
     if (tryatlas(sizes[i],sizes[j],fname,pointlist,0)==0) goto success;
    }
  }
  fprintf(stderr, "FATAL: Maximum texture size exceeded\n");
  return;
success:
  tryatlas(sizes[i],sizes[j],fname,pointlist,tag);
}

void usage(void)
{
  printf("Usage: ttffnt2scm <ttf file> <ascii bit depth>  <comma separated point sizes> <font tag>\n");
  printf("Generates texture font data on standard out\n");
  exit(0);
} 

int main( int argc, char **argv )
{
  char *s1,*s2;
  int n=0,pointlist[]={0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
  if (argc!=5) usage();
  for( s2 = argv[3]; s2; ) {
    while( *s2 == ' ' || *s2 == '\t' ) s2++;
    s1 = strsep( &s2, "," );
    if( *s1 ) {
      int val;
      char ch;
      int ret = sscanf( s1, " %i %c", &val, &ch );
      if( ret == 1 ) { pointlist[n++]=val; }
    }
  }
  load_glyph_set(argv[2]);
  makeatlas(argv[1],pointlist,argv[4]);
  return 0;
}

// eof
