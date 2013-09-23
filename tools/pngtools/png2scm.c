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
// tool to convert png images to scheme textures

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#include <png.h>

int w, h;

#define TYPE_RGB 1
#define TYPE_RGBA 2
#define TYPE_ALPHA 3
int type=0;

unsigned char *png_load(char *fname)
{
  int i, x, y;
  int bit_depth, color_type;
  unsigned char *image_data;
  png_structp png_ptr;
  png_infop info_ptr;
  png_bytep *row_pointers;
  png_uint_32  width, height;
  int channels;
  int rowbytes;
  unsigned char *buffer;

  FILE *infile;
  unsigned char sig[8];
  infile = fopen(fname, "rb");
  if (!infile) return 0;
  fread(sig, 1, 8, infile);
  if (png_sig_cmp(sig, 0, 8) != 0) return 0;
  png_ptr = png_create_read_struct(PNG_LIBPNG_VER_STRING, NULL, NULL, NULL);
  if (!png_ptr) return 0; 
  info_ptr = png_create_info_struct(png_ptr);
  if (!info_ptr) { png_destroy_read_struct(&png_ptr, NULL, NULL); return 0; }
  png_init_io(png_ptr, infile);
  png_set_sig_bytes(png_ptr, 8);  
  png_read_info(png_ptr, info_ptr); 
  png_get_IHDR(png_ptr, info_ptr, &width, &height, &bit_depth, &color_type, NULL, NULL, NULL);
  buffer=(unsigned char *)malloc((int)width*(int)height*4);
  if (!buffer) { return 0; }
  w = width;
  h = height;
  if (color_type == PNG_COLOR_TYPE_PALETTE) png_set_expand(png_ptr);
  if (color_type == PNG_COLOR_TYPE_GRAY && bit_depth < 8) png_set_expand(png_ptr);
  if (png_get_valid(png_ptr, info_ptr, PNG_INFO_tRNS)) png_set_expand(png_ptr);
  if (bit_depth == 16) png_set_strip_16(png_ptr);
  if (color_type == PNG_COLOR_TYPE_GRAY || color_type == PNG_COLOR_TYPE_GRAY_ALPHA) png_set_gray_to_rgb(png_ptr);
  png_read_update_info(png_ptr, info_ptr);
  rowbytes = (int)png_get_rowbytes(png_ptr, info_ptr);
  channels = (int)png_get_channels(png_ptr, info_ptr);
  if (color_type == PNG_COLOR_TYPE_GRAY || color_type == PNG_COLOR_TYPE_GRAY_ALPHA) 
    type=TYPE_ALPHA; else { if (channels==4) type=TYPE_RGBA; else type=TYPE_RGB; }
  if ((image_data = (unsigned char  *)malloc(rowbytes*height)) == NULL) {
    png_destroy_read_struct(&png_ptr, &info_ptr, NULL);
    return 0;
  }
  if ((row_pointers = (png_bytepp)malloc(height*sizeof(png_bytep))) == NULL) {
    png_destroy_read_struct(&png_ptr, &info_ptr, NULL);
    free(image_data);
    return 0;
  }
  for (i = 0;  i < height;  ++i) { row_pointers[i] = image_data + (height-i-1)*rowbytes; }
  png_read_image(png_ptr, row_pointers);
  if (channels == 3 ) {
    for (x=0;x<width;x++) {
      for (y=0;y<height;y++) {
        buffer[4*(y*width+x)+0] = image_data[3*(y*width+x)];
        buffer[4*(y*width+x)+1] = image_data[3*(y*width+x)+1];
        buffer[4*(y*width+x)+2] = image_data[3*(y*width+x)+2];
        buffer[4*(y*width+x)+3] = 0xff;
      }
    }
  } else {
    if (channels == 4 ) { 
      for (x=0;x<width;x++) {
        for (y=0;y<height;y++) {
          buffer[4*(y*width+x)+0] = image_data[4*(y*width+x)];
          buffer[4*(y*width+x)+1] = image_data[4*(y*width+x)+1];
          buffer[4*(y*width+x)+2] = image_data[4*(y*width+x)+2];
          buffer[4*(y*width+x)+3] = image_data[4*(y*width+x)+3];
        }
      }
    }
  }
  png_read_end(png_ptr, NULL);
  free(image_data);
  free(row_pointers);
  png_destroy_read_struct(&png_ptr, &info_ptr, NULL);
  return buffer;
}

void usage(void)
{
  printf("usage: png2scm <png filename w/o suffix>\n");
  printf("convert an alpha channel png to rgba texture scheme structure on standard out\n");
  exit(1);
}

int fastlz_compress(const void*, int, void*);

void printcompressedtexture(unsigned char *data,int type, int w, int h)
{
  int i, maxlen, depth, clen, j;
  unsigned char *cdata;
  unsigned char *clean_data;
   
  switch (type) {
  case TYPE_ALPHA:
    depth=1;
    break;
  case TYPE_RGB:
    depth=3;
    break;
  case TYPE_RGBA:
    depth=4;
    break;
  }

  clean_data = (unsigned char *)malloc(depth*w*h);
  maxlen = 67+(int)(1.05*depth*w*h);
  cdata = (unsigned char *)malloc(maxlen);
  memset(clean_data,0,w*h*depth);

  j=0;
  switch (type) {
  case TYPE_ALPHA:
    for (i=0;i<4*w*h;i++) { if (i%4==0) {clean_data[j++] = data[i];} }
    break;
  case TYPE_RGB:
    for (i=0;i<4*w*h;i++) { if (i%4!=3) {clean_data[j++] = data[i];} }
    break;
  case TYPE_RGBA:
    memcpy(clean_data,data,depth*w*h);
    break;
  }

  clen = fastlz_compress(clean_data,depth*w*h,cdata);

  printf("1 ");
  for (i=0;i<clen;i++) {
    printf("%i",(int)cdata[i]);
    if (i<clen-1) printf(" ");
  }
  free(cdata);
  free(clean_data);
}

int main(int argc, char *argv[])
{
  char name[1024];memset(name,0,1024);
  if (argc!=2) usage();
  {
    int i,j,mark=-1;
    for (i=0;i<strlen(argv[1]);i++) {
      if (argv[1][i]=='/') mark=i;
    }
    strncpy(name,argv[1]+mark+1,strlen(argv[1])-mark-4-1);
    unsigned char *data=(unsigned char*)png_load(argv[1]), *data2;
    int w2 = (int)pow(2.,ceil(log(w)/log(2.)));
    int h2 = (int)pow(2.,ceil(log(h)/log(2.)));
    data2=(unsigned char*)malloc(w2*h2*4);
    memset(data2,0,w2*h2*4);
    for (i=0;i<h;i++) {
      memcpy(data2+4*i*w2,data+4*i*w,4*w);
    }
    if (type==TYPE_RGB) { 
      int i=0, bail=0;
      while (i++<w*h&&!bail) { 
        if (data[4*i]!=data[4*i+1]) bail=1;
        if (data[4*i+1]!=data[4*i+2]) bail=1;
      }
      if (!bail) type=TYPE_ALPHA;
    }
    if (data) {
      printf(";; Automatically generated. Do not edit.\n");
      printf(";; png2scm ver 2.0. type=%i\n", type);
      printf("(define %s.raw (glCoreTextureCreate %i %i (u8vector-decompress '#u8(",name, w2 ,h2 );
      printcompressedtexture(data2,type,w2,h2);
      printf("))))\n");
      printf("(define %s.img (list %i %i %s.raw 0. 0. %f %f))\n", name,w,h,name,(double)w/(double)w2, (double)h/(double)h2);
    } else {
     fprintf(stderr,"ERROR!\n");
    }
  }
}

