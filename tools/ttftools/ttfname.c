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
// tool to output font family name

#include <ft2build.h>
#include FT_FREETYPE_H
#include FT_SFNT_NAMES_H

void usage(void)
{
  printf("Usage: ttfname <ttf file>\n");
  printf("Prints font family name on stdout\n");
  exit(0);
} 

int main( int argc, char **argv )
{
  int i;
  FT_Error error;
  FT_Library  library;
  FT_Face     face;  
  if (argc!=2) usage();
  error = FT_Init_FreeType( &library );
  if ( error ) { fprintf(stderr,"ERROR: failed in FT_Init_FreeType()\n"); exit(1); }
  error = FT_New_Face( library, argv[1], 0, &face );
  if ( error ) { fprintf(stderr,"ERROR: failed in FT_New_Face()\n"); exit(1); }
  FT_SfntName aname;
  error = FT_Get_Sfnt_Name(face, 1, &aname ); 
  if ( error ) { fprintf(stderr,"ERROR: failed in FT_GetSfnt_Name()\n"); exit(1); }
  int len = aname.string_len+1;
  char *buf = (char *)malloc(len);
  memset(buf,0,len);
  memcpy(buf,aname.string,len-1);
  if (len>2&&buf[0]==0) {
    for (i=0;i<len/2;i++) { buf[i]=buf[2*i+1]; }
    buf[len>>1]=0;
  }
  printf("%s\n",buf);
  FT_Done_Face(face); 
  FT_Done_FreeType(library); 
  return 0;
}

// eof
