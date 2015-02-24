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

;; truetype - support for font atlas and string rendering

(define ttf:debuglevel 0)
(define (ttf:log level . x)
   (if (>= ttf:debuglevel level) (apply log-system (append (list "ttf: ") x))))

(c-declare  #<<end-of-c-declare

/* =========================================================================
 * Freetype GL - A C OpenGL Freetype engine
 * Platform:    Any
 * WWW:         http://code.google.com/p/freetype-gl/
 * -------------------------------------------------------------------------
 * Copyright 2011, 2012 Nicolas P. Rougier. All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 *  1. Redistributions of source code must retain the above copyright notice,
 *     this list of conditions and the following disclaimer.
 *
 *  2. Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in the
 *     documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY NICOLAS P. ROUGIER ''AS IS'' AND ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO
 * EVENT SHALL NICOLAS P. ROUGIER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
 * INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 * The views and conclusions contained in the software and documentation are
 * those of the authors and should not be interpreted as representing official
 * policies, either expressed or implied, of Nicolas P. Rougier.
 * ========================================================================= */

// edited and reformated for lambdanative use 

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

#include <ft2build.h>
#include FT_FREETYPE_H
#include FT_STROKER_H
// #include FT_ADVANCES_H
#include FT_LCD_FILTER_H

#include <stdint.h>
#include <math.h>
#include <wchar.h>

typedef union {
  float data[4];
  float xyzw[4];
  float rgba[4];
  float rgb[3];
  float xyz[3];
  struct { float x;   float y;     float z;     float w;      };
  struct { float x_;   float y_;   float width; float height; };
  struct { float r;   float g;     float b;     float a;      };
  struct { float red; float green; float blue;  float alpha;  };
} vec4;

typedef union {
  float data[3];
  float rgb[3];
  float xyz[3];
  float xy[2];
  struct { float x;     float y;      float z;     };
  struct { float width; float height; float depth; };
  struct { float r;     float g;      float b;     };
  struct { float red;   float green;  float blue;  };
} vec3;

typedef union {
  float data[2];
  float xy[2];
  struct { float x;         float y;      };
  struct { float width;     float height; };
  struct { float l;         float a;      };
  struct { float luminance; float alpha;  };
} vec2;

typedef union {
  int data[4];
  int xyzw[4];
  int rgba[4];
  int rgb[3];
  int xyz[3];
  struct { int x;   int y;     int z;     int w;      };
  struct { int x_;  int y_;    int width; int height; };
  struct { int r;   int g;     int b;     int a;      };
  struct { int red; int green; int blue;  int alpha;  };
} ivec4;

typedef union {
  int data[3];
  int rgb[3];
  int xyz[3];
  int xy[2];
  struct { int x;     int y;      int z;     };
  struct { int width; int height; int depth; };
  struct { int r;     int g;      int b;     };
  struct { int red;   int green;  int blue;  };
} ivec3;

typedef union {
int data[2];
  int xy[2];
  struct { int x;         int y;      };
  struct { int width;     int height; };
  struct { int l;         int a;      };
  struct { int luminance; int alpha;  };
} ivec2;

typedef struct {
  void * items;
  size_t capacity;
  size_t size;
  size_t item_size;
} vector_t;

// ------------------------------------------------------------- vector

static vector_t * vector_new( size_t item_size )
{
  assert( item_size );
  vector_t *self = (vector_t *) malloc( sizeof(vector_t) );
  if( !self ) {
    fprintf( stderr, "line %d: No more memory for allocating data\n", __LINE__ );
    exit( EXIT_FAILURE );
  }
  self->item_size = item_size;
  self->size      = 0;
  self->capacity  = 1;
  self->items     = malloc( self->item_size * self->capacity );
  return self;
}

static void vector_delete( vector_t *self )
{
  assert( self );
  free( self->items );
  free( self );
}

static const void * vector_get( const vector_t *self, size_t index )
{
  assert( self );
  assert( self->size );
  assert( index  < self->size );
  return self->items + index * self->item_size;
}

static const void * vector_back( const vector_t *self )
{
  assert( self );
  assert( self->size );
  return vector_get( self, self->size-1 );
}

static size_t vector_size( const vector_t *self )
{
  assert( self );
  return self->size;
}

static void vector_reserve( vector_t *self, const size_t size )
{
  assert( self );
  if( self->capacity < size) {
    self->items = realloc( self->items, size * self->item_size );
    self->capacity = size;
  }
}

static void vector_clear( vector_t *self )
{
  assert( self );
  self->size = 0;
}

static void vector_set( vector_t *self, const size_t index, const void *item )
{
  assert( self );
  assert( self->size );
  assert( index  < self->size );
  memcpy( self->items + index * self->item_size, item, self->item_size );
}

static void vector_insert( vector_t *self, const size_t index, const void *item )
{
  assert( self );
  assert( index <= self->size);
  if( self->capacity <= self->size ) {
    vector_reserve(self, 2 * self->capacity );
  }
  if( index < self->size ) {
    memmove( self->items + (index + 1) * self->item_size,
      self->items + (index + 0) * self->item_size,
      (self->size - index)  * self->item_size);
  }
  self->size++;
  vector_set( self, index, item );
}

static void vector_erase_range( vector_t *self, const size_t first, const size_t last )
{
  assert( self );
  assert( first < self->size );
  assert( last  < self->size+1 );
  assert( first < last );
  memmove( self->items + first * self->item_size,
    self->items + last  * self->item_size,
    (self->size - last)   * self->item_size);
  self->size -= (last-first);
}

static void vector_erase( vector_t *self, const size_t index )
{
  assert( self );
  assert( index < self->size );
  vector_erase_range( self, index, index+1 );
}

static void vector_push_back( vector_t *self, const void *item )
{
  vector_insert( self, self->size, item );
}

// ------------------------------------------------------ texture_atlas

typedef struct {
  vector_t * nodes;
  size_t width;
  size_t height;
  size_t depth;
  size_t used;
  unsigned int id;
  unsigned char * data;
} texture_atlas_t;

static texture_atlas_t * texture_atlas_new( const size_t width, const size_t height, const size_t depth )
{
  assert( (depth == 1) || (depth == 3) || (depth == 4) );
  texture_atlas_t *self = (texture_atlas_t *) malloc( sizeof(texture_atlas_t) );
  if( self == NULL) {
        fprintf( stderr, "line %d: No more memory for allocating data\n", __LINE__ );
        exit( EXIT_FAILURE );
  }
  self->nodes = vector_new( sizeof(ivec3) );
  self->used = 0;
  self->width = width;
  self->height = height;
  self->depth = depth;
  self->id = 0;
  ivec3 node = {{0,0,width}};
  vector_push_back( self->nodes, &node );
  self->data = (unsigned char *) calloc( width*height*depth, sizeof(unsigned char) );
  if( self->data == NULL) {
    fprintf( stderr, "line %d: No more memory for allocating data\n", __LINE__ );
    exit( EXIT_FAILURE );
  }
  return self;
}

static void texture_atlas_delete( texture_atlas_t *self )
{
  assert( self );
  vector_delete( self->nodes );
  if( self->data ) { free( self->data ); }
  free( self );
}

static void texture_atlas_set_region( texture_atlas_t * self, const size_t x,
                          const size_t y, const size_t width, const size_t height,
                          const unsigned char * data, const size_t stride )
{
  assert( self );
  assert( x < self->width);
  assert( (x + width) <= self->width);
  assert( y < self->height);
  assert( (y + height) <= self->height);
  size_t i;
  size_t depth = self->depth;
  size_t charsize = sizeof(char);
  for( i=0; i<height; ++i ) {
    memcpy( self->data+((y+i)*self->width + x ) * charsize * depth, 
      data + (i*stride) * charsize, width * charsize * depth  );
  }
}

static int texture_atlas_fit( texture_atlas_t * self, const size_t index, const size_t width, const size_t height )
{
  assert( self );
  ivec3 *node = (ivec3 *) (vector_get( self->nodes, index ));
  int x = node->x, y, width_left = width;
  size_t i = index;
  if ( (x + width) > self->width ) { return -1; }
  y = node->y;
  while( width_left > 0 ) {
    node = (ivec3 *) (vector_get( self->nodes, i ));
    if( node->y > y ) { y = node->y; }
    if( (y + height) > self->height ) { return -1; }
    width_left -= node->z;
    ++i;
  }
  return y;
}

static void texture_atlas_merge( texture_atlas_t * self )
{
  assert( self );
  ivec3 *node, *next;
  size_t i;
  for( i=0; i< self->nodes->size-1; ++i ) {
    node = (ivec3 *) (vector_get( self->nodes, i ));
    next = (ivec3 *) (vector_get( self->nodes, i+1 ));
    if( node->y == next->y ) {
      node->z += next->z;
      vector_erase( self->nodes, i+1 );
      --i;
    }
  }
}

static ivec4 texture_atlas_get_region( texture_atlas_t * self, const size_t width, const size_t height )
{
  assert( self );
  int y, best_height, best_width, best_index;
  ivec3 *node, *prev;
  ivec4 region = {{0,0,width,height}};
  size_t i;
  best_height = INT_MAX;
  best_index  = -1;
  best_width = INT_MAX;
  for( i=0; i<self->nodes->size; ++i ) {
        y = texture_atlas_fit( self, i, width, height );
    if( y >= 0 ) {
      node = (ivec3 *) vector_get( self->nodes, i );
      if( ( (y + height) < best_height ) ||
          ( ((y + height) == best_height) && (node->z < best_width)) ) {
        best_height = y + height;
        best_index = i;
        best_width = node->z;
        region.x = node->x;
        region.y = y;
      }
    }
  }
  if( best_index == -1 ) {
    region.x = -1;
    region.y = -1;
    region.width = 0;
    region.height = 0;
    return region;
  }
  node = (ivec3 *) malloc( sizeof(ivec3) );
  if( node == NULL) {
    fprintf( stderr, "line %d: No more memory for allocating data\n", __LINE__ );
    exit( EXIT_FAILURE );
  }
  node->x = region.x;
  node->y = region.y + height;
  node->z = width;
  vector_insert( self->nodes, best_index, node );
  free( node );
  for(i = best_index+1; i < self->nodes->size; ++i) {
    node = (ivec3 *) vector_get( self->nodes, i );
    prev = (ivec3 *) vector_get( self->nodes, i-1 );
    if (node->x < (prev->x + prev->z) ) {
      int shrink = prev->x + prev->z - node->x;
      node->x += shrink;
      node->z -= shrink;
      if (node->z <= 0) {
        vector_erase( self->nodes, i );
        --i;
      } else {
        break;
      }
    } else {
      break;
    }
  }
  texture_atlas_merge( self );
  self->used += width * height;
  return region;
}

// ------------------------------------------------- texture_font

typedef struct {
  wchar_t charcode;
  float kerning;
} kerning_t;

typedef struct {
  wchar_t charcode;
  unsigned int id;
  size_t width;
  size_t height;
  int offset_x;
  int offset_y;
  float advance_x;
  float advance_y;
  float s0;
  float t0;
  float s1;
  float t1;
  vector_t * kerning;
  int outline_type;
  float outline_thickness;
} texture_glyph_t;

typedef struct {
  vector_t * glyphs;
  texture_atlas_t * atlas;
  char * filename;
  float size;
  int hinting;
  int outline_type;
  float outline_thickness;
  int filtering;
  unsigned char lcd_weights[5];
  float height;
  float linegap;
  float ascender;
  float descender;
  float underline_position;
  float underline_thickness;
} texture_font_t;

#undef __FTERRORS_H__
#define FT_ERRORDEF( e, v, s )  { e, s },
#define FT_ERROR_START_LIST     {
#define FT_ERROR_END_LIST       { 0, 0 } };
const struct {
  int code;
  const char*  message;
} FT_Errors[] =
#include FT_ERRORS_H

static int texture_font_load_face( FT_Library * library, const char * filename, const float size, FT_Face * face )
{
  assert( library );
  assert( filename );
  assert( size );
  size_t hres = 64;
  FT_Error error;
  FT_Matrix matrix = { (int)((1.0/hres) * 0x10000L),
                       (int)((0.0)      * 0x10000L),
                       (int)((0.0)      * 0x10000L),
                       (int)((1.0)      * 0x10000L) };
  error = FT_Init_FreeType( library );
  if( error ) {
    fprintf(stderr, "FT_Error (0x%02x) : %s\n", FT_Errors[error].code, FT_Errors[error].message);
    return 0;
  }
  error = FT_New_Face( *library, filename, 0, face );
  if( error ) {
    fprintf( stderr, "FT_Error (line %d, code 0x%02x) : %s\n", __LINE__, FT_Errors[error].code, FT_Errors[error].message);
    FT_Done_FreeType( *library );
    return 0;
  }
  error = FT_Select_Charmap( *face, FT_ENCODING_UNICODE );
  if( error ) {
    fprintf( stderr, "FT_Error (line %d, code 0x%02x) : %s\n",
             __LINE__, FT_Errors[error].code, FT_Errors[error].message );
    FT_Done_Face( *face );
    FT_Done_FreeType( *library );
    return 0;
  }
  error = FT_Set_Char_Size( *face, (int)(size*64), 0, 72*hres, 72 );
  if( error ) {
    fprintf( stderr, "FT_Error (line %d, code 0x%02x) : %s\n", __LINE__, FT_Errors[error].code, FT_Errors[error].message );
    FT_Done_Face( *face );
    FT_Done_FreeType( *library );
    return 0;
  }
  FT_Set_Transform( *face, &matrix, NULL );
  return 1;
}

static texture_glyph_t * texture_glyph_new( void )
{
  texture_glyph_t *self = (texture_glyph_t *) malloc( sizeof(texture_glyph_t) );
  if( self == NULL) {
    fprintf( stderr, "line %d: No more memory for allocating data\n", __LINE__ );
    exit( EXIT_FAILURE );
  }
  self->id        = 0;
  self->width     = 0;
  self->height    = 0;
  self->outline_type = 0;
  self->outline_thickness = 0.0;
  self->offset_x  = 0;
  self->offset_y  = 0;
  self->advance_x = 0.0;
  self->advance_y = 0.0;
  self->s0        = 0.0;
  self->t0        = 0.0;
  self->s1        = 0.0;
  self->t1        = 0.0;
  self->kerning   = vector_new( sizeof(kerning_t) );
  return self;
}

static void texture_glyph_delete( texture_glyph_t *self )
{
  assert( self );
  vector_delete( self->kerning );
  free( self );
}

static float texture_glyph_get_kerning( const texture_glyph_t * self, const wchar_t charcode )
{
  size_t i;
  assert( self );
  for( i=0; i<vector_size(self->kerning); ++i ) {
    kerning_t * kerning = (kerning_t *) vector_get( self->kerning, i );
    if( kerning->charcode == charcode ) {
      return kerning->kerning;
    }
  }
  return 0;
}

static void texture_font_generate_kerning( texture_font_t *self )
{
  assert( self );
  size_t i, j;
  FT_Library library;
  FT_Face face;
  FT_UInt glyph_index, prev_index;
  texture_glyph_t *glyph, *prev_glyph;
  FT_Vector kerning;
  if( !texture_font_load_face( &library, self->filename, self->size, &face ) ) {
    return;
  }
  for( i=1; i<self->glyphs->size; ++i ) {
    glyph = *(texture_glyph_t **) vector_get( self->glyphs, i );
    glyph_index = FT_Get_Char_Index( face, glyph->charcode );
    vector_clear( glyph->kerning );
    for( j=1; j<self->glyphs->size; ++j ) {
      prev_glyph = *(texture_glyph_t **) vector_get( self->glyphs, j );
      prev_index = FT_Get_Char_Index( face, prev_glyph->charcode );
      FT_Get_Kerning( face, prev_index, glyph_index, FT_KERNING_UNFITTED, &kerning );
      if( kerning.x ) {
        kerning_t k = {prev_glyph->charcode, kerning.x / (float)(64.0f*64.0f)};
        vector_push_back( glyph->kerning, &k );
      }
    }
  }
  FT_Done_Face( face );
  FT_Done_FreeType( library );
}

static texture_glyph_t * texture_font_get_glyph( texture_font_t * self, wchar_t charcode );

static texture_font_t * texture_font_new( texture_atlas_t * atlas, const char * filename, const float size)
{
  assert( filename );
  assert( size );
  texture_font_t *self = (texture_font_t *) malloc( sizeof(texture_font_t) );
  if( self == NULL) {
    fprintf( stderr, "line %d: No more memory for allocating data\n", __LINE__ );
    exit( EXIT_FAILURE );
  }
  self->glyphs = vector_new( sizeof(texture_glyph_t *) );
  self->atlas = atlas;
  self->height = 0;
  self->ascender = 0;
  self->descender = 0;
  self->filename = strdup( filename );
  self->size = size;
  self->outline_type = 0;
  self->outline_thickness = 0.0;
  self->hinting = 1;
  self->filtering = 1;
  // FT_LCD_FILTER_LIGHT   is (0x00, 0x55, 0x56, 0x55, 0x00)
  // FT_LCD_FILTER_DEFAULT is (0x10, 0x40, 0x70, 0x40, 0x10)
  self->lcd_weights[0] = 0x10;
  self->lcd_weights[1] = 0x40;
  self->lcd_weights[2] = 0x70;
  self->lcd_weights[3] = 0x40;
  self->lcd_weights[4] = 0x10;
  FT_Library library;
  FT_Face face;
  if( !texture_font_load_face( &library, self->filename, self->size*100, &face ) ) { return self; }
  self->underline_position = face->underline_position / (float)(64.0f*64.0f) * self->size;
  self->underline_position = round( self->underline_position );
  if( self->underline_position > -2 ) {
      self->underline_position = -2.0;
  }
  self->underline_thickness = face->underline_thickness / (float)(64.0f*64.0f) * self->size;
  self->underline_thickness = round( self->underline_thickness );
  if( self->underline_thickness < 1 ) {
      self->underline_thickness = 1.0;
  }
  FT_Size_Metrics metrics = face->size->metrics; 
  self->ascender = (metrics.ascender >> 6) / 100.0;
  self->descender = (metrics.descender >> 6) / 100.0;
  self->height = (metrics.height >> 6) / 100.0;
  self->linegap = self->height - self->ascender + self->descender;
  FT_Done_Face( face );
  FT_Done_FreeType( library );
  texture_font_get_glyph( self, -1 );
  return self;
}

static void texture_font_delete( texture_font_t *self )
{
  assert( self );
  if( self->filename ) { free( self->filename ); }
  size_t i;
  texture_glyph_t *glyph;
  for( i=0; i<vector_size( self->glyphs ); ++i) {
    glyph = *(texture_glyph_t **) vector_get( self->glyphs, i );
    texture_glyph_delete( glyph);
  }
  vector_delete( self->glyphs );
  free( self );
}

static size_t texture_font_load_glyphs( texture_font_t * self, const wchar_t * charcodes )
{
  assert( self );
  assert( charcodes );
  size_t i, x, y, width, height, depth, w, h;
  FT_Library library;
  FT_Error error;
  FT_Face face;
  FT_Glyph ft_glyph;
  FT_GlyphSlot slot;
  FT_Bitmap ft_bitmap;
  FT_UInt glyph_index;
  texture_glyph_t *glyph;
  ivec4 region;
  size_t missed = 0;
  width  = self->atlas->width;
  height = self->atlas->height;
  depth  = self->atlas->depth;
  region.x=1;
  region.y=1;
  if( !texture_font_load_face( &library, self->filename, self->size, &face ) ) { return wcslen(charcodes); }
  for( i=0; i<wcslen(charcodes); ++i ) {
    glyph_index = FT_Get_Char_Index( face, charcodes[i] );
    FT_Int32 flags = 0;
    if( self->outline_type > 0 ) {
      flags |= FT_LOAD_NO_BITMAP;
    } else {
      flags |= FT_LOAD_RENDER;
    }
    if( !self->hinting ) {
      flags |= FT_LOAD_NO_HINTING | FT_LOAD_NO_AUTOHINT;
    } else {
      flags |= FT_LOAD_FORCE_AUTOHINT;
    }
    if( depth == 3 ) {
      FT_Library_SetLcdFilter( library, FT_LCD_FILTER_LIGHT );
      flags |= FT_LOAD_TARGET_LCD;
      if( self->filtering ) {
//      FT_Library_SetLcdFilterWeights( library, self->lcd_weights );
      }
    }
    error = FT_Load_Glyph( face, glyph_index, flags );
    if( error ) {
      fprintf( stderr, "FT_Error (line %d, code 0x%02x) : %s\n", __LINE__, FT_Errors[error].code, FT_Errors[error].message );
      FT_Done_FreeType( library );
      return wcslen(charcodes)-i;
    }
    int ft_bitmap_width = 0;
    int ft_bitmap_rows = 0;
    int ft_bitmap_pitch = 0;
    int ft_glyph_top = 0;
    int ft_glyph_left = 0;
    if( self->outline_type == 0 ) {
      slot            = face->glyph;
      ft_bitmap       = slot->bitmap;
      ft_bitmap_width = slot->bitmap.width;
      ft_bitmap_rows  = slot->bitmap.rows;
      ft_bitmap_pitch = slot->bitmap.pitch;
      ft_glyph_top    = slot->bitmap_top;
      ft_glyph_left   = slot->bitmap_left;
    } else {
      FT_Stroker stroker;
      error = FT_Stroker_New( library, &stroker );
      if( error ) {
        fprintf(stderr, "FT_Error (0x%02x) : %s\n", FT_Errors[error].code, FT_Errors[error].message);
        return 0;
      }
      FT_Stroker_Set( stroker, (int)(self->outline_thickness *64),
          FT_STROKER_LINECAP_ROUND, FT_STROKER_LINEJOIN_ROUND, 0);
      error = FT_Get_Glyph( face->glyph, &ft_glyph);
      if( error ) {
        fprintf(stderr, "FT_Error (0x%02x) : %s\n", FT_Errors[error].code, FT_Errors[error].message);
        return 0;
      }
      if( self->outline_type == 1 ) {
        error = FT_Glyph_Stroke( &ft_glyph, stroker, 1 );
      } else if ( self->outline_type == 2 ) {
        error = FT_Glyph_StrokeBorder( &ft_glyph, stroker, 0, 1 );
      } else if ( self->outline_type == 3 ) {
        error = FT_Glyph_StrokeBorder( &ft_glyph, stroker, 1, 1 );
      } if( error ) {
        fprintf(stderr, "FT_Error (0x%02x) : %s\n", FT_Errors[error].code, FT_Errors[error].message);
        return 0;
      }
      if( depth == 1) {
        error = FT_Glyph_To_Bitmap( &ft_glyph, FT_RENDER_MODE_NORMAL, 0, 1);
        if( error ) {
          fprintf(stderr, "FT_Error (0x%02x) : %s\n", FT_Errors[error].code, FT_Errors[error].message);
          return 0;
        }
      } else {
        error = FT_Glyph_To_Bitmap( &ft_glyph, FT_RENDER_MODE_LCD, 0, 1);
        if( error ) {
          fprintf(stderr, "FT_Error (0x%02x) : %s\n", FT_Errors[error].code, FT_Errors[error].message);
          return 0;
        }
      }
      FT_BitmapGlyph ft_bitmap_glyph = (FT_BitmapGlyph) ft_glyph;
      ft_bitmap       = ft_bitmap_glyph->bitmap;
      ft_bitmap_width = ft_bitmap.width;
      ft_bitmap_rows  = ft_bitmap.rows;
      ft_bitmap_pitch = ft_bitmap.pitch;
      ft_glyph_top    = ft_bitmap_glyph->top;
      ft_glyph_left   = ft_bitmap_glyph->left;
      FT_Stroker_Done(stroker);
    }
    w = ft_bitmap_width/depth + 1;
    h = ft_bitmap_rows + 1;
    region = texture_atlas_get_region( self->atlas, w, h );
    if ( region.x < 0 ) { missed++; continue; }
    w = w - 1;
    h = h - 1;
    x = region.x;
    y = region.y;
    if (charcodes[i]!=13) {
      texture_atlas_set_region( self->atlas, x, y, w, h, ft_bitmap.buffer, ft_bitmap.pitch );
    }
    glyph = texture_glyph_new( );
    glyph->charcode = charcodes[i];
    glyph->width    = w;
    glyph->height   = h;
    glyph->outline_type = self->outline_type;
    glyph->outline_thickness = self->outline_thickness;
    glyph->offset_x = ft_glyph_left;
    glyph->offset_y = ft_glyph_top;
    glyph->s0       = x/(float)width;
    glyph->t0       = y/(float)height;
    glyph->s1       = (x + glyph->width)/(float)width;
    glyph->t1       = (y + glyph->height)/(float)height;
    FT_Load_Glyph( face, glyph_index, FT_LOAD_RENDER | FT_LOAD_NO_HINTING);
    slot = face->glyph;
    glyph->advance_x = slot->advance.x/64.0;
    glyph->advance_y = slot->advance.y/64.0;
    vector_push_back( self->glyphs, &glyph );
  }
  if( self->outline_type > 0 ) { FT_Done_Glyph( ft_glyph ); }
  FT_Done_Face( face );
  FT_Done_FreeType( library );
  texture_font_generate_kerning( self );
  return missed;
}

static texture_glyph_t * texture_font_get_glyph( texture_font_t * self, wchar_t charcode )
{
  assert( self );
  size_t i;
  static wchar_t *buffer = 0;
  texture_glyph_t *glyph;
  assert( self );
  assert( self->filename );
  assert( self->atlas );
  for( i=0; i<self->glyphs->size; ++i ) {
    glyph = *(texture_glyph_t **) vector_get( self->glyphs, i );
    if( (glyph->charcode == charcode) &&
      (glyph->outline_type == self->outline_type) &&
      (glyph->outline_thickness == self->outline_thickness) ) {
      return glyph;
    }
  }
  if( charcode == (wchar_t)(-1) ) {
    size_t width  = self->atlas->width;
    size_t height = self->atlas->height;
    ivec4 region = texture_atlas_get_region( self->atlas, 5, 5 );
    texture_glyph_t * glyph = texture_glyph_new( );
    static unsigned char data[4*4*3] = {-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
                                        -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
                                        -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
                                        -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1};
    if ( region.x < 0 ) { return NULL; }
    texture_atlas_set_region( self->atlas, region.x, region.y, 4, 4, data, 0 );
    glyph->charcode = (wchar_t)(-1);
    glyph->s0 = (region.x+2)/(float)width;
    glyph->t0 = (region.y+2)/(float)height;
    glyph->s1 = (region.x+3)/(float)width;
    glyph->t1 = (region.y+3)/(float)height;
    vector_push_back( self->glyphs, &glyph );
    return glyph; //*(texture_glyph_t **) vector_back( self->glyphs );
  }
  if( !buffer) {
    buffer = (wchar_t *) calloc( 2, sizeof(wchar_t) );
  }
  buffer[0] = charcode;
  if( texture_font_load_glyphs( self, buffer ) == 0 ) {
    return *(texture_glyph_t **) vector_back( self->glyphs );
  }
  return NULL;
}

// end of Freetype GL
// =========================================================================

// generate lambdanative texture mapped font from truetype

static wchar_t *glyph_set=0;
static texture_atlas_t * atlas = 0;
static texture_font_t * font = 0;

int fastlz_compress(const void*, int, void*);

static int clen;

static void ln_atlas_compile_compressedtexture(unsigned char *data, int len)
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

static void ln_atlas_compile_atlas(char *name, texture_atlas_t *a)
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
  ln_atlas_compile_compressedtexture(data,depth*w*h);
  printf("};\nend-of-c-declare\n)\n");
  printf("(define %s.z  (let ((u8v (make-u8vector %i))) ((c-lambda (scheme-object int) void \"memcpy(___CAST(void*,___BODY_AS(___arg1,___tSUBTYPED)),%s_font,___arg2);\") u8v %i) u8v))\n", name, clen+1, sane_name, clen+1);
  printf("(define %s.raw (glCoreTextureCreate %i %i (u8vector-decompress %s.z) GL_LINEAR GL_REPEAT))\n",name, w ,h, name );
}

static void ln_atlas_compile_font(char *tag, texture_font_t *font, int w, int h)
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
      printf("%f %f %f %f)", glyph->s0, glyph->t1+.5/(double)h, glyph->s1+.5/(double)w, glyph->t0 );
      printf(" %i %f %i", (int)glyph->offset_x, glyph->advance_x, (int)glyph->offset_y);
      printf(")\n");
    }
  }
}

static size_t ln_atlas_try(size_t w, size_t h, char *fname, int *pointlist,char *compiletag)
{
  int ptidx=0;
  size_t i, missed=0;
  if (strlen(compiletag)&&atlas) ln_atlas_compile_atlas(compiletag,atlas);
  if (atlas) { texture_atlas_delete(atlas); atlas=0; }
  atlas=texture_atlas_new( w, h, 1 );
  while (1) {
    char buf[128];
    i = pointlist[ptidx++];
    if (i==0) break;
    if (font) { texture_font_delete(font); font=0; }
    font = texture_font_new( atlas, fname, i );
    if (strlen(compiletag)) {
      sprintf(buf,"%s_%i.fnt", compiletag, (int)i);
      printf("(define %s (list\n", buf);
      sprintf(buf,"%s.raw", compiletag);
    }
    missed += texture_font_load_glyphs( font, glyph_set);
    if (strlen(compiletag)) ln_atlas_compile_font(compiletag,font,w,h);
    if (strlen(compiletag)) printf("))\n");
  }
  return missed;
}

static int ln_atlas_make(char *fname, int *pointlist, int *glyphlist, char *compiletag)
{
  int i,j;
  int nglyphs=0;
  while (glyphlist[nglyphs]) nglyphs++;
  if (nglyphs>=0) {
    glyph_set=(wchar_t*)malloc((nglyphs+1)*sizeof(wchar_t));
    for (i=0;i<nglyphs;i++) glyph_set[i]=(wchar_t)glyphlist[i];
    glyph_set[nglyphs]=0;
  }
  size_t sizes[]={64,128,256,512,1024,2048,4096};
  for (j=0;j<5;j++) {
    for (i=0;i<5;i++) {
     if (ln_atlas_try(sizes[i],sizes[j],fname,pointlist,"")==0) goto success;
    }
  }
  fprintf(stderr, "Warning: texture size > 1024x1024, may not work on all platforms!\n");
  for (j=4;j<7;j++) {
    for (i=4;i<7;i++) {
      if (ln_atlas_try(sizes[i],sizes[j],fname,pointlist,"")==0) goto success;
    }
  }
  fprintf(stderr, "texture capacity exceeded: %dx%d is too small\n",sizes[j-1],sizes[i-1]);
  return 1; // texture capacity exceeded
success:
  if (strlen(compiletag)) ln_atlas_try(sizes[i],sizes[j],fname,pointlist, compiletag);
  fflush(stdout);
  return 0;
}

static void ln_atlas_free()
{
  if (atlas) { texture_atlas_delete(atlas); atlas=0; }
  if (font) { texture_font_delete(font); font=0; }
  if (glyph_set) { free(glyph_set); glyph_set=0; }
}

static int ln_atlas_info(int idx) 
{
  int res=0;
  if (atlas) {
    switch (idx) {
      case 1: res=atlas->width; break;
      case 2: res=atlas->height; break;
      case 3: res = atlas->depth*atlas->width*atlas->height; break;
    }
  }
  return res;
} 

static void ln_atlas_data(unsigned char *data)
{
  if (atlas) {
    memcpy(data,atlas->data,atlas->depth*atlas->width*atlas->height);
  }
}

static int ln_glyph_fxinfo(int gidx, int idx)
{
  int res=0;
  if (font) {
    texture_glyph_t *glyph = *(texture_glyph_t **) vector_get( font->glyphs, gidx);
    switch (idx) {
      case 1: res = (int)glyph->charcode; break;
      case 2: res = (int)glyph->width; break;
      case 3: res = (int)glyph->height; break;
      case 4: res = (int)glyph->offset_x; break;
      case 5: res = (int)glyph->offset_y; break;
    }
  } 
  return res;
}

static double ln_glyph_flinfo(int gidx, int idx)
{
  double res=0;
  if (font&&atlas) {
    texture_glyph_t *glyph = *(texture_glyph_t **) vector_get( font->glyphs, gidx);
    switch (idx) { 
      case 1: res=(double)glyph->s0; break;
      case 2: res=(double)glyph->t1+.5/(double)atlas->height; break;
      case 3: res=(double)glyph->s1+.5/(double)atlas->width; break;
      case 4: res=(double)glyph->t0; break;
      case 5: res=(double)glyph->advance_x; break; 
    }
  }
  return res;
}

static int ln_font_height()
{
  int res=0;
  if (font) {
    res = (int)ceil(font->ascender-font->descender);
  }
  return res;
}

#include FT_SFNT_NAMES_H

static char *ln_font_name(char *fname)
{ 
  FT_Error error;
  FT_Library library=0;
  FT_Face    face=0;
  FT_SfntName aname;
  aname.string=0;
  static char result[256];
  error = FT_Init_FreeType( &library );
  if ( error ) goto bail_name;
  error = FT_New_Face( library, fname, 0, &face );
  if ( error ) goto bail_name;
  error = FT_Get_Sfnt_Name(face, 1, &aname );
  if ( error ) goto bail_name;
 bail_name:
  memset(result,0,256);
  if (aname.string) {
    int i,len = aname.string_len+1;
    if (len>256) len=256;
    memcpy(result,aname.string,len-1);
    if (len>2&&result[0]==0) {
      for (i=0;i<len/2;i++) { result[i]=result[2*i+1]; }
      result[len>>1]=0;
    }
  }
  if (face) FT_Done_Face(face);
  if (library) FT_Done_FreeType(library);
  return result;
}

end-of-c-declare
)

(define ttf:ascii (let loop ((g 32)(res '())) (if (= g 127) res (loop (+ g 1) (append res (list g))))))
(define ttf:numerals (let loop ((g 48)(res '())) (if (= g 58) res (loop (+ g 1) (append res (list g))))))

(define ttf:atlas-make
   (c-lambda (char-string scheme-object scheme-object char-string) int
     "___result=ln_atlas_make(___arg1, ___CAST(void*,___BODY_AS(___arg2,___tSUBTYPED)), 
                              ___CAST(void*,___BODY_AS(___arg3,___tSUBTYPED)), ___arg4);"))
(define ttf:atlas-free (c-lambda () void "ln_atlas_free"))
(define ttf:atlas-info (c-lambda (int) int "ln_atlas_info"))
(define ttf:atlas-data (c-lambda (scheme-object) void "ln_atlas_data(___CAST(void*,___BODY_AS(___arg1,___tSUBTYPED)));"))
(define ttf:glyph-fxinfo (c-lambda (int int) int "ln_glyph_fxinfo"))
(define ttf:glyph-flinfo (c-lambda (int int) double "ln_glyph_flinfo"))
(define ttf:font-height (c-lambda () int "ln_font_height"))

(define ttf-name (c-lambda (char-string) char-string "ln_font_name"))

;; outputs a scheme font file to stdout for compilation
(define (ttf-compile fname pointlist glyphlist name)
  (ttf:log 1 "ttf-compile " name " " fname " " pointlist " " glyphlist)
  (let* ((pointvector (s32vector-append (list->s32vector pointlist) (s32vector 0)))
         (glyphvector (s32vector-append (list->s32vector glyphlist) (s32vector 0)))
         (atlas (fx= (ttf:atlas-make fname pointvector glyphvector name) 0)))
    (ttf:atlas-free) 
    atlas))


(define ttf:dyncache (make-table))

;; returns a scheme font for runtime use
(define (ttf->fnt fname pointsize . glyphlist)
  (ttf:log 1 "ttf->fnt" fname " " pointsize " " glyphlist)
  (let* ((fntid  (equal?-hash (list fname pointsize glyphlist)))
         (cachedfnt (table-ref ttf:dyncache fntid #f)))
    (if cachedfnt cachedfnt 
      (let* ((glyphs (cond
               ((null? glyphlist) ttf:ascii)
               ((string? (car glyphlist)) (utf8string->unicode (car glyphlist)))
               (else (car glyphlist))))
             (glyphvector (s32vector-append (list->s32vector glyphs) (s32vector 0)))
             (atlas (fx= (ttf:atlas-make fname (s32vector pointsize 0) glyphvector "") 0))
             (atlas-width (if atlas (ttf:atlas-info 1) #f))
             (atlas-height (if atlas (ttf:atlas-info 2) #f))
             (atlas-datalen (if atlas (ttf:atlas-info 3) #f))
             (atlas-data (if atlas (let ((v (make-u8vector atlas-datalen))) (ttf:atlas-data v) v) #f))
             (atlas-texture (if atlas ((eval 'glCoreTextureCreate) atlas-width atlas-height atlas-data) #f)) 
             (fnt (if atlas-texture
               (let loop ((n 1)(gs glyphs)(fnt '()))
                 (if (fx= (length gs) 0) (let ((fnl (append 
                     (list (list 0 (list 0 (ttf:font-height) atlas-texture 0. 0. 0. 0.) 0 0 0)) fnt)))
                     (ttf:atlas-free) fnl)
                   (loop (fx+ n 1) (cdr gs) (append fnt (list 
                     (list (ttf:glyph-fxinfo n 1) 
                         (list (ttf:glyph-fxinfo n 2) (ttf:glyph-fxinfo n 3) atlas-texture
                             (ttf:glyph-flinfo n 1) (ttf:glyph-flinfo n 2) (ttf:glyph-flinfo n 3) (ttf:glyph-flinfo n 4))
                       (ttf:glyph-fxinfo n 4) (ttf:glyph-flinfo n 5) (ttf:glyph-fxinfo n 5)))))))
               #f)))
         (if fnt (table-set! ttf:dyncache fntid fnt))
         fnt))))

;; eof
