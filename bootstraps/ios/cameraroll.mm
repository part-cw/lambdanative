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
// access to last image in camera roll

#import "config.h"

#ifdef USE_CAMERAROLL

#import <UIKit/UIKit.h>
#import <AssetsLibrary/AssetsLibrary.h>

UIImage *lastphoto=0;
static ALAssetsLibrary *library = 0;

unsigned char *lastphoto_data=0;
int lastphoto_w=0, lastphoto_h=0;

extern "C" void iphone_cameraroll_update() {
    if (!library) { library = [[ALAssetsLibrary alloc] init]; }
    [library enumerateGroupsWithTypes:ALAssetsGroupSavedPhotos usingBlock:^(ALAssetsGroup *group, BOOL *stop) {
        [group setAssetsFilter:[ALAssetsFilter allPhotos]];
        [group enumerateAssetsAtIndexes:[NSIndexSet indexSetWithIndex:[group numberOfAssets]-1] options:0 usingBlock:^(ALAsset *alAsset, NSUInteger index, BOOL *innerStop) { 
            if (alAsset) {
              ALAssetRepresentation *representation = [alAsset defaultRepresentation];
             // lastphoto = [UIImage imageWithCGImage:[representation fullResolutionImage]];
              lastphoto = [UIImage imageWithCGImage:[representation fullResolutionImage] scale:1.0 orientation:UIImageOrientationUp];
              if (lastphoto) {
                CGImageRef inImage= lastphoto.CGImage;
                int img_w= CGImageGetWidth(inImage);
                int img_h= CGImageGetHeight(inImage);
                lastphoto_w = img_w;
                lastphoto_h = img_h;
                CGColorSpaceRef colorSpace = CGColorSpaceCreateDeviceRGB();
                if (lastphoto_data) { free(lastphoto_data); lastphoto_data=0; }
                lastphoto_data=(unsigned char *)malloc(img_w*img_h*4);
                memset(lastphoto_data,128, img_w*img_h*4);
                if (lastphoto_data) {
               //   CGContextRef context=CGBitmapContextCreate(lastphoto_data, 
               //      img_w, img_h, 8, img_w*4, colorSpace , kCGImageAlphaLast);
                  // this was painful to get right! yikes
                  CGContextRef context=CGBitmapContextCreate(lastphoto_data, 
                     img_w, img_h, 8, img_w*4, colorSpace , kCGImageAlphaNoneSkipLast);
                  if (context) {
                    CGContextDrawImage(context,CGRectMake(0,0,img_w,img_h), inImage);
                    CGContextRelease(context);
                  }
                }
                CGColorSpaceRelease(colorSpace);
              }
            }
       }];
    }
    failureBlock: ^(NSError *error) { lastphoto=0; 
    }];
}

// return width of image
extern "C" unsigned int iphone_cameraroll_width()
{
  return (lastphoto?lastphoto_w:0);
}

// return height of image
extern "C" unsigned int iphone_cameraroll_height()
{
  return (lastphoto?lastphoto_h:0);
}

// transfer and scale pixel data
extern "C" void iphone_cameraroll_rgb(unsigned char *data,int w, int h)
{
  if (lastphoto_data) { 
    int i,j;
    int img_w = lastphoto_w;
    int img_h = lastphoto_h;
    for (i=0;i<w;i++) {
      for (j=0;j<h;j++) {
      //  int img_i = (i*img_w)/w;
      //  int img_j = (j*img_h)/h;
        // image appears rotated 90 degrees??
        int img_i = (j*img_w)/h;
        int img_j = (i*img_h)/w;
        int img_ofs = (img_j*img_w + img_i)*4;
        int ofs = ((h-j-1)*w+(w-i-1))*3;
        data[ofs] =  lastphoto_data[img_ofs];
        data[ofs+1] = lastphoto_data[img_ofs+1];
        data[ofs+2] = lastphoto_data[img_ofs+2];
      }
    }
  }
}

#endif // USE_CAMERAROLL

// eof
