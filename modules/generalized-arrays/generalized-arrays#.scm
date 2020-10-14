#|
Copyright 2020 Bradley J Lucier.
All Rights Reserved.

Permission is hereby granted, free of charge,
to any person obtaining a copy of this software
and associated documentation files (the "Software"),
to deal in the Software without restriction,
including without limitation the rights to use, copy,
modify, merge, publish, distribute, sublicense,
and/or sell copies of the Software, and to permit
persons to whom the Software is furnished to do so,
subject to the following conditions:

The above copyright notice and this permission notice
(including the next paragraph) shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF
ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT
LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO
EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE
FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN
AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
OTHER DEALINGS IN THE SOFTWARE.
|#

(##namespace
 (""
  ;; Miscellaneous Functions
  translation? permutation?
  
  ;; Intervals
  make-interval interval?
  interval-dimension
  interval-lower-bound interval-upper-bound
  interval-lower-bounds->list interval-upper-bounds->list
  interval-lower-bounds->vector interval-upper-bounds->vector
  interval= interval-volume interval-subset? interval-contains-multi-index?
  interval-projections
  interval-for-each
  interval-dilate interval-intersect interval-translate interval-permute
  interval-rotate interval-scale interval-cartesian-product
  
  ;; Storage Classes
  make-storage-class storage-class?
  storage-class-getter storage-class-setter
  storage-class-checker storage-class-maker storage-class-copier
  storage-class-length storage-class-default
  generic-storage-class
  s8-storage-class s16-storage-class s32-storage-class s64-storage-class
  u1-storage-class
  u8-storage-class u16-storage-class u32-storage-class u64-storage-class
  f8-storage-class f16-storage-class f32-storage-class f64-storage-class
  c64-storage-class c128-storage-class
  
  ;; Arrays
  make-array array?
  array-domain array-getter array-dimension
  mutable-array? array-setter
  specialized-array-default-safe? specialized-array-default-mutable?
  make-specialized-array specialized-array?
  array-storage-class array-indexer array-body array-safe?
  array-elements-in-order?
  specialized-array-share
  array-copy
  array-curry array-extract array-tile array-translate array-permute
  array-rotate array-reverse array-sample
  array-outer-product
  array-map array-for-each
  array-fold array-fold-right array-reduce
  array-any array-every
  array->list list->array
  array-assign!
  specialized-array-reshape
  array-ref array-set!
  ))
