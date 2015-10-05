#|
LambdaNative - a cross-platform Scheme framework
Copyright (c) 2009-2015, University of British Columbia
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

;; minimal interface to libxml2

(##namespace ("xml#"))
(##include "~~lib/gambit#.scm")
(##include "xml#.scm")

(##namespace ("" string->u8vector))

(c-declare #<<c-declare-end

#include <stdio.h>

#include <libxml/tree.h>
#include <libxml/parser.h>

c-declare-end
)

(c-define-type xmlDoc (pointer "xmlDoc"))
(c-define-type xmlNode (pointer "xmlNode"))
(c-define-type xmlAttribute (pointer "xmlAttribute"))

(define xmlParseFile (c-lambda (char-string) xmlDoc "xmlParseFile"))
(define (xmlParseMemory u8v) ((c-lambda (scheme-object int) xmlDoc 
  "___result=xmlParseMemory(___CAST(void*,___BODY_AS(___arg1,___tSUBTYPED)),___arg2);")
    u8v (u8vector-length u8v)))
(define xmlDocGetRootElement (c-lambda (xmlDoc) xmlNode "xmlDocGetRootElement"))
(define xmlNodeListGetString (c-lambda (xmlDoc xmlNode int) char-string "xmlNodeListGetString"))
(define xmlFreeDoc (c-lambda (xmlDoc) void "xmlFreeDoc"))

(define-macro (xml name entry type)
  `(define ,(string->symbol (string-append "xml" (symbol->string name)  "->" (symbol->string entry)))
     (c-lambda (,(string->symbol (string-append "xml" (symbol->string name)))) ,type 
        ,(string-append "___result=___arg1->" (symbol->string entry) ";"))))
(xml Node children xmlNode)
(xml Node next xmlNode)
(xml Node name char-string)
(xml Node content char-string)
(xml Node properties xmlAttribute)
(xml Node type int)
(xml Node doc xmlDoc)
(xml Attribute name char-string)
(xml Attribute children xmlNode)
(xml Attribute next xmlAttribute)

(define XML_ELEMENT_NODE ((c-lambda () int "___result=XML_ELEMENT_NODE;")))
(define XML_TEXT_NODE ((c-lambda () int "___result=XML_TEXT_NODE;")))
(define (xml-element? n) (fx= (xmlNode->type n) XML_ELEMENT_NODE))
(define (xml-text? n) (fx= (xmlNode->type n) XML_TEXT_NODE))

;; decode the xml data structure

(define (node-attrs n)
  (let loop ((a (xmlNode->properties n))(res '(@)))
    (if (not a) res
      (loop (xmlAttribute->next a) (append res (list (list 
        (string->symbol (xmlAttribute->name a)) (xmlNodeListGetString (xmlNode->doc n) (xmlAttribute->children a) 1))))))))

(define (node-list _n)
  (let loop ((n _n)(res '()))
    (if (not n) res
      (loop (xmlNode->next n) (append res
        (if (xml-element? n) (list n) 
          (if (xml-text? n) (list (xmlNode->content n)) '())))))))

(define (sxml name attrs children)
  (append (list (string->symbol name)) (if (= (length attrs) 1) '()  (list attrs)) children))

(define (node->sxml n)
  (map (lambda (n) 
    (if (string? n) n
      (sxml (xmlNode->name n)
            (node-attrs n)
            (node->sxml (xmlNode->children n)))))
        (node-list n)))

(define (xmlfile->sxml fname)
  (if (file-exists? fname)
    (let* ((d (xmlParseFile fname))
           (r (if d (xmlDocGetRootElement d) #f))
           (res (if r (car (node->sxml r)) #f)))
      (if d (xmlFreeDoc d))
      res
    ) #f))

(define (xml->sxml data)
  (let* ((u8v (if (string? data) (string->u8vector data) data))
         (d (xmlParseMemory u8v))
         (r (if d (xmlDocGetRootElement d) #f))
         (res (if r (car (node->sxml r)) #f)))
    (if d (xmlFreeDoc d))
    res))

;; eof
