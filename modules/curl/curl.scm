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

;; minimal curl wrapper

(c-declare  #<<end-of-c-declare

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <curl/curl.h>
#include <oauth.h>

int curl_needsinit=1;

// ---------------
// payload handler

static int payload_len=0;
static char *payload_data=0;

static void curl_payload_reset()
{
  if (payload_data) { free(payload_data); payload_data=0; payload_len=0; }
}

static size_t curl_payload_callback(char *ptr, size_t size, size_t nmemb, void *userdata)
{
  int i,n=size*nmemb;
  payload_data=realloc(payload_data,payload_len+n+1); 
  memcpy(&payload_data[payload_len],ptr,n);
  payload_len+=n;
  payload_data[payload_len]=0;
  return n;
}

// ---------------
// GET wrapper

static int curl_GET(char *url, char *header)
{
  if (curl_needsinit) {
    curl_global_init(CURL_GLOBAL_ALL);
    curl_needsinit=0;
  }
  curl_payload_reset();
  struct curl_slist *slist=NULL;
  slist = curl_slist_append(slist,header);
  CURL *curl = curl_easy_init();
  curl_easy_setopt(curl, CURLOPT_URL, url);
  curl_easy_setopt(curl, CURLOPT_USERAGENT, "user-string");
  curl_easy_setopt(curl, CURLOPT_HTTPHEADER, slist);
  curl_easy_setopt(curl, CURLOPT_SSL_VERIFYPEER, 0);
  curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, curl_payload_callback);
  int curlstatus = curl_easy_perform(curl);
  curl_easy_cleanup(curl);
  curl_slist_free_all(slist);
  return curlstatus;
}

// ---------------
// POST wrapper

static int curl_POST(char *url, char *header)
{
  int i;
  if (curl_needsinit) {
    curl_global_init(CURL_GLOBAL_ALL);
    curl_needsinit=0;
  }
  curl_payload_reset();
  int l = strlen(url);
  for (i=0;i<l;i++) { if (url[i]=='?') break; } url[i]=0;
  char *post_params = (i<l-1?(char *)&url[i+1]:0);
  struct curl_slist *slist=NULL;
  slist = curl_slist_append(slist,header);
  CURL *curl = curl_easy_init();
  curl_easy_setopt(curl, CURLOPT_URL, url);
  curl_easy_setopt(curl, CURLOPT_USERAGENT, "user-string");
  curl_easy_setopt(curl, CURLOPT_HTTPHEADER, slist);
  curl_easy_setopt(curl, CURLOPT_POST, 1);
  curl_easy_setopt(curl, CURLOPT_POSTFIELDS, post_params);
  curl_easy_setopt(curl, CURLOPT_SSL_VERIFYPEER, 0);
  curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, curl_payload_callback);
  int curlstatus = curl_easy_perform(curl);
  curl_easy_cleanup(curl);
  curl_slist_free_all(slist);
  return curlstatus;
}

end-of-c-declare
)

(define curl-GET (c-lambda (char-string char-string) int "curl_GET"))
(define curl-POST (c-lambda (char-string char-string) int "curl_POST"))
(define curl-data (c-lambda () char-string "___result=payload_data;"))

;; eof
