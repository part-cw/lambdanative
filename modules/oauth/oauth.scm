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

;; minimal oauth wrapper

(c-declare  #<<end-of-c-declare

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include <oauth.h>

static char *oauth_POST_header(char *url, char *consumer_key, char *consumer_secret, char *user_token, char *user_secret)
{
  int i, argc;
  char **argv;
  static char *auth_header=0;
  if (auth_header) free(auth_header);
  argv=malloc(0);
  argc=oauth_split_url_parameters(url,&argv);
  char *tmp_url = oauth_sign_array2(&argc, &argv, NULL, OA_HMAC, "POST",
    consumer_key, consumer_secret, (strlen(user_token)>0?user_token:0), (strlen(user_secret)>0?user_secret:0));
  char *auth_params = oauth_serialize_url_sep(argc, 1, argv, ", ", 6);
  auth_header = malloc(strlen(auth_params)+24);
  sprintf(auth_header, "Authorization: OAuth %s", auth_params);
  for (i = 0; i < argc; i++ ) { free(argv[i]); } free(argv);
  free(tmp_url);
  free(auth_params);
  return auth_header;
}

static char *oauth_GET_signedurl(char *url,char *consumer_key, char *consumer_secret, char *user_token, char *user_secret)
{
  static char *signedurl=0;
  if (signedurl) free(signedurl);
  signedurl = oauth_sign_url2(url, NULL, OA_HMAC, "GET", consumer_key, consumer_secret, (strlen(user_token)>0?user_token:0), (strlen(user_secret)>0?user_secret:0));
  return signedurl;
}

end-of-c-declare
)

(define oauth-POST-signedheader (c-lambda (char-string char-string char-string char-string char-string) char-string "oauth_POST_header"))
(define oauth-GET-signedurl (c-lambda (char-string char-string char-string char-string char-string) char-string "oauth_GET_signedurl"))

;; eof
