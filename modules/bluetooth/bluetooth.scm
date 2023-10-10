
(c-declare #<<end-of-c-declare

#ifdef ANDROID
  void android_btle_startscan(void);
  void android_btle_stopscan(void);
  char* android_get_scanresults(void);
#endif

void btle_startscan(){
#ifdef ANDROID
  return android_btle_startscan();
#else
  return;
#endif
}

void btle_stopscan(){
#ifdef ANDROID
  return android_btle_stopscan();
#else
  return;
#endif
}

char* btle_get_scanresults(void){
#ifdef ANDROID
  return android_get_scanresults();
#else
  return "[]";
#endif
}

end-of-c-declare
)

(define btle-startscan (c-lambda () void "btle_startscan"))
(define btle-stopscan (c-lambda () void "btle_stopscan"))
(define btle-get-scanresult-string (c-lambda () nonnull-char-string "btle_get_scanresults"))

(define (btle-get-scanresults)
  (json-decode (btle-get-scanresult-string)))
