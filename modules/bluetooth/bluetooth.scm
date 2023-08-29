
(c-declare #<<end-of-c-declare

#ifdef ANDROID
  void android_btle_startscan(void);
  char** android_btle_scanresults(void);
#endif

char** btle_scanresults(){
#ifdef ANDROID
  return android_btle_scanresults();
#else
  char** result = (char**)malloc(sizeof(char*));
  result[0] = NULL;
  return result;
#endif
}

void btle_startscan(){
#ifdef ANDROID
  return android_btle_startscan();
#else
  return;
#endif
}

end-of-c-declare
)

(define btle-startscan (c-lambda () void "btle_startscan"))
(define btle-scanresults (c-lambda () nonnull-char-string-list "btle_scanresults"))
