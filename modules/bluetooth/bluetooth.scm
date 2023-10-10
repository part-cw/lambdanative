
(c-declare #<<end-of-c-declare

typedef struct ScanResult {
  char* mac_address;
  int rssi;
} ScanResult;

___SCMOBJ scheme_release_sr(void* sr);

___SCMOBJ scheme_release_sr(void* sr){
  ScanResult* result = (ScanResult*)sr;
  free(result->mac_address);
  return ___FIX(___NO_ERR);
}

typedef struct DemoStruct {
  int x;
  int y;
} DemoStruct;

#ifdef ANDROID
  void android_btle_startscan(void);
  void android_btle_stopscan(void);
  ScanResult* android_get_scanresults(void);
  int android_get_numresults(void);
  ScanResult btle_scanresults_ref(ScanResult* results, int i);
  char* btle_get_macaddress(ScanResult result);
  int btle_get_rssi(ScanResult result);
  DemoStruct* getDemoStruct(void);
  int getDemoX(DemoStruct* demo);
  DemoStruct** getDemoList(void);
  int getDemoXAt(DemoStruct** ds, int i);
  char* getDemoSAt(DemoStruct** ds, int i);
  void scheme_release_ds(void* ds);
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

ScanResult* btle_get_scanresults(void){
#ifdef ANDROID
  return android_get_scanresults();
#else
  return (ScanResult*)NULL;
#endif
}

int btle_get_numresults(void){
#ifdef ANDROID
  return android_get_numresults();
#else
  return 0;
#endif
}

end-of-c-declare
)

(c-define-type ScanResult (struct "ScanResult"))
(c-define-type DemoStruct (pointer (struct "DemoStruct" #f "scheme_release_ds")))

(define btle-startscan (c-lambda () void "btle_startscan"))
(define btle-stopscan (c-lambda () void "btle_stopscan"))
(define btle-get-scanresults (c-lambda () (pointer ScanResult) "btle_get_scanresults"))
(define btle-get-numresults (c-lambda () int "btle_get_numresults"))
(define btle-scanresults-ref (c-lambda ((pointer ScanResult) int) ScanResult "btle_scanresults_ref"))
(define btle-get-macaddress (c-lambda (ScanResult) char-string "btle_get_macaddress"))
(define btle-get-rssi (c-lambda (ScanResult) int8 "btle_get_rssi"))  

(define get-demo-struct (c-lambda () DemoStruct "getDemoStruct"))
(define get-demo-x (c-lambda (DemoStruct) int "getDemoX"))
(define get-demo-list (c-lambda () (pointer DemoStruct) "getDemoList"))
(define get-demo-x-at (c-lambda ((pointer DemoStruct) int) int "getDemoXAt"))
(define get-demo-s-at (c-lambda ((pointer DemoStruct) int) char-string "getDemoSAt"))


