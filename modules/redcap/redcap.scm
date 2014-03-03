#|
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
|#

;; REDCap module - Allows data export-/import to an Research Electronic Data Capture server

;; intermediate data handling 

(define redcap:datachunk 10000)
(define redcap:data (u8vector))
(define redcap:datalen 0)

(define (redcap:data-clear!)
  (set! redcap:datalen 0)
  (u8vector-shrink! redcap:data 0))

(define (redcap:data->string)
  (let ((str (u8vector->string (subu8vector redcap:data 0 redcap:datalen))))
    (redcap:data-clear!)
    str))

(define (redcap:data->u8vector)
  (let ((v (subu8vector redcap:data 0 redcap:datalen)))
    (redcap:data-clear!)
    v))

(define (redcap:data-append! v)
  (let ((lv (u8vector-length v))
        (la (u8vector-length redcap:data)))
    (if (> (+ redcap:datalen lv) la)
      (set! redcap:data (u8vector-append redcap:data (make-u8vector (max lv redcap:datachunk)))))
    (subu8vector-move! v 0 lv redcap:data redcap:datalen)
    (set! redcap:datalen (+ redcap:datalen lv))))

;; Local variables specifying REDCap server settings
(define redcap:url "/redcap/api/")
(define (redcap-url-set! url) (set! redcap:url url))
(define redcap:user-agent "lambdanative/1.0")
(define redcap:content-type "application/x-www-form-urlencoded")
(define redcap:content-type-file "multipart/form-data")
(define redcap:boundary (string-append "------------------------------" 
  (number->string (inexact->exact (floor (current-time-seconds))))))
(define redcap:buf (##still-copy (make-u8vector 1024)))

;; Helper function to remove quotes from strings.
(define (redcap:string-remove-quotes str)
  (let ((strlength (string-length str)))
    (if (and (fx>= (string-length str) 2)
             (string=? (substring str 0 1) "\"")
             (string=? (substring str (- strlength 1) strlength) "\""))
      (substring str 1 (- strlength 1))
      str)
  ))

;; Helper function to parse REDCAP JSON format [much easier than XML parsing]
(define (redcap:jsonstr->list str)
  (if (or (list? str) (fx< (string-length str) 3) (not (string-contains str "[{")) (not (string-contains str "}]")))
    (list)
    (map (lambda (li) (if (fx> (length li) 0)
                        ;; THIRD, go through each field and recombine values that have commas in them
                        ;; while splitting between the field name and value, handling possible colons in the value
                        ;; and making a field name, value pair
                        (let cloop ((in (cdr li)) (transfer (car li)) (out '()))
                           (if (fx> (length in) 0)
                             (let ((item (car in)))
                                (if (and (char=? (string-ref item 0) #\") (fx> (string-length item) 1))
                                  ;; If string starts with a quotation mark (and isn't just the closing quotation mark), just transfer it to output list
                                   (cloop (cdr in) item (append out (list transfer)))
                                   ;; Otherwise, combine it with previous item being transferred as this is from the split of a comma within a value 
                                   (cloop (cdr in) (string-append transfer "," item) out)))
                             ;; Once through the whole list, split by :'s
                             (map (lambda (s) 
                                     (let ((p (string-split s #\:)))
                                        (if (fx= (length p) 2)
                                          ;; If there was only one colon, make field name, value pair
                                          (cons (redcap:string-remove-quotes (car p)) (redcap:string-remove-quotes (cadr p)))
                                          ;; Otherwise colons must have been found in the value, put it back into one string with colons and then make pair
                                          (cons (redcap:string-remove-quotes (car p)) (redcap:string-remove-quotes (string-mapconcat (cdr p) ":"))))))
                                  (append out (list transfer)))))
                        ;; If list is empty, do nothing to it
                        li))
         ;; SECOND split by commas after removing the extra starting two characters of each record - [{ or ,{ 
         ;; This split will separate into individual fields, although may split by commas in the middle of values
         (map (lambda (s) (string-split (substring s 2 (string-length s)) #\,)) 
              (string-split
                 ;; FIRST remove anything before the opening [{ or after the end }], include remove the ]
                 ;; and split by }, this splits it into records, but after this the first one will
                 ;; begin with [{ and the others will begin with ,{
                 (let ((index0 (string-contains str "[{"))
                       (index1 (string-contains str "}]")))
                    (substring str index0 index1))
                 #\}))))
)

;; Helper function to split return string into header and body
(define (redcap:split-headerbody str)
  (let ((pos (string-contains str "\r\n\r\n")))
    (if pos (list (substring str 0 pos) (substring str (+ pos 4) (string-length str))) (list str (list)))
  )
)

;; Helper function that returns a returned vector split into header and body
;; and changes the header into a string
(define (redcap:split-headerbody-vector vctr)
   (let loop ((lst (u8vector->list vctr)) (hdrlst '()))
     (if (fx> (length lst) 4)
       ;; Look for the vector equivalent of "\r\n\r\n"
       (if (and (fx= (car lst) 13) (fx= (cadr lst) 10) (fx= (caddr lst) 13) (fx= (cadddr lst) 10))
         ;; Turn the header into a string, keep everything after the header as a vector
         (list (u8vector->string (list->u8vector hdrlst)) (list->u8vector (list-tail lst 4)))
         ;; Otherwise go to the next spot in the list
         (loop (cdr lst) (append hdrlst (list (car lst)))))
       ;; If through the list and no divider found, just return as string - just header
       (list (u8vector->string vctr) #f)))
)

;; Helper function which makes REDCap EAV XML given a record identifier and list of values
(define (redcap:list->xmlstr record event data)
  (string-append "<?xml version=\"1.0\" encoding=\"UTF-8\" ?>" "\r\n" 
    "<records>" "\r\n"
    (let loop ((i 0) (str ""))
      (if (= i (length data)) str
  (loop (+ i 1) (string-append str "<item>"
    "<record>" record "</record>"
          "<redcap_event_name>" event "</redcap_event_name>"
    "<field_name>" (car (list-ref data i)) "</field_name>"
    "<value>" (cadr (list-ref data i))"</value>"
    "</item>" "\r\n"))
      )
    )
    "</records>")
)

(define (redcap-export-metadata host token . xargs)
         ;; See if format was specified in xargs, use json by default
  (let* ((format (redcap:arg 'format xargs "json"))
         (request (string-append "format=" format "&content=metadata&token=" token "\n"))
         (request-str (string-append "POST " redcap:url " HTTP/1.0" "\n"
           "Host: " host "\n"
           "User-Agent: " redcap:user-agent  "\n"
           "Content-Type: " redcap:content-type  "\n"
           "Content-Length: " (number->string (string-length request)) "\n"
           "\r\n" request "\n")))
    ;; Check if we have a valid connection before proceeding
    (if (fx= (httpsclient-open host) 1)
      (begin
        (httpsclient-send (string->u8vector request-str))
        (redcap:data-clear!)
        (let loop ((n #f))
          (if (and n (fx<= n 0)) 
            (begin 
              (httpsclient-close)  
              (let ((output (cadr (redcap:split-headerbody (redcap:data->string)))))
                 (if (string=? format "json")
                   ;; If format is json, turn into a list, otherwise just return output
                   (redcap:jsonstr->list output)
                   output))
            ) (begin
             (if (and n (> n 0)) 
               (redcap:data-append! (subu8vector redcap:buf 0 n)))
            (loop (httpsclient-recv redcap:buf))
          ))
        )
      )
      (begin
        (log-warning "Cannot export from REDCap, no valid connection")
        (httpsclient-close)
        #f ;; Denote difference between no data and no connection
      )
    )
  )
)

(define (redcap-import-record host token record data . xargs)
 (let*  ((bd redcap:boundary)
        (ct (string-append "--" bd "\r\n"
             "Content-Disposition: form-data; name=\"content\"" "\r\n" "\r\n" 
             "record" "\r\n"))
        (ft (string-append "--" bd "\r\n"
             "Content-Disposition: form-data; name=\"format\"" "\r\n" "\r\n" 
             "xml" "\r\n"))
        (tp (string-append "--" bd "\r\n"
             "Content-Disposition: form-data; name=\"type\"" "\r\n" "\r\n" 
             "eav" "\r\n"))
        (ob (string-append "--" bd "\r\n"
             "Content-Disposition: form-data; name=\"overwriteBehavior\"" "\r\n" "\r\n" 
             "overwrite" "\r\n"))
        (tk (string-append "--" bd "\r\n"
             "Content-Disposition: form-data; name=\"token\"" "\r\n" "\r\n"
             token "\r\n"))
        (event (redcap:arg 'event xargs ""))
        (dt (string-append "--" bd "\r\n"
             "Content-Disposition: form-data; name=\"data\"" "\r\n"
             "Content-Type: application/xml; charset=utf-8" "\r\n" "\r\n" 
             (redcap:list->xmlstr record event data) "\r\n"))
        (cl (string-append "--" bd "--"))
        (request-str (string-append "POST " redcap:url " HTTP/1.0" "\r\n"
          "Host: " host "\r\n"
          "User-Agent: " redcap:user-agent  "\r\n"
          "Content-Length: " (number->string (+ (string-length ct) (string-length ft) (string-length tp) 
            (string-length ob) (string-length tk) (string-length dt) (string-length cl))) "\r\n"  
          "Content-Type: " redcap:content-type-file "; boundary=" bd "\r\n" "\r\n"
          ct ft tp ob tk dt cl "\r\n")))
    ;; Check if we have a valid connection before proceeding
    (if (fx= (httpsclient-open host) 1)
      (begin
        (httpsclient-send (string->u8vector request-str))
        (redcap:data-clear!)
        (let loop ((n #f))
          (if (and n (fx<= n 0)) 
            (begin 
              (httpsclient-close)  
              (let ((msg (redcap:split-headerbody (redcap:data->string))))
                (if (and (string? (car msg)) (fx> (string-length (car msg)) 12) (or (string=? (substring (car msg) 9 12) "201")
                                                                                    (string=? (substring (car msg) 9 12) "200"))) #t
                  (begin (log-error "REDCap:" (cadr msg)) #f)
                )
              )
            ) (begin
             (if (and n (> n 0)) 
               (redcap:data-append! (subu8vector redcap:buf 0 n)))
             (loop (httpsclient-recv redcap:buf))
          ))
        )
      )
      (begin
        (log-warning "Cannot import to REDCap, no valid connection")
        (httpsclient-close)
        #f
      )
    )
  )
)

(define (redcap-import-record-csv host token str . xargs)
 (let*  ((bd redcap:boundary)
        (ct (string-append "--" bd "\r\n"
             "Content-Disposition: form-data; name=\"content\"" "\r\n" "\r\n" 
             "record" "\r\n"))
        (ft (string-append "--" bd "\r\n"
             "Content-Disposition: form-data; name=\"format\"" "\r\n" "\r\n" 
             "csv" "\r\n"))
        (tp (string-append "--" bd "\r\n"
             "Content-Disposition: form-data; name=\"type\"" "\r\n" "\r\n" 
             "flat" "\r\n"))
        ;; Default overwriteBehavior is "overwrite", but can be specified in xargs as "normal"
        (over (redcap:arg 'overwrite xargs "overwrite"))
        (ob (string-append "--" bd "\r\n"
             "Content-Disposition: form-data; name=\"overwriteBehavior\"" "\r\n" "\r\n" 
             over "\r\n"))
        (tk (string-append "--" bd "\r\n"
             "Content-Disposition: form-data; name=\"token\"" "\r\n" "\r\n"
             token "\r\n"))
        (dt (string-append "--" bd "\r\n"
             "Content-Disposition: form-data; name=\"data\"" "\r\n"
             "Content-Type: text/csv" "\r\n" "\r\n" 
             str "\r\n"))
        (cl (string-append "--" bd "--"))
        (request-str (string-append "POST " redcap:url " HTTP/1.0" "\r\n"
          "Host: " host "\r\n"
          "User-Agent: " redcap:user-agent  "\r\n"
          "Content-Length: " (number->string (+ (string-length ct) (string-length ft) (string-length tp) 
            (string-length ob) (string-length tk) (string-length dt) (string-length cl))) "\r\n"  
          "Content-Type: " redcap:content-type-file "; boundary=" bd "\r\n" "\r\n"
          ct ft tp ob tk dt cl "\r\n")))
    ;; Check if we have a valid connection before proceeding
    (if (fx= (httpsclient-open host) 1)
      (begin
        (httpsclient-send (string->u8vector request-str))
        (redcap:data-clear!)
        (let loop ((n #f))
          (if (and n (fx<= n 0)) 
            (begin 
              (httpsclient-close)  
              (let ((msg (redcap:split-headerbody (redcap:data->string))))
                (if (and (string? (car msg)) (fx> (string-length (car msg)) 12) 
                         (or (string=? (substring (car msg) 9 12) "201")
                             (string=? (substring (car msg) 9 12) "200"))) 
                  #t
                  (let ((message (cadr msg))) 
                    (log-error "REDCap:" (if (list? message) (car msg) (cadr msg)))
                    #f)
                )
              )
            ) (begin
             (if (and n (> n 0)) 
               (redcap:data-append! (subu8vector redcap:buf 0 n)))
             (loop (httpsclient-recv-reentrant redcap:buf))
          ))
        )
      )
      (begin
        (log-warning "Cannot import to REDCap, no valid connection")
        (httpsclient-close)
        #f
      )
    )
  )
)

(define (redcap-export-records host token . xargs)
  ;; See if format was specified in xargs, use json by default
  (let* ((format (redcap:arg 'format xargs "json"))
         (request (string-append "token=" token "&content=record&format=" format "&type=flat"))
         (records (redcap:arg 'records xargs #f))
         (recordstr (if (pair? records)
           (let loop ((i 0) (str ""))
             (if (= i (length records)) str
               (loop (+ i 1) (string-append str "&records%5B" (number->string i) "%5D=" (list-ref records i)))
             ))
           ""))
         (events (redcap:arg 'events xargs #f))
         (eventstr (if (pair? events)
           (let loop ((i 0) (str ""))
             (if (= i (length events)) str
               (loop (+ i 1) (string-append str "&events%5B" (number->string i) "%5D=" (list-ref events i)))
             ))
           ""))
         (forms (redcap:arg 'forms xargs #f))
         (formstr (if (pair? forms)
           (let loop ((i 0) (str ""))
             (if (= i (length forms)) str
               (loop (+ i 1) (string-append str "&forms%5B" (number->string i) "%5D=" (list-ref forms i)))
             ))
           ""))
         (fields (redcap:arg 'fields xargs #f))
         (fieldstr (if (pair? fields)
           (let loop ((i 0) (str ""))
             (if (= i (length fields)) str
               (loop (+ i 1) (string-append str "&fields%5B" (number->string i) "%5D=" (list-ref fields i)))
             ))
           ""))
         (request-str (string-append "POST " redcap:url " HTTP/1.0" "\n"
           "Host: " host "\n"
           "User-Agent: " redcap:user-agent  "\n"
           "Content-Type: " redcap:content-type  "\n"
           "Content-Length: " (number->string (+ (string-length request) (string-length recordstr) (string-length eventstr) (string-length formstr) (string-length fieldstr))) "\n"
           "\r\n" request recordstr eventstr formstr fieldstr "\n")))

    ;; Check if we have a valid connection before proceeding
    (if (fx= (httpsclient-open host) 1)
      (begin
        (httpsclient-send (string->u8vector request-str))
        (redcap:data-clear!)
        (let loop ((n #f))
          (if (and n (fx<= n 0)) 
            (begin 
              (httpsclient-close)
              (let ((output (cadr (redcap:split-headerbody (redcap:data->string)))))
                 (if (string=? format "json")
                   ;; If format is json, turn into a list, otherwise just return output
                   (if (string? output) (redcap:jsonstr->list output) #f)
                   output))
            ) (begin
            (if (and n (> n 0)) 
              (redcap:data-append! (subu8vector redcap:buf 0 n)))
            (loop (httpsclient-recv-reentrant redcap:buf))
          ))
        )
      )
      (begin
        (log-warning "Cannot export from REDCap, no valid connection")
        (httpsclient-close)
        #f  ;; Return false to denote difference between no data and no connection
      )
    )
  )
)

(define (redcap-export-ids host token . xargs)
  (let* ((event (redcap:arg 'event xargs #f))
         (request (string-append "token=" token "&content=record&" (if event (string-append "events=" event "&") "") "format=json&fields=participant_id&type=flat"))
         (request-str (string-append "POST " redcap:url " HTTP/1.0" "\n"
           "Host: " host "\n"
           "User-Agent: " redcap:user-agent  "\n"
           "Content-Type: " redcap:content-type  "\n"
           "Content-Length: " (number->string (+ (string-length request))) "\n"
           "\r\n" request "\n")))
    ;; Check if we have a valid connection before proceeding
    (if (fx= (httpsclient-open host) 1)
      (begin
        (httpsclient-send (string->u8vector request-str))
        (redcap:data-clear!)
        (let loop ((n #f))
          (if (and n (fx<= n 0)) 
            (begin 
              (httpsclient-close)
              
              ;; Get data as a list, make sure first entry isn't an error
              (let ((datalist (redcap:jsonstr->list (cadr (redcap:split-headerbody (redcap:data->string))))))
                
                (if (and (fx> (length datalist) 0) (string=? (caaar datalist) "error\""))
                  ;; If the first entry is an error, then log it and return false
                  (begin
                    (log-error "REDCap error: " (cdaar datalist))
                    #f)
                   (maps (lambda (l)
                          (cdr (car l))) 
                         datalist))))
            (begin
              (if (and n (> n 0)) 
                (redcap:data-append! (subu8vector redcap:buf 0 n)))
              (loop (httpsclient-recv redcap:buf))
          ))
        )
      )
      (begin
        (log-warning "Cannot export from REDCap, no valid connection")
        (httpsclient-close)
        #f  ;; Denote difference between no data and no connection
      )
    )
  )
)

(define (redcap-import-file host token record field filename . xargs)
  (let* ((filesize (if (file-exists? filename)
           (file-info-size (file-info filename)) #f))
         (data
           ;; Create string that is the same length as the file
           (let ((content (if filesize (make-u8vector filesize) #f)))
             (if content 
               (begin
                 ;; Read file into the string and return it
                 (with-input-from-file filename (lambda ()
                   (read-subu8vector content 0 filesize)))
                 content)
               (u8vector))))
         (bd redcap:boundary)
         (ct (string-append "--" bd "\r\n"
              "Content-Disposition: form-data; name=\"content\"" "\r\n" "\r\n"
              "file" "\r\n"))
         (at (string-append "--" bd "\r\n"
              "Content-Disposition: form-data; name=\"action\"" "\r\n" "\r\n"
              "import" "\r\n"))
         (rd (string-append "--" bd "\r\n"
              "Content-Disposition: form-data; name=\"record\"" "\r\n" "\r\n"
              record "\r\n"))
         (fd (string-append "--" bd "\r\n"
              "Content-Disposition: form-data; name=\"field\"" "\r\n" "\r\n"
              field "\r\n"))
         (form (redcap:arg 'form xargs #f))
         (fo (if form 
               (string-append "--" bd "\r\n"
                 "Content-Disposition: form-data; name=\"form_instance_id\"" "\r\n" "\r\n"
                 form "\r\n")
               ""))
         (event (redcap:arg 'event xargs #f))
         (ev (if event 
               (string-append "--" bd "\r\n"
                 "Content-Disposition: form-data; name=\"event\"" "\r\n" "\r\n"
                 event "\r\n")
             ""))
         (tk (string-append "--" bd "\r\n"
              "Content-Disposition: form-data; name=\"token\"" "\r\n" "\r\n"
              token "\r\n"))
         (dtvector (u8vector-append 
              (string->u8vector (string-append "--" bd "\r\n"
                                 "Content-Disposition: form-data; name=\"file\"; filename=\"" filename "\"" "\r\n"
                                 "Content-Type: application/octet-stream" "\r\n" "\r\n"))
              data 
              (string->u8vector "\r\n")))
         (cl (string-append "--" bd "--"))
         (request-vector (u8vector-append
           (string->u8vector (string-append "POST " redcap:url " HTTP/1.0" "\r\n"
                               "Host: " host "\r\n"
                               "User-Agent: " redcap:user-agent  "\r\n"
                               "Content-Length: " (number->string (+ (string-length ct) (string-length at) (string-length rd) 
                               (string-length fd) (string-length tk) (string-length fo) (string-length ev) (u8vector-length dtvector) (string-length cl))) "\r\n"
                               "Content-Type: " redcap:content-type-file "; boundary=" bd "\r\n" "\r\n"
                               ct at rd fo ev fd tk))
           dtvector 
           (string->u8vector (string-append cl "\r\n")))))

   ;; Check if we have a valid connection before proceeding
    (if (and filesize (fx= (httpsclient-open host) 1))
      (begin
        (httpsclient-send request-vector)
        (redcap:data-clear!)
        (let loop ((n #f))
          (if (and n (fx<= n 0))
            (begin
              (httpsclient-close)
              (let ((msg (redcap:split-headerbody (redcap:data->string))))
                (if (and (string? (car msg)) (fx> (string-length (car msg)) 12) 
                         (or (string=? (substring (car msg) 9 12) "201")                                                                                   (string=? (substring (car msg) 9 12) "200"))) 
                  #t
                  (let ((message (cadr msg)))
                    (log-error "REDCap:" (if (list? message) (if (fx> (string-length (car msg)) 0) (car msg) " No response, file may be too large for upload") message))
                    #f)
                )
              )
            ) (begin
            (if (and n (> n 0)) 
              (redcap:data-append! (subu8vector redcap:buf 0 n)))
            (loop (httpsclient-recv redcap:buf))
          ))
        )
      )
      (begin
        (if (not filesize) 
          (let ((str (string-append "File " filename " not found."))) (log-error str))
          (log-warning "Cannot import to REDCap, no valid connection"))
        (httpsclient-close)
        #f
      )
    )
  )
)

(define (redcap-export-file host token record field . xargs)
  (let* ((event (redcap:arg 'event xargs ""))
         (form (redcap:arg 'form xargs #f))
         (request (string-append "content=file&action=export&token=" token
                                 "&record=" record
                                 (if event
                                   (string-append "&event=" event)
                                   ""
                                 )
                                 (if form
                                   (string-append "&form_instance_id=" form)
                                   ""
                                 )
                                 "&field=" field))
         (request-str (string-append "POST " redcap:url " HTTP/1.0" "\n"
           "Host: " host "\n"
           "User-Agent: " redcap:user-agent  "\n"
           "Content-Type: " redcap:content-type  "\n"
           "Content-Length: " (number->string (string-length request)) "\n"
           "\r\n" request "\n")))
    ;; Check if we have a valid connection before proceeding
    (if (fx= (httpsclient-open host) 1)
      (begin
        (httpsclient-send (string->u8vector request-str))
        (redcap:data-clear!)
        (let loop ((n 1))
          (if (fx<= n 0) 
            (begin 
              (httpsclient-close)
              ;; Get a list consisting of the header as a string followed by the body as a vector
              (let ((fileout (redcap:split-headerbody-vector (redcap:data->u8vector))))
                ;; If the returned code is 200 or 201, just return the header and file
                (if (and (string? (car fileout)) (fx> (string-length (car fileout)) 12) 
                         (or (string=? (substring (car fileout) 9 12) "201")
                             (string=? (substring (car fileout) 9 12) "200")))
                  fileout
                  ;; Display and log the REDCap error message, but return the error message as a string
                  (let ((message (u8vector->string (cadr fileout))))
                    (log-error "REDCap:" message)
                    message)
                ))
            )
            (let ((count (httpsclient-recv redcap:buf)))
              (if (> count 0) (redcap:data-append! redcap:buf))
              (loop count))
          )
        )
      )
      (begin
        (log-warning "Cannot export from REDCap, no valid connection")
        (httpsclient-close)
        "" ;; Could also return #f
      )
    )
  )
)

(define (redcap:arg k args def)
  (let loop ((as args))
    (if (fx= (length as) 0)
      def
      (if (equal? k (car as)) (cadr as) (loop (cdr as)))
    )
  ))
;; eof
