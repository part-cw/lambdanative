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
    ;; Determine whether content just empty or whether it was improperly formatted (possibly incomplete)
    (if (and (string? str) (fx>= (string-length str) 3)) #f (list))
    (let* ((index (string-contains str "[{"))
           ;; Remove anything outside brackets first
           (output (json-decode (substring str index (string-length str)))))
      (if (json-error? output) #f (vector->list output))))
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
(define (redcap:list->xmlstr record event data . xargs)
  (let* ((instance (redcap:arg 'instance xargs #f))
        (instrument (redcap:arg 'instrument xargs #f))
        (repeat (if (and instance instrument) ;;add repeat information if provided
                     (string-append     "<redcap_repeat_instrument>" instrument "</redcap_repeat_instrument>"
                                        "<redcap_repeat_instance>" instance "</redcap_repeat_instance>") "")))
  	(string-append "<?xml version=\"1.0\" encoding=\"UTF-8\" ?>" "\r\n"
    "<records>" "\r\n"
    (let loop ((i 0) (str ""))
      (if (= i (length data)) str
  	(loop (+ i 1) 
         (let* ((lpair (list-ref data i))
                (field (car lpair))
                (fieldname (if (string? field) field (symbol->string field)))
                (val (if (list? lpair) (cadr lpair) (cdr lpair)))
                (value (if (number? val) (number->string val) val))) 
               (string-append str "<item>"
		    "<record>" record "</record>" 
		      repeat                                                     
		    "<redcap_event_name>" event "</redcap_event_name>"
		    "<field_name>" fieldname "</field_name>"
		    "<value>" value "</value>"
		    "</item>" "\r\n")))
		      )
		    )
		    "</records>"))
)

;; Helper function to build the http request string
(define (redcap:make-request-str host request . bd)
  (string-append "POST " redcap:url " HTTP/1.0" "\r\n"
    "Host: " host "\r\n"
    "User-Agent: " redcap:user-agent  "\r\n"
    "Content-Length: " (number->string (string-length request)) "\r\n"
    "Content-Type: " (if (null? bd) redcap:content-type (string-append redcap:content-type-file "; boundary=" (car bd))) "\r\n"
    "\r\n" request "\n"))

;; Checks for an error message in header, logs the message and returns false. If there is no error (ie. code is 200 or 201), just return true
(define (redcap:error-check msg)
   (if (and (string? (car msg)) (fx> (string-length (car msg)) 12) (or (string=? (substring (car msg) 9 12) "201")
                                                                       (string=? (substring (car msg) 9 12) "200")))
     (begin  (log-warning "REDCap: submission success. response" msg) #t)
     (let ((ret (cadr msg)))
       (log-error "REDCap:" (if (and (list? ret) (fx= (length ret) 0)) " Nothing returned" (cadr msg))) #f))
)

(define (redcap-export-metadata host token . xargs)
         ;; See if format was specified in xargs, use json by default
  (let* ((format (redcap:arg 'format xargs "json"))
         (forms (redcap:arg 'forms xargs #f))
         (fields (redcap:arg 'fields xargs #f))
         (request (string-append "format=" format "&content=metadata&token=" token (if forms (string-append "&forms[0]=" forms ) "") (if fields (string-append "&fields[0]=" fields ) "") ))  ;;TODO expand to multiple fields and forms
         (request-str (redcap:make-request-str host request)))
    ;; Check if we have a valid connection before proceeding
    (if (fx= (httpsclient-open host) 1)
      (begin
       ;;(display request-str)
        (httpsclient-send (string->u8vector request-str))
        (redcap:data-clear!)
        (let loop ((n #f))
          (if (and n (fx<= n 0))
            (begin
              (httpsclient-close)
              (let ((output (cadr (redcap:split-headerbody (redcap:data->string)))))
                 (if (string=? format "json")
                   ;; If format is json, turn into a list, otherwise just return output
                   (let ((datalist (redcap:jsonstr->list output)))
                     (cond
                       ((not (list? datalist))
                         ;; If no list returned, json not properly formatted
                         (log-error "REDCap error: Incomplete json " datalist)
                         #f)
                       ((and (fx> (length datalist) 0) (string=? (caaar datalist) "error\""))
                         ;; If the first entry is an error, then log it and return false
                         (log-error "REDCap error: " (cdaar datalist))
                          #f)
                       (else datalist)))
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

(define (redcap-export-instrument host token . xargs)
         ;; See if format was specified in xargs, use json by default
  (let* ((format (redcap:arg 'format xargs "json"))
         (request (string-append "format=" format "&content=instrument&token=" token))
         (request-str (redcap:make-request-str host request)))
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
                   (let ((datalist (redcap:jsonstr->list output)))
                     (cond
                       ((not (list? datalist))
                         ;; If no list returned, json not properly formatted
                         (log-error "REDCap error: Incomplete json")
                         #f)
                       ((and (fx> (length datalist) 0) (string=? (caaar datalist) "error\""))
                         ;; If the first entry is an error, then log it and return false
                         (log-error "REDCap error: " (cdaar datalist))
                          #f)
                       (else datalist)))
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
 (let*  ((event (redcap:arg 'event xargs ""))
        (instance (redcap:arg 'instance xargs #f))
        (instrument (redcap:arg 'instrument xargs #f))
        (type (redcap:arg 'type xargs "eav"))
        (over (redcap:arg 'overwrite xargs "overwrite"))
        (message   (string-append  "token=" token "&content=record&format=xml&type=" type "&overwriteBehavior=" over "&data=" (redcap:list->xmlstr record event data 'instance instance 'instrument instrument) " &returnContent=count&returnFormat=json")) 
        (request-str (redcap:make-request-str host message)))
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
                (redcap:error-check msg)
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
        (request (string-append ct ft tp ob tk dt cl))
        (request-str (redcap:make-request-str host request bd)))
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
                (redcap:error-check msg)
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
  (let* ((format (redcap:arg 'format xargs "json"));; See if format was specified in xargs, use json by default
         (type (redcap:arg 'type xargs "flat"))
         (request (string-append "token=" token "&content=record&format=" format "&type=" type))
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
         (filter (redcap:arg 'filter xargs #f))
         (filterstr (if (string? filter)
                      (string-append "&filterLogic=" filter)
                      ""))
         (request-all (string-append request eventstr formstr fieldstr recordstr filterstr))
         (request-str (redcap:make-request-str host request-all)))
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
                (if (redcap:error-check msg)
                  (let ((output (cadr msg)))
                    (if (string=? format "json")
                      ;; If format is json, turn into a list, otherwise just return output
                      (if (string? output)
                        ;; If a string turn into a list, otherwise just return false
                        (let ((datalist (redcap:jsonstr->list output)))
                          (cond
                            ((not (list? datalist))
                              ;; If no list returned, json not properly formatted
                              (log-error "REDCap error: Incomplete json" msg)
                              #f)
                            ((and (fx> (length datalist) 0) (string=? (caaar datalist) "error\""))
                              ;; If the first entry is an error, then log it and return false
                              (log-error "REDCap error: " (cdaar datalist))
                              #f)
                            (else datalist)))
                        #f)
                      output))
                  #f))
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

;returns the index for instance in
(define (redcap-get-next-instance host token record form)
  (let* ((forms (if (pair? form) form (list form)))
       (records (if (pair? record) record (list record)))
       (maxinstance 0)
       (response (redcap-export-records host token 'forms forms 'records records 'type "eav"))  ;;currently redcap8.0 flat does not return instances
       (len (if response (length response) 0))
       )
    (if response
       (if (fx> len 0)
           (begin (let loop ((entries response))
            (if (fx> (length entries) 0)
              	(let* ((row (car entries))
                       (val (alist-ref row "redcap_repeat_instance" 0))
                       (instrument (alist-ref row "redcap_repeat_instrument" #f))
                       (instance  (if (string? val) (string->number (string-remove-quotes val)) val)))
                      (if (not instrument) (log-warning "Exported REDcap instrument " (car forms) " is not repeatable"))
                      (if (fx> instance maxinstance) (set! maxinstance instance))
                      (loop (cdr entries))))) (fx+ maxinstance 1))
           (begin (log-warning "Exported REDcap record has no repeated entry") 0))
       (begin (log-warning "Cannot retrieve instance number from REDCap") #f)))
)


(define (redcap-export-fieldnames host token)
  (let* ((request (string-append "token=" token "&content=exportFieldNames&format=json"))
         (request-str (redcap:make-request-str host request)))
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
              (let ((msg (redcap:split-headerbody (redcap:data->string))))
                (if (redcap:error-check msg)
                  (let ((datalist (redcap:jsonstr->list (cadr msg))))
                    (cond
                      ((not (list? datalist))
                         ;; If no list returned, json not properly formatted
                         (log-error "REDCap error: Incomplete json")
                         #f)
                      ((and (fx> (length datalist) 0) (string=? (caaar datalist) "error\""))
                        ;; If the first entry is an error, then log it and return false
                        (log-error "REDCap error: " (cdaar datalist))
                        #f)
                      (else
                        (map cdr (map caddr datalist)))))
                  #f)))
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

(define (redcap-export-ids host token . xargs)
  (let ((fieldnames (redcap-export-fieldnames host token)))
    (if fieldnames
      (let* ((event (redcap:arg 'event xargs #f))
             (id (if (list? fieldnames) (car fieldnames) #f))
             (request (string-append "token=" token "&content=record&" (if event (string-append "events=" event "&") "")
                                     "format=json&fields=" id "&type=flat"))
             (request-str (redcap:make-request-str host request)))
        ;; Check if we have a valid connection before proceeding
        (if (and (fx= (httpsclient-open host) 1) id)
          (begin
            (httpsclient-send (string->u8vector request-str))
            (redcap:data-clear!)
            (let loop ((n #f))
              (if (and n (fx<= n 0))
                (begin
                  (httpsclient-close)
                  ;; Get data as a list, make sure first entry isn't an error
                  (let ((msg (redcap:split-headerbody (redcap:data->string))))
                    (if (redcap:error-check msg)
                      (let ((datalist (redcap:jsonstr->list (cadr msg))))
                        (cond
                          ((not (list? datalist))
                            ;; If no list returned, json not properly formatted
                            (log-error "REDCap error: Incomplete json")
                            #f)
                          ((and (fx> (length datalist) 0) (string=? (caaar datalist) "error\""))
                             ;; If the first entry is an error, then log it and return false
                            (log-error "REDCap error: " (cdaar datalist))
                            #f)
                          (else
                            (maps
                              (lambda (entry)
                                (let ((id (cdar entry)))
                                  ;; May be a number if downloaded from newer REDCap - convert back to string
                                  (if (number? id) (number->string id) id)))
                              datalist))))
                      #f)))
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
      ;; No fieldnames returned, so cannot get IDs
      #f
    )
  )
)


(define (redcap-import-file host token record field filename . xargs)
  (let* ((filesize (if (file-exists? filename) (file-info-size (file-info filename)) #f))
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
         (repeat (redcap:arg 'repeat xargs #f))
         (re (if repeat
               (string-append "--" bd "\r\n"
                 "Content-Disposition: form-data; name=\"repeat_instance\"" "\r\n" "\r\n"
                 repeat "\r\n")
             ""))
         (tk (string-append "--" bd "\r\n"
              "Content-Disposition: form-data; name=\"token\"" "\r\n" "\r\n"
              token "\r\n"))
         (dt (string-append "--" bd "\r\n"
           "Content-Disposition: form-data; name=\"file\"; filename=\"" filename "\"" "\r\n"
           "Content-Type: application/octet-stream" "\r\n" "\r\n"))
         (cl (string-append "--" bd "--"))
         (close-vector (u8vector-append (string->u8vector (string-append "\r\n" cl "\r\n"))))
         (request-vector (string->u8vector (string-append "POST " redcap:url " HTTP/1.0" "\r\n"
           "Host: " host "\r\n"
           "User-Agent: " redcap:user-agent  "\r\n"
           "Content-Length: " (number->string (+ (string-length ct) (string-length at) (string-length rd)
             (string-length fd) (string-length tk) (string-length fo) (string-length ev) (string-length re) (string-length dt)
             (if filesize filesize 0) (u8vector-length close-vector))) "\r\n"
           "Content-Type: " redcap:content-type-file "; boundary=" bd "\r\n" "\r\n"
             ct at rd fo ev re fd tk dt))))

   ;; Check if we have a valid connection before proceeding
    (if (and filesize (fx= (httpsclient-open host) 1))
      (let* ((fh (open-input-file filename))
             (buflen 100000)
             (buf (make-u8vector buflen)))
        (httpsclient-send request-vector)
        (let loop ((start 0) (end (if (fx< filesize buflen) filesize buflen)))
          (if (fx>= start filesize)
            (if (port? fh) (close-input-port fh))
            (let ((len (fx- end start)))
              (input-port-byte-position fh start)
              (read-subu8vector buf 0 len fh)
              (if (fx< len buflen)
                (httpsclient-send (subu8vector buf 0 len))
                (httpsclient-send buf)
              )
              (loop (fx+ start buflen) (if (fx> (fx+ start (fx* 2 buflen)) filesize) filesize (fx+ end buflen)))
            )
          )
        )
        (httpsclient-send close-vector)
        (redcap:data-clear!)
        (let loop ((n #f))
          (if (and n (fx<= n 0))
            (begin
              (httpsclient-close)
              (let ((msg (redcap:split-headerbody (redcap:data->string))))
                (redcap:error-check msg)
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
          (begin
            (log-warning "Cannot import to REDCap, no valid connection")
            (httpsclient-close)))
        #f
      )
    )
  )
)

(define (redcap-export-file host token record field . xargs)
  (let* ((event (redcap:arg 'event xargs ""))
         (repeat (redcap:arg 'repeat xargs ""))
         (form (redcap:arg 'form xargs #f))
         (request (string-append "content=file&action=export&token=" token
                                 "&record=" record
                                 (if event
                                   (string-append "&event=" event)
                                   ""
                                 )
                                 (if repeat
                                   (string-append "&repeat_instance=" repeat)
                                   ""
                                 )
                                 (if form
                                   (string-append "&form_instance_id=" form)
                                   ""
                                 )
                                 "&field=" field))
         (request-str (redcap:make-request-str host request)))
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

(define (redcap-delete-file host token record field . xargs)
  (let* ((event (redcap:arg 'event xargs ""))
         (form (redcap:arg 'form xargs #f))
         (request (string-append "content=file&action=delete&token=" token
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
         (request-str (redcap:make-request-str host request)))
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
                (redcap:error-check msg)
              )
            ) (begin
            (if (and n (> n 0))
              (redcap:data-append! (subu8vector redcap:buf 0 n)))
            (loop (httpsclient-recv redcap:buf))
          ))
        )
      )
      (begin
        (log-warning "Cannot delete file on REDCap, no valid connection")
        (httpsclient-close)
      )
    )
  )
)

(define (redcap-get-filename header)
         ;; Get index of name=" which occurs before file name in the header
  (let* ((nameindex1 (string-contains header "name=\""))
         ;; Get part of header string after the above string
         (subheader (substring header (+ nameindex1 6) (string-length header)))
         ;; Get index of next quotation marks
         (nameindex2 (if nameindex1 (string-contains subheader "\"") #f)))
      ;; Return the name of the file
      (if (and nameindex1 nameindex2)
        (substring subheader 0 nameindex2)
        #f))
)

(define (redcap:arg k args def)
  (let loop ((as args))
    (if (fx= (length as) 0)
      def
      (if (equal? k (car as)) (cadr as) (loop (cdr as)))
    )
  ))
;; eof
