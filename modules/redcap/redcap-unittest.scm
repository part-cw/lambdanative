;; Test information - must be filled in to work
;; To setup the test project that works with this unit test, import the files appropriately in REDCap:
;; REDCapDataDictionaryForUnitTest.csv, REDCapEventsForUnitTest.csv, and REDCapInstrumentMappingForUnitTest.csv
;; The unit test uses the default Arm 1.
;; For a true test, delete all records in the project before running it
(define redcap:testurl "/redcap/api/") ;; Update this if necessary
(define redcap:testhost "<add here>")
(define redcap:testtoken "<add here>")


;; HELPER FUNCTIONS
;; The folowing two are identical to Racket's take and drop: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Flist..rkt%29._take%29%29
(define (take lst n)
  (if (eq? n 0)
      '()
      (cons (car lst) (take (cdr lst) (- n 1)))))

(define (drop lst n)
  (if (eq? n 0)
      lst
      (drop (cdr lst) (- n 1))))

;; Test equality with given function; print both arguments if they are not equal
(define (test-success eqfn? result expectation)
  (let ((success (eqfn? result expectation)))
    (if (not success) (begin (display "Result obtained:\n") (display result) (display "\nis not equal to expected value:\n") (display expectation) (newline)))
    success))

(define (test-failure eqfn? result expectation)
  (test-success (lambda (r e) (not (eqfn? r e))) result expectation))

;; Unit test template
;; f takes no arguments and returns a boolean
(define (add-unit-test name f)
  (unit-test "REDCap" name
    (lambda ()
      (display (string-append "TEST - " name "\n"))
      (redcap-url-set! redcap:testurl)
      (let ((success (f)))
        (display (if success "PASS\n\n" "FAIL\n\n"))
        success))))


;; UNIT TESTS
;; Metadata-related functions
;;  - redcap-export-metadata
;;    - forms
;;    - format
;;    - fields
;;  - redcap-export-instrument
;;  - redcap-export-fieldnames
(unit-test "REDCap" "Metadata"
  (lambda ()
    (redcap-url-set! redcap:testurl)
    (let ((success #t))
      (set! success (test-success equal? (redcap-export-metadata redcap:testhost redcap:testtoken) redcap:testmetadata))
      (if success (set! success (test-success equal? (redcap-export-metadata redcap:testhost redcap:testtoken 'forms "form_2") (take (drop redcap:testmetadata 3) 6))))
      (if success (set! success (test-success string=? (redcap-export-metadata redcap:testhost redcap:testtoken 'format "xml" 'fields "num") redcap:testnum)))
      (if success (set! success (test-success string=? (redcap-export-metadata redcap:testhost redcap:testtoken 'format "csv" 'fields "firstrow") redcap:testfirst)))
      (if success (set! success (test-success string=? (redcap-export-instrument redcap:testhost redcap:testtoken 'format "csv") "instrument_name,instrument_label\nform_1,\"Form 1\"\nform_2,\"Form 2\"\nform_3,\"Form 3\"\n")))
      (if success (set! success (test-success string=? (redcap-export-instrument redcap:testhost redcap:testtoken 'format "xml") (string-append "<?xml version=\"1.0\" encoding=\"UTF-8\" ?>\n<instruments>\n"
                                                                                                                                                   "<item><instrument_name><![CDATA[form_1]]></instrument_name>"
                                                                                                                                                   "<instrument_label><![CDATA[Form 1]]></instrument_label></item>\n"
                                                                                                                                                   "<item><instrument_name><![CDATA[form_2]]></instrument_name>"
                                                                                                                                                   "<instrument_label><![CDATA[Form 2]]></instrument_label></item>\n"
                                                                                                                                                   "<item><instrument_name><![CDATA[form_3]]></instrument_name>"
                                                                                                                                                   "<instrument_label><![CDATA[Form 3]]></instrument_label></item>\n</instruments>\n"))))
      (if success (set! success (test-success equal? (redcap-export-instrument redcap:testhost redcap:testtoken) '((("instrument_name" . "form_1") ("instrument_label" . "Form 1")) (("instrument_name" . "form_2") ("instrument_label" . "Form 2")) (("instrument_name" . "form_3") ("instrument_label" . "Form 3"))))))
      ;; Export all fields, does not include file fields
      (if success (set! success (test-success equal? (redcap-export-fieldnames redcap:testhost redcap:testtoken) redcap:testfields)))
      success)))

(define redcap:testmetadata '((("field_name" . "study_no") ("form_name" . "form_1") ("section_header" . "") ("field_type" . "text") ("field_label" . "Study No") ("select_choices_or_calculations" . "") ("field_note" . "")
                               ("text_validation_type_or_show_slider_number" . "") ("text_validation_min" . "") ("text_validation_max" . "") ("identifier" . "") ("branching_logic" . "") ("required_field" . "y")
                               ("custom_alignment" . "") ("question_number" . "") ("matrix_group_name" . "") ("matrix_ranking" . "") ("field_annotation" . ""))
                              (("field_name" . "yesno") ("form_name" . "form_1") ("section_header" . "") ("field_type" . "yesno") ("field_label" . "Yes or No?") ("select_choices_or_calculations" . "") ("field_note" . "A note here")
                               ("text_validation_type_or_show_slider_number" . "") ("text_validation_min" . "") ("text_validation_max" . "") ("identifier" . "") ("branching_logic" . "") ("required_field" . "")
                               ("custom_alignment" . "RH") ("question_number" . "") ("matrix_group_name" . "") ("matrix_ranking" . "") ("field_annotation" . ""))
                              (("field_name" . "num") ("form_name" . "form_1") ("section_header" . "") ("field_type" . "text") ("field_label" . "This is an integer") ("select_choices_or_calculations" . "") ("field_note" . "")
                               ("text_validation_type_or_show_slider_number" . "integer") ("text_validation_min" . "0") ("text_validation_max" . "10") ("identifier" . "") ("branching_logic" . "") ("required_field" . "")
                               ("custom_alignment" . "") ("question_number" . "") ("matrix_group_name" . "") ("matrix_ranking" . "") ("field_annotation" . ""))
                              (("field_name" . "drop") ("form_name" . "form_2") ("section_header" . "") ("field_type" . "dropdown") ("field_label" . "Multiple choice dropdown") ("select_choices_or_calculations" . "1, One | 2, Two | 3, Three") ("field_note" . "")
                               ("text_validation_type_or_show_slider_number" . "") ("text_validation_min" . "") ("text_validation_max" . "") ("identifier" . "") ("branching_logic" . "") ("required_field" . "")
                               ("custom_alignment" . "") ("question_number" . "") ("matrix_group_name" . "") ("matrix_ranking" . "") ("field_annotation" . ""))
                              (("field_name" . "radio") ("form_name" . "form_2") ("section_header" . "") ("field_type" . "radio") ("field_label" . "Single answer") ("select_choices_or_calculations" . "1, A | 2, B | 3, C") ("field_note" . "")
                               ("text_validation_type_or_show_slider_number" . "") ("text_validation_min" . "") ("text_validation_max" . "") ("identifier" . "") ("branching_logic" . "") ("required_field" . "")
                               ("custom_alignment" . "") ("question_number" . "") ("matrix_group_name" . "") ("matrix_ranking" . "") ("field_annotation" . ""))
                              (("field_name" . "checks") ("form_name" . "form_2") ("section_header" . "Bottom section") ("field_type" . "checkbox") ("field_label" . "Names") ("select_choices_or_calculations" . "1, Fred Flinstone | 2, Jack Johnson | 3, Carl Sagan | 4, Bob Saget")
                               ("field_note" . "") ("text_validation_type_or_show_slider_number" . "") ("text_validation_min" . "") ("text_validation_max" . "") ("identifier" . "y") ("branching_logic" . "") ("required_field" . "y")
                               ("custom_alignment" . "LH") ("question_number" . "") ("matrix_group_name" . "") ("matrix_ranking" . "") ("field_annotation" . ""))
                              (("field_name" . "firstrow") ("form_name" . "form_2") ("section_header" . "Look, a matrix!") ("field_type" . "radio") ("field_label" . "First row") ("select_choices_or_calculations" . "1, A | 2, B | 3, C")
                               ("field_note" . "") ("text_validation_type_or_show_slider_number" . "") ("text_validation_min" . "") ("text_validation_max" . "") ("identifier" . "") ("branching_logic" . "") ("required_field" . "y")
                               ("custom_alignment" . "") ("question_number" . "") ("matrix_group_name" . "mat") ("matrix_ranking" . "y") ("field_annotation" . "first!"))
                              (("field_name" . "secondrow") ("form_name" . "form_2") ("section_header" . "") ("field_type" . "radio") ("field_label" . "Second row") ("select_choices_or_calculations" . "1, A | 2, B | 3, C")
                               ("field_note" . "") ("text_validation_type_or_show_slider_number" . "") ("text_validation_min" . "") ("text_validation_max" . "") ("identifier" . "") ("branching_logic" . "") ("required_field" . "")
                               ("custom_alignment" . "") ("question_number" . "") ("matrix_group_name" . "mat") ("matrix_ranking" . "y") ("field_annotation" . ""))
                              (("field_name" . "file") ("form_name" . "form_2") ("section_header" . "") ("field_type" . "file") ("field_label" . "A file") ("select_choices_or_calculations" . "") ("field_note" . "")
                               ("text_validation_type_or_show_slider_number" . "") ("text_validation_min" . "") ("text_validation_max" . "") ("identifier" . "") ("branching_logic" . "") ("required_field" . "")
                               ("custom_alignment" . "") ("question_number" . "") ("matrix_group_name" . "") ("matrix_ranking" . "") ("field_annotation" . ""))
                              (("field_name" . "date_dec") ("form_name" . "form_3") ("section_header" . "") ("field_type" . "text") ("field_label" . "Date (decreasing)") ("select_choices_or_calculations" . "") ("field_note" . "")
                               ("text_validation_type_or_show_slider_number" . "date_ymd") ("text_validation_min" . "") ("text_validation_max" . "") ("identifier" . "") ("branching_logic" . "") ("required_field" . "")
                               ("custom_alignment" . "") ("question_number" . "") ("matrix_group_name" . "") ("matrix_ranking" . "") ("field_annotation" . ""))
                              (("field_name" . "date_inc_sec") ("form_name" . "form_3") ("section_header" . "") ("field_type" . "text") ("field_label" . "Date with seconds (increasing)") ("select_choices_or_calculations" . "") ("field_note" . "")
                               ("text_validation_type_or_show_slider_number" . "datetime_seconds_dmy") ("text_validation_min" . "") ("text_validation_max" . "") ("identifier" . "") ("branching_logic" . "") ("required_field" . "")
                               ("custom_alignment" . "") ("question_number" . "") ("matrix_group_name" . "") ("matrix_ranking" . "") ("field_annotation" . ""))
                              (("field_name" . "crawling") ("form_name" . "form_3") ("section_header" . "") ("field_type" . "text") ("field_label" . "Crawling") ("select_choices_or_calculations" . "") ("field_note" . "")
                               ("text_validation_type_or_show_slider_number" . "crawling") ("text_validation_min" . "") ("text_validation_max" . "") ("identifier" . "") ("branching_logic" . "") ("required_field" . "")
                               ("custom_alignment" . "") ("question_number" . "") ("matrix_group_name" . "") ("matrix_ranking" . "") ("field_annotation" . ""))
                              (("field_name" . "postal_code") ("form_name" . "form_3") ("section_header" . "") ("field_type" . "text") ("field_label" . "Postal Code") ("select_choices_or_calculations" . "") ("field_note" . "")
                               ("text_validation_type_or_show_slider_number" . "postalcode_canada") ("text_validation_min" . "") ("text_validation_max" . "") ("identifier" . "") ("branching_logic" . "") ("required_field" . "")
                               ("custom_alignment" . "") ("question_number" . "") ("matrix_group_name" . "") ("matrix_ranking" . "") ("field_annotation" . ""))
                              (("field_name" . "email") ("form_name" . "form_3") ("section_header" . "") ("field_type" . "text") ("field_label" . "Email") ("select_choices_or_calculations" . "") ("field_note" . "")
                               ("text_validation_type_or_show_slider_number" . "email") ("text_validation_min" . "") ("text_validation_max" . "") ("identifier" . "") ("branching_logic" . "") ("required_field" . "")
                               ("custom_alignment" . "") ("question_number" . "") ("matrix_group_name" . "") ("matrix_ranking" . "") ("field_annotation" . ""))
                              (("field_name" . "truefalse") ("form_name" . "form_3") ("section_header" . "") ("field_type" . "truefalse") ("field_label" . "Vrai ou faux") ("select_choices_or_calculations" . "") ("field_note" . "")
                               ("text_validation_type_or_show_slider_number" . "") ("text_validation_min" . "") ("text_validation_max" . "") ("identifier" . "") ("branching_logic" . "") ("required_field" . "")
                               ("custom_alignment" . "RH") ("question_number" . "") ("matrix_group_name" . "") ("matrix_ranking" . "") ("field_annotation" . ""))
                              (("field_name" . "slider") ("form_name" . "form_3") ("section_header" . "") ("field_type" . "slider") ("field_label" . "Slider") ("select_choices_or_calculations" . "") ("field_note" . "")
                               ("text_validation_type_or_show_slider_number" . "") ("text_validation_min" . "") ("text_validation_max" . "") ("identifier" . "") ("branching_logic" . "") ("required_field" . "")
                               ("custom_alignment" . "RH") ("question_number" . "") ("matrix_group_name" . "") ("matrix_ranking" . "") ("field_annotation" . ""))))

(define redcap:testnum (string-append "<?xml version=\"1.0\" encoding=\"UTF-8\" ?>\n<records>\n"
"<item><field_name><![CDATA[num]]></field_name><form_name><![CDATA[form_1]]></form_name><section_header></section_header><field_type><![CDATA[text]]></field_type>"
"<field_label><![CDATA[This is an integer]]></field_label><select_choices_or_calculations></select_choices_or_calculations><field_note></field_note>"
"<text_validation_type_or_show_slider_number><![CDATA[integer]]></text_validation_type_or_show_slider_number><text_validation_min><![CDATA[0]]></text_validation_min><text_validation_max><![CDATA[10]]></text_validation_max>"
"<identifier></identifier><branching_logic></branching_logic><required_field></required_field><custom_alignment></custom_alignment><question_number></question_number>"
"<matrix_group_name></matrix_group_name><matrix_ranking></matrix_ranking><field_annotation></field_annotation></item>\n</records>\n"))

(define redcap:testfirst (string-append "field_name,form_name,section_header,field_type,field_label,select_choices_or_calculations,field_note,text_validation_type_or_show_slider_number,"
                                        "text_validation_min,text_validation_max,identifier,branching_logic,required_field,custom_alignment,question_number,matrix_group_name,matrix_ranking,field_annotation\n"
                                        "firstrow,form_2,\"Look, a matrix!\",radio,\"First row\",\"1, A | 2, B | 3, C\",,,,,,,y,,,mat,y,first!\n"))

(define redcap:testfields '("study_no" "yesno" "num" "form_1_complete" "drop" "radio" "checks___1" "checks___2" "checks___3" "checks___4" "firstrow" "secondrow" "form_2_complete" "date_dec" "date_inc_sec" "crawling" "postal_code" "email" "truefalse" "slider" "form_3_complete"))

;; Importing and exporting records functions
;;  - redcap-import-record
;;    - event
;;    - overwrite
;;    - instrument
;;    - instance
;;  - redcap-export-record
;;    - records
;;    - fields
;;    - events
;;    - filter
;;    - forms
;;  - redcap-export-ids
;;    - event
(unit-test "REDCap" "Records"
  (lambda ()
    (redcap-url-set! redcap:testurl)
    (let ((success #t))
      ;; Import json format
      (redcap-import-record redcap:testhost redcap:testtoken "1" redcap:testrecord1a 'event "event_a_arm_1")
      (redcap-import-record redcap:testhost redcap:testtoken "1" redcap:testrecord1b 'event "event_b_arm_1")
      (redcap-import-record redcap:testhost redcap:testtoken "1" redcap:testrecord1c 'event "event_b_arm_1")
      (redcap-import-record redcap:testhost redcap:testtoken "1" redcap:testrecord1d 'event "event_a_arm_1")
      ;; Import with overwrite turned on (by default), sets an existing value to blank
      (redcap-import-record redcap:testhost redcap:testtoken "1" redcap:testchange1a 'event "event_a_arm_1")
      ;; Import with overwrite turned off does not set an existing value to blank
      (redcap-import-record redcap:testhost redcap:testtoken "1" redcap:testchange1a 'event "event_b_arm_1" 'overwrite "normal")
      ;; Import csv with repeated events and instruments
      (redcap-import-record-csv redcap:testhost redcap:testtoken redcap:testrecord2)
      ;; Import to specific repeated instrument
      (redcap-import-record redcap:testhost redcap:testtoken "1" redcap:testrepeatable 'event "event_d_arm_1" 'instrument "form_2" 'instance "2")
      ;; Import to repeated event
      (redcap-import-record redcap:testhost redcap:testtoken "1" redcap:testrepeatable 'event "event_c_arm_1" 'instance "2")
      ;; Export full data as json (default)
      (set! success (test-success equal? (redcap-export-records redcap:testhost redcap:testtoken) redcap:testfullexport))
      ;; Export xml from a specific record
      (if success (set! success (test-success string=? (redcap-export-records redcap:testhost redcap:testtoken 'records (list "1") 'format "xml") redcap:testrecord1exp)))
      ;; Export specific field values as csv, specify name of set of checkboxes - only includes unique ID field + redcap_event_name if the unique ID is included
      (if success (set! success (test-success string=? (redcap-export-records redcap:testhost redcap:testtoken 'fields (list "study_no" "num" "radio" "checks") 'format "csv") redcap:testfieldsexp1)))
      (if success (set! success (test-success string=? (redcap-export-records redcap:testhost redcap:testtoken 'fields (list "num" "radio" "checks") 'format "csv") redcap:testfieldsexp2)))
      ;; Export from a specific event
      (if success (set! success (test-success equal? (redcap-export-records redcap:testhost redcap:testtoken 'events (list "event_b_arm_1")) redcap:testeventsexp)))
      ;; Export with filter
      (if success (set! success (test-success equal? (redcap-export-records redcap:testhost redcap:testtoken 'filter "[num] = 42") redcap:testfilterexp)))
      ;; Export from a specific form
      (if success (set! success (test-success equal? (redcap-export-records redcap:testhost redcap:testtoken 'records (list "2") 'forms (list "2")) redcap:testformsexp)))
      ;; Export ids
      (if success (set! success (test-success equal? (redcap-export-ids redcap:testhost redcap:testtoken) '("1" "1" "1" "1" "2"))))
      ;; Export ids given event
      (if success (set! success (test-success equal? (redcap-export-ids redcap:testhost redcap:testtoken 'event "event_a_arm_1") '("1" "2"))))
      ;; Get next available instance index
      (if success (set! success (test-success eq? (redcap-get-next-instance redcap:testhost redcap:testtoken "1" "2") 3)))
      (if success (set! success (test-success eq? (redcap-get-next-instance-index redcap:testhost redcap:testtoken "1" 'form "form_2" 'event "event_c_arm_1") 3)))
      (if success (set! success (test-success eq? (redcap-get-next-instance-index redcap:testhost redcap:testtoken "1" 'form "form_2" 'event "event_d_arm_1") 3)))
      (if success (set! success (test-success eq? (redcap-get-next-instance-index redcap:testhost redcap:testtoken "1" 'event "event_d_arm_1") 3)))
      (if success (set! success (test-success eq? (redcap-get-next-instance-index redcap:testhost redcap:testtoken "1" 'form "form_2") 3)))
      (if success (set! success (test-success eq? (redcap-get-next-instance-index redcap:testhost redcap:testtoken "1" 'form "form_3") #f)))
      (if success (set! success (test-success eq? (redcap-get-next-instance-index redcap:testhost redcap:testtoken "1" 'event "event_b_arm_1") #f)))
      success)))

(define redcap:testrecord1a '(("yesno" . "1") ("num" . "9") ("form_1_complete" . "1")))
(define redcap:testrecord1b '((checks 1 3)))
(define redcap:testrecord1c '(("yesno" "0") ("num" "8") ("form_1_complete" "2")
                              ("drop" "1") ("radio" "2") ("checks___1" "0") ("checks___2" "0") ("checks___4" "1") ("firstrow" "1") ("secondrow" "2") ("form_2_complete" "0")))
;; Date must be in YY-MM-DD format; date + time must be in YY-MM-DD HH:MM[:SS]; crawling must be integer; slider 0-100 by default; email, postal code have format restrictions
(define redcap:testrecord1d '(("date_dec" "96-06-16") ("date_inc_sec" "96-06-16 09:09:09") ("email" "test@mailinator.com") ("crawling" "0") ("postal_code" "v5v 5v5") ("slider" "80") ("truefalse" "1") ("form_3_complete" "2")))
(define redcap:testchange1a '(("yesno" "")))
(define redcap:testchange1b '(("drop" "")))
(define redcap:testrepeatable '(("firstrow" "2")))
(define redcap:testrecord2 (string-append "study_no,redcap_event_name,redcap_repeat_instrument,redcap_repeat_instance,yesno,num,form_1_complete,firstrow,form_2_complete\n"
                                          "1,event_d_arm_1,,,,,0,,\n"
                                          "1,event_c_arm_1,,1,1,,1,,0\n"
                                          "1,event_c_arm_1,,2,,,0,1,2\n"
                                          "1,event_d_arm_1,form_2,1,,,,2,1\n"
                                          "1,event_d_arm_1,form_2,2,,,,1,2\n"
                                          "2,event_a_arm_1,,,1,42,2,,"))

(define redcap:testfullexport '((("study_no" . "1") ("redcap_event_name" . "event_a_arm_1") ("redcap_repeat_instrument" . "") ("redcap_repeat_instance" . "") ("yesno" . "") ("num" . "9") ("form_1_complete" . "1")
                                  ("drop" . "") ("radio" . "") ("checks___1" . "") ("checks___2" . "") ("checks___3" . "") ("checks___4" . "") ("firstrow" . "") ("secondrow" . "") ("file" . "") ("form_2_complete" . "")
                                  ("date_dec" . "1996-06-16") ("date_inc_sec" . "1996-06-16 09:09:09") ("crawling" . "0") ("postal_code" . "v5v 5v5") ("email" . "test@mailinator.com") ("truefalse" . "1") ("slider" . "80") ("form_3_complete" . "2"))
                                (("study_no" . "1") ("redcap_event_name" . "event_b_arm_1") ("redcap_repeat_instrument" . "") ("redcap_repeat_instance" . "") ("yesno" . "0") ("num" . "8") ("form_1_complete" . "2")
                                  ("drop" . "1") ("radio" . "2") ("checks___1" . "0") ("checks___2" . "0") ("checks___3" . "1") ("checks___4" . "1") ("firstrow" . "1") ("secondrow" . "2") ("file" . "") ("form_2_complete" . "0")
                                  ("date_dec" . "") ("date_inc_sec" . "") ("crawling" . "") ("postal_code" . "") ("email" . "") ("truefalse" . "") ("slider" . "") ("form_3_complete" . ""))
                                (("study_no" . "1") ("redcap_event_name" . "event_d_arm_1") ("redcap_repeat_instrument" . "") ("redcap_repeat_instance" . "") ("yesno" . "") ("num" . "") ("form_1_complete" . "0")
                                  ("drop" . "") ("radio" . "") ("checks___1" . "") ("checks___2" . "") ("checks___3" . "") ("checks___4" . "") ("firstrow" . "") ("secondrow" . "") ("file" . "") ("form_2_complete" . "")
                                  ("date_dec" . "") ("date_inc_sec" . "") ("crawling" . "") ("postal_code" . "") ("email" . "") ("truefalse" . "") ("slider" . "") ("form_3_complete" . ""))
                                (("study_no" . "1") ("redcap_event_name" . "event_c_arm_1") ("redcap_repeat_instrument" . "") ("redcap_repeat_instance" . 1) ("yesno" . "1") ("num" . "") ("form_1_complete" . "1")
                                  ("drop" . "") ("radio" . "") ("checks___1" . "0") ("checks___2" . "0") ("checks___3" . "0") ("checks___4" . "0") ("firstrow" . "") ("secondrow" . "") ("file" . "") ("form_2_complete" . "0")
                                  ("date_dec" . "") ("date_inc_sec" . "") ("crawling" . "") ("postal_code" . "") ("email" . "") ("truefalse" . "") ("slider" . "") ("form_3_complete" . ""))
                                (("study_no" . "1") ("redcap_event_name" . "event_c_arm_1") ("redcap_repeat_instrument" . "") ("redcap_repeat_instance" . 2) ("yesno" . "") ("num" . "") ("form_1_complete" . "0")
                                  ("drop" . "") ("radio" . "") ("checks___1" . "0") ("checks___2" . "0") ("checks___3" . "0") ("checks___4" . "0") ("firstrow" . "2") ("secondrow" . "") ("file" . "") ("form_2_complete" . "2")
                                  ("date_dec" . "") ("date_inc_sec" . "") ("crawling" . "") ("postal_code" . "") ("email" . "") ("truefalse" . "") ("slider" . "") ("form_3_complete" . ""))
                                (("study_no" . "1") ("redcap_event_name" . "event_d_arm_1") ("redcap_repeat_instrument" . "form_2") ("redcap_repeat_instance" . 1) ("yesno" . "") ("num" . "") ("form_1_complete" . "")
                                  ("drop" . "") ("radio" . "") ("checks___1" . "0") ("checks___2" . "0") ("checks___3" . "0") ("checks___4" . "0") ("firstrow" . "2") ("secondrow" . "") ("file" . "") ("form_2_complete" . "1")
                                  ("date_dec" . "") ("date_inc_sec" . "") ("crawling" . "") ("postal_code" . "") ("email" . "") ("truefalse" . "") ("slider" . "") ("form_3_complete" . ""))
                                (("study_no" . "1") ("redcap_event_name" . "event_d_arm_1") ("redcap_repeat_instrument" . "form_2") ("redcap_repeat_instance" . 2) ("yesno" . "") ("num" . "") ("form_1_complete" . "")
                                  ("drop" . "") ("radio" . "") ("checks___1" . "0") ("checks___2" . "0") ("checks___3" . "0") ("checks___4" . "0") ("firstrow" . "2") ("secondrow" . "") ("file" . "") ("form_2_complete" . "2")
                                  ("date_dec" . "") ("date_inc_sec" . "") ("crawling" . "") ("postal_code" . "") ("email" . "") ("truefalse" . "") ("slider" . "") ("form_3_complete" . ""))
                                (("study_no" . "2") ("redcap_event_name" . "event_a_arm_1") ("redcap_repeat_instrument" . "") ("redcap_repeat_instance" . "") ("yesno" . "1") ("num" . "42") ("form_1_complete" . "2")
                                  ("drop" . "") ("radio" . "") ("checks___1" . "") ("checks___2" . "") ("checks___3" . "") ("checks___4" . "") ("firstrow" . "") ("secondrow" . "") ("file" . "") ("form_2_complete" . "")
                                  ("date_dec" . "") ("date_inc_sec" . "") ("crawling" . "") ("postal_code" . "") ("email" . "") ("truefalse" . "") ("slider" . "") ("form_3_complete" . "0"))))

(define redcap:testrecord1exp (string-append "<?xml version=\"1.0\" encoding=\"UTF-8\" ?>\n<records>\n"
                                             "<item><study_no><![CDATA[1]]></study_no><redcap_event_name><![CDATA[event_a_arm_1]]></redcap_event_name><redcap_repeat_instrument><![CDATA[]]></redcap_repeat_instrument>"
                                             "<redcap_repeat_instance><![CDATA[]]></redcap_repeat_instance><yesno><![CDATA[]]></yesno><num><![CDATA[9]]></num><form_1_complete><![CDATA[1]]></form_1_complete><drop><![CDATA[]]>"
                                             "</drop><radio><![CDATA[]]></radio><checks___1><![CDATA[]]></checks___1><checks___2><![CDATA[]]></checks___2><checks___3><![CDATA[]]></checks___3><checks___4><![CDATA[]]></checks___4>"
                                             "<firstrow><![CDATA[]]></firstrow><secondrow><![CDATA[]]></secondrow><file><![CDATA[]]></file><form_2_complete><![CDATA[]]></form_2_complete>"
                                             "<date_dec><![CDATA[1996-06-16]]></date_dec><date_inc_sec><![CDATA[1996-06-16 09:09:09]]></date_inc_sec><crawling><![CDATA[0]]></crawling><postal_code><![CDATA[v5v 5v5]]></postal_code>"
                                             "<email><![CDATA[test@mailinator.com]]></email><truefalse><![CDATA[1]]></truefalse><slider><![CDATA[80]]></slider><form_3_complete><![CDATA[2]]></form_3_complete></item>\n"

                                             "<item><study_no><![CDATA[1]]></study_no><redcap_event_name><![CDATA[event_b_arm_1]]></redcap_event_name><redcap_repeat_instrument><![CDATA[]]></redcap_repeat_instrument>"
                                             "<redcap_repeat_instance><![CDATA[]]></redcap_repeat_instance><yesno><![CDATA[0]]></yesno><num><![CDATA[8]]></num><form_1_complete><![CDATA[2]]></form_1_complete><drop><![CDATA[1]]>"
                                             "</drop><radio><![CDATA[2]]></radio><checks___1><![CDATA[0]]></checks___1><checks___2><![CDATA[0]]></checks___2><checks___3><![CDATA[1]]></checks___3><checks___4><![CDATA[1]]></checks___4>"
                                             "<firstrow><![CDATA[1]]></firstrow><secondrow><![CDATA[2]]></secondrow><file><![CDATA[]]></file><form_2_complete><![CDATA[0]]></form_2_complete>"
                                             "<date_dec><![CDATA[]]></date_dec><date_inc_sec><![CDATA[]]></date_inc_sec><crawling><![CDATA[]]></crawling><postal_code><![CDATA[]]></postal_code>"
                                             "<email><![CDATA[]]></email><truefalse><![CDATA[]]></truefalse><slider><![CDATA[]]></slider><form_3_complete><![CDATA[]]></form_3_complete></item>\n"

                                             "<item><study_no><![CDATA[1]]></study_no><redcap_event_name><![CDATA[event_d_arm_1]]></redcap_event_name><redcap_repeat_instrument><![CDATA[]]></redcap_repeat_instrument>"
                                             "<redcap_repeat_instance><![CDATA[]]></redcap_repeat_instance><yesno><![CDATA[]]></yesno><num><![CDATA[]]></num><form_1_complete><![CDATA[0]]></form_1_complete><drop><![CDATA[]]>"
                                             "</drop><radio><![CDATA[]]></radio><checks___1><![CDATA[]]></checks___1><checks___2><![CDATA[]]></checks___2><checks___3><![CDATA[]]></checks___3><checks___4><![CDATA[]]></checks___4>"
                                             "<firstrow><![CDATA[]]></firstrow><secondrow><![CDATA[]]></secondrow><file><![CDATA[]]></file><form_2_complete><![CDATA[]]></form_2_complete>"
                                             "<date_dec><![CDATA[]]></date_dec><date_inc_sec><![CDATA[]]></date_inc_sec><crawling><![CDATA[]]></crawling><postal_code><![CDATA[]]></postal_code>"
                                             "<email><![CDATA[]]></email><truefalse><![CDATA[]]></truefalse><slider><![CDATA[]]></slider><form_3_complete><![CDATA[]]></form_3_complete></item>\n"

                                             "<item><study_no><![CDATA[1]]></study_no><redcap_event_name><![CDATA[event_c_arm_1]]></redcap_event_name><redcap_repeat_instrument><![CDATA[]]></redcap_repeat_instrument>"
                                             "<redcap_repeat_instance><![CDATA[1]]></redcap_repeat_instance><yesno><![CDATA[1]]></yesno><num><![CDATA[]]></num><form_1_complete><![CDATA[1]]></form_1_complete><drop><![CDATA[]]>"
                                             "</drop><radio><![CDATA[]]></radio><checks___1><![CDATA[0]]></checks___1><checks___2><![CDATA[0]]></checks___2><checks___3><![CDATA[0]]></checks___3><checks___4><![CDATA[0]]></checks___4>"
                                             "<firstrow><![CDATA[]]></firstrow><secondrow><![CDATA[]]></secondrow><file><![CDATA[]]></file><form_2_complete><![CDATA[0]]></form_2_complete>"
                                             "<date_dec><![CDATA[]]></date_dec><date_inc_sec><![CDATA[]]></date_inc_sec><crawling><![CDATA[]]></crawling><postal_code><![CDATA[]]></postal_code>"
                                             "<email><![CDATA[]]></email><truefalse><![CDATA[]]></truefalse><slider><![CDATA[]]></slider><form_3_complete><![CDATA[]]></form_3_complete></item>\n"

                                             "<item><study_no><![CDATA[1]]></study_no><redcap_event_name><![CDATA[event_c_arm_1]]></redcap_event_name><redcap_repeat_instrument><![CDATA[]]></redcap_repeat_instrument>"
                                             "<redcap_repeat_instance><![CDATA[2]]></redcap_repeat_instance><yesno><![CDATA[]]></yesno><num><![CDATA[]]></num><form_1_complete><![CDATA[0]]></form_1_complete><drop><![CDATA[]]>"
                                             "</drop><radio><![CDATA[]]></radio><checks___1><![CDATA[0]]></checks___1><checks___2><![CDATA[0]]></checks___2><checks___3><![CDATA[0]]></checks___3><checks___4><![CDATA[0]]></checks___4>"
                                             "<firstrow><![CDATA[2]]></firstrow><secondrow><![CDATA[]]></secondrow><file><![CDATA[]]></file><form_2_complete><![CDATA[2]]></form_2_complete>"
                                             "<date_dec><![CDATA[]]></date_dec><date_inc_sec><![CDATA[]]></date_inc_sec><crawling><![CDATA[]]></crawling><postal_code><![CDATA[]]></postal_code>"
                                             "<email><![CDATA[]]></email><truefalse><![CDATA[]]></truefalse><slider><![CDATA[]]></slider><form_3_complete><![CDATA[]]></form_3_complete></item>\n"

                                             "<item><study_no><![CDATA[1]]></study_no><redcap_event_name><![CDATA[event_d_arm_1]]></redcap_event_name><redcap_repeat_instrument><![CDATA[form_2]]></redcap_repeat_instrument>"
                                             "<redcap_repeat_instance><![CDATA[1]]></redcap_repeat_instance><yesno><![CDATA[]]></yesno><num><![CDATA[]]></num><form_1_complete><![CDATA[]]></form_1_complete><drop><![CDATA[]]>"
                                             "</drop><radio><![CDATA[]]></radio><checks___1><![CDATA[0]]></checks___1><checks___2><![CDATA[0]]></checks___2><checks___3><![CDATA[0]]></checks___3><checks___4><![CDATA[0]]></checks___4>"
                                             "<firstrow><![CDATA[2]]></firstrow><secondrow><![CDATA[]]></secondrow><file><![CDATA[]]></file><form_2_complete><![CDATA[1]]></form_2_complete>"
                                             "<date_dec><![CDATA[]]></date_dec><date_inc_sec><![CDATA[]]></date_inc_sec><crawling><![CDATA[]]></crawling><postal_code><![CDATA[]]></postal_code>"
                                             "<email><![CDATA[]]></email><truefalse><![CDATA[]]></truefalse><slider><![CDATA[]]></slider><form_3_complete><![CDATA[]]></form_3_complete></item>\n"

                                             "<item><study_no><![CDATA[1]]></study_no><redcap_event_name><![CDATA[event_d_arm_1]]></redcap_event_name><redcap_repeat_instrument><![CDATA[form_2]]></redcap_repeat_instrument>"
                                             "<redcap_repeat_instance><![CDATA[2]]></redcap_repeat_instance><yesno><![CDATA[]]></yesno><num><![CDATA[]]></num><form_1_complete><![CDATA[]]></form_1_complete><drop><![CDATA[]]>"
                                             "</drop><radio><![CDATA[]]></radio><checks___1><![CDATA[0]]></checks___1><checks___2><![CDATA[0]]></checks___2><checks___3><![CDATA[0]]></checks___3><checks___4><![CDATA[0]]></checks___4>"
                                             "<firstrow><![CDATA[2]]></firstrow><secondrow><![CDATA[]]></secondrow><file><![CDATA[]]></file><form_2_complete><![CDATA[2]]></form_2_complete>"
                                             "<date_dec><![CDATA[]]></date_dec><date_inc_sec><![CDATA[]]></date_inc_sec><crawling><![CDATA[]]></crawling><postal_code><![CDATA[]]></postal_code>"
                                             "<email><![CDATA[]]></email><truefalse><![CDATA[]]></truefalse><slider><![CDATA[]]></slider><form_3_complete><![CDATA[]]></form_3_complete></item>\n"
                                             "</records>"))

(define redcap:testfieldsexp1 (string-append "study_no,redcap_event_name,redcap_repeat_instrument,redcap_repeat_instance,num,radio,checks___1,checks___2,checks___3,checks___4\n"
                                             "1,event_a_arm_1,,,9,,,,,\n"
                                             "1,event_b_arm_1,,,8,2,0,0,1,1\n"
                                             "1,event_d_arm_1,,,,,,,,\n"
                                             "1,event_c_arm_1,,1,,,0,0,0,0\n"
                                             "2,event_a_arm_1,,,42,,,,,\n"))

(define redcap:testfieldsexp2 (string-append "num,radio,checks___1,checks___2,checks___3,checks___4\n"
                                            "9,,,,,\n"
                                            "8,2,0,0,1,1\n"
                                             ",,,,,\n"
                                             ",,0,0,0,0\n"
                                            "42,,,,,\n"))

(define redcap:testformsexp '((("study_no" . "2") ("redcap_event_name" . "event_a_arm_1") ("redcap_repeat_instrument" . "") ("redcap_repeat_instance" . "") ("yesno" . "1") ("num" . "42") ("form_1_complete" . "2")
                               ("drop" . "") ("radio" . "") ("checks___1" . "") ("checks___2" . "") ("checks___3" . "") ("checks___4" . "") ("firstrow" . "") ("secondrow" . "") ("file" . "") ("form_2_complete" . "")
                               ("date_dec" . "") ("date_inc_sec" . "") ("crawling" . "") ("postal_code" . "") ("email" . "") ("truefalse" . "") ("slider" . "") ("form_3_complete" . "0"))))

(define redcap:testeventsexp '((("study_no" . "1") ("redcap_event_name" . "event_b_arm_1") ("redcap_repeat_instrument" . "") ("redcap_repeat_instance" . "") ("yesno" . "0") ("num" . "8") ("form_1_complete" . "2")
                                ("drop" . "1") ("radio" . "2") ("checks___1" . "0") ("checks___2" . "0") ("checks___3" . "1") ("checks___4" . "1") ("firstrow" . "1") ("secondrow" . "2") ("file" . "") ("form_2_complete" . "0")
                                ("date_dec" . "") ("date_inc_sec" . "") ("crawling" . "") ("postal_code" . "") ("email" . "") ("truefalse" . "") ("slider" . "") ("form_3_complete" . ""))))

(define redcap:testfilterexp '((("study_no" . "2") ("redcap_event_name" . "event_a_arm_1") ("redcap_repeat_instrument" . "") ("redcap_repeat_instance" . "") ("yesno" . "1") ("num" . "42") ("form_1_complete" . "2")
                                ("drop" . "") ("radio" . "") ("checks___1" . "") ("checks___2" . "") ("checks___3" . "") ("checks___4" . "") ("firstrow" . "") ("secondrow" . "") ("file" . "") ("form_2_complete" . "")
                                ("date_dec" . "") ("date_inc_sec" . "") ("crawling" . "") ("postal_code" . "") ("email" . "") ("truefalse" . "") ("slider" . "") ("form_3_complete" . "0"))))

;; Importing and exporting files functions
;;  - redcap-import-file
;;    - event
;;    - repeat
;; - redcap-export-file
;;    - event
;;    - repeat
;; - redcap-delete-file
;;    - event
;;    - repeat
(unit-test "REDCap" "Files"
  (lambda () (and
    (redcap:filetest "event_b_arm_1" #f)
    (redcap:filetest "event_c_arm_1" "1")
    (redcap:filetest "event_d_arm_1" "2"))))

(define (redcap:filetest event repeat)
    (redcap-url-set! redcap:testurl)
    (let ((testfile (string-append (system-directory) (system-pathseparator) "test.csv"))
          (csvcontent (list (list "header1" "header2") (list "A" "B")))
          (success #t))
      ;; Remove the file if exists
      (if (file-exists? testfile) (delete-file testfile))
      ;; Create a small csv file
      (csv-write testfile csvcontent)
      ;; Import the file
    (redcap-import-file redcap:testhost redcap:testtoken "1" "file" testfile 'event event 'repeat repeat)
      ;; Export the file
    (let ((f (redcap-export-file redcap:testhost redcap:testtoken "1" "file" testfile 'event event 'repeat repeat)))
        (if (list? f)
          (let* ((filepath (string-append (system-directory) (system-pathseparator) "export.csv"))
                 (fh (open-output-file filepath))
                 (content (cadr f)))
             ;; Check file name is list
             (if success (set! success (string-contains (car f) "name=\"test.csv\"")))
             ;; If file exists, delete it
             (if (file-exists? filepath) (delete-file testfile))
             (write-subu8vector content 0 (u8vector-length content) fh)
             (close-output-port fh)
            ;; Check exported file
            (let ((downloadedcontent (csv-read filepath)))
              (if success (set! success (equal? csvcontent downloadedcontent)))))
          (set! success #f)))
      ;; Check the a regular export shows a file
    (if success (set! success (test-success string-contains (redcap-export-records redcap:testhost redcap:testtoken 'records (list "1") 'events (list event) 'fields (list "file") 'format "csv") "[document]")))
      ;; Delete file to prepare for next time
    (redcap-delete-file redcap:testhost redcap:testtoken "1" "file" 'event event 'repeat repeat)
      ;; Check that the file is gone
    (if success (set! success (test-failure string-contains (redcap-export-records redcap:testhost redcap:testtoken 'records (list "1") 'events (list event) 'fields (list "file") 'format "csv") "[document]")))
    success))
