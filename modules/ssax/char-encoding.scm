
;;(include "../libs/gambit/myenv.sch")
;;(include "../libs/gambit/common.sch")

; Top-level cond-expand expanded automatically
(define ascii->char integer->char)


;	ucscode->char INT -> CHAR
; Return a character whose UCS (ISO/IEC 10646) code is INT
; Note
; This function is required for processing of XML character entities:
; According to Section "4.1 Character and Entity References"
; of the XML Recommendation:
;  "[Definition: A character reference refers to a specific character
;   in the ISO/IEC 10646 character set, for example one not directly
;   accessible from available input devices.]"

(define (ucscode->char code)
  (cond-expand
    (bigloo
      (ucs2->char (integer->ucs2 code)))
    ((or scheme48 scsh)			; Scheme48 has no support for UCS
      (ascii->char code))
    (else
      (integer->char code))))

; Commonly used control characters

(define char-return (ascii->char 13))
(define char-tab    (ascii->char 9))
(define char-newline (ascii->char 10)) ; a.k.a. #\newline, per R5RS
(define char-space (ascii->char 32))
