
; optimized (string-rindex name #\:)
; returns position of a separator between namespace-id and LocalName
(define-macro (sxml:find-name-separator len)
  `(let rpt ((pos (-- ,len))) 
     (cond
       ((negative? pos) #f) 	
       ((char=? #\: (string-ref name pos)) pos)
       (else (rpt (-- pos))))))
