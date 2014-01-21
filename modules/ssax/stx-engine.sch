
; A helper for stx:xsl->stx 
(define-macro stx:call-function 
  (lambda (name type tpl-node $-env)
  `(let ((fn (,$-env 
	      (string->symbol ,name))))
    (if 
      (eq? fn '*LT-NOT-FOUND*)
      (apply stx:error 
	     (append 
	      (list "Undefined " ,type  " with name " ,name " is called by:" nl)
	       (sxml:clean-feed (sxml:sxml->xml ',tpl-node))
	       (list nl "Valid names: ") (map car (,$-env))
				))
      (call-with-err-handler
	(lambda()
	  (sxml:clean-feed
 	      (fn current-node stx:templates current-root (,$-env '*LT-ADD* 
						      `(stx:param ,',tpl-node)))))
	(lambda (mes)
	  (apply stx:error 
		 (list ,type " evaluation ERROR" 
		   nl mes nl "for:" nl
		   (sxml:clean-feed 
		     (sxml:sxml->xml ',tpl-node))))))))))
