;; Lambdanative eventloop template
;; this is for hybrid console applications with an event loop

(main
;; initialization
  (lambda (w h)
     (scheduler-init)
  )
;; events
  (lambda (t x y) 
     (scheduler-iterate)
     (thread-sleep! 1e-3)
  )
;; termination
  (lambda () (scheduler-cleanup))
)

;; eof
