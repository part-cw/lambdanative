(define intValue (method "intValue" "java.lang.Double"))  ;; FIXME teach jscheme fixnums!

;; Try this to find out how where methods are defined:
;;
(procedure? (method "checkOrRequestPermission" (android-app-class) "java.lang.String"))

;; Just to see an error:
;;
(error "nananana")
