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

;; DemoSerial - A serial terminal emulation project.
(define gui #f)
(define buf "")
(define serialport #f)
(define serial:last-checked 0.)

;; Determines the port Id of the USB serial cord currently in use on a mac.
(define (detect-mac-usb-serial)
  (let loop ((files (directory-files "/dev")))
    (if (fx= (length files) 0) 
      ""
      (let ((file (car files)))
        (if (and (fx> (string-length file) 14) 
                 (string=? (substring file 0 14) "tty.usbserial-")) 
            (string-append "/dev/" file)
            (loop (cdr files))
        ))
    )
  ))

(define (open-callback g w t x y)
  (let* ((devname (cond 
           ((string=? (system-platform) "macosx") (detect-mac-usb-serial))
           ((string=? (system-platform) "linux") "/dev/ttyUSB0")
           ((string=? (system-platform) "win32") "COM1")
           ((string=? (system-platform) "android") "0")
           (else "/dev/tty.iap") ;; Works only on jailbroken iOS devices
         ))
         (baudrate (list-ref serial:ratelist (glgui-widget-get gui speed-list 'current)))
         (databits 8)
         (parity 0)
         (stopbits 1)
         (newdev (serial-try devname baudrate databits parity stopbits #f)))
    (if newdev 
      (begin 
        (glgui-widget-set! g w 'color DarkGreen)
        (glgui-widget-set! g close-button 'color White)
        (set! serialport newdev)
        (serial-cache-setup serialport (lambda (l) (and (> l 0) (not (= l 13)))) #x0D);
      )
      (glgui-widget-set! g w 'color Red)
    )
  )
)
(define (close-callback g w t x y)
  (if serialport 
    (begin 
      (serial-close serialport)
      (glgui-widget-set! g w 'color DarkGreen)
    )
    (glgui-widget-set! g w 'color DarkGrey)
  )
  (glgui-widget-set! g open-button 'color White)
)
(define (speed-list-callback g w t x y)
 ;;This is needed to update the current selection
 #f
)
(define (build-speed-list)
  (let loop ((i 0) (result (list)))
    (if (= i (length serial:ratelist))
       result
       (loop (+ i 1) (append result (list (speed-list-element (list-ref serial:ratelist i) i))))
    )
  )
)
(define (speed-list-element entry pos)
  (lambda (g wgt x y w h s)
    (glgui:draw-text-left (+ x 5) y 50 16 (number->string entry) ascii_16.fnt (if (= pos (glgui-widget-get gui speed-list 'current)) Blue DarkGray))
  )
)

;; Main loop functions
(main
;; initialization
  (lambda (w h)
    (let ((w 320)
          (h 480))
      (make-window w h)
      (glgui-orientation-set! GUI_PORTRAIT)
      (set! gui (make-glgui))
      ;; Menubar
      (glgui-menubar gui 0 (- h 44) w 44)
      (glgui-pixmap  gui 8 (- h 36) title.img)
      ;; Controls
      (set! speed-list
        (glgui-list gui 5 (- h 44 33) (/ w 3) 32 16 (build-speed-list) speed-list-callback)
      )
      (glgui-widget-set! gui speed-list 'current 0)
      (set! open-button
        (glgui-button-string gui (+ 5 (/ w 3) 5) (- h 44 25) 75 25 "Open" ascii_16.fnt open-callback)
      )
      (set! close-button
        (glgui-button-string gui (+ 5 (/ w 3) 5 80) (- h 44 25) 75 25 "Close" ascii_16.fnt close-callback)
      )
      (glgui-widget-set! gui close-button 'color DarkGray)
      ;;Chat List
      (set! chat-list 
        (glgui-chat gui 5 (+ (/ w 1.5) 5 20 2) (- w 10) 163 16 (list) ascii_16.fnt #f)
      )
      (make-store "main")
      ;;Keypad
      (set! message-string 
        (glgui-label gui 5 (+ (/ w 1.5) 5) (- w 10) 20 "" ascii_16.fnt White (color-shade White 0.1))
      )
      (glgui-widget-set! gui message-string 'align GUI_ALIGNRIGHT)
      (glgui-keypad gui 0 0 (glgui-width-get) 160 ascii_32.fnt)
      ;; I need logfiles
      (if (not (file-exists? log:path)) (create-directory log:path))
    )
  )
;; events
  (lambda (t x y) 
    (if (= t EVENT_KEYPRESS) 
      (begin
        (cond
          ((= x EVENT_KEYESCAPE) 
            (terminate)
          )
          ((and (>= x 32) (< x 127))
            (set! buf (string-append buf (string (integer->char x))))
          )
          ((= x 3) ;; This is backspace
            (if (> (string-length buf) 0) (set! buf (substring buf 0 (- (string-length buf) 1))))
          )
          ((= x 1) ;; This is return
            (if (and (> (string-length buf) 0) serialport) 
              (begin
                (glgui-widget-set! gui chat-list 'list (append (list (list (floor ##now) "Out" buf 1)) (glgui-widget-get "main" chat-list 'list)))
                (set! buf (string-append buf "\n"))
                (let loop ((m (u8vector->list (string->u8vector buf)))(err #f))
                  (if err 
                    (begin (set! serialport #f) (glgui-widget-set! gui open-button 'color Red))
                    (if (fx> (length m) 0) (begin
                        (serial-writechar serialport (car m))
                        (loop (cdr m) (or (serial-error) (serial-timeout)))
                    ))
                  )
                )
              )
            )
            (set! buf "")
          )
        )
        (glgui-widget-set! gui message-string 'label buf)
      )
    )
    ;; See if we got serial data
    (if (and serialport (fl> (fl- ##now serial:last-checked) 0.01))
      (let ((data (serial-cache-read serialport)))
        (if (and data (fx> (string-length data) 0))
          (glgui-widget-set! gui chat-list 'list (append (list (list (floor ##now) "In" data 0)) (glgui-widget-get "main" chat-list 'list)))
        )
        (set! serial:last-checked ##now)
      )
    )
    ;; Sleep and plot
    (glgui-event gui t x y)
    (thread-sleep! 0.01)
  )
;; termination
  (lambda () 
    (if serialport (serial-close serialport))
    #t
  )
;; suspend
  (lambda () (glgui-suspend))
;; resume
  (lambda () (glgui-resume))
)
;;eof
