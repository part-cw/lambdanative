#|
LambdaNative - a cross-platform Scheme framework
Copyright (c) 2009-2018, University of British Columbia
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
;; UIForm helloworld example

(define demouiform:example (list->table `(
  (main
    "Demo UI Form"
    ("Widgets" widgets)
    ("About" about)
    (spacer height 200)
    (label text "Hello from LambdaNative" size header)
    (spacer)
    ;; Show an image (file listed in EMBED)
    (image file "LN_logo.png")
  )
  (widgets
   "Widgets"
   #f
   ("Done" main)
   (spacer)
   ;; Make a text field where the entered number will be stored in key "someid" in the database, indent it on both sides
   (textentry id someid text "Number here:" keypad numint indent 0.4 indentright 0.4)
   (spacer height 10)
   ;; Don't store the password in the database table, just store it in the UI
   (textentry id passid text "Password:" password #t location ui)
   (spacer)
   ;; Simple radio box with a callback popup from one of the options.
   (radiobox id aradio text "Pick Yes" left ("Yes" "1" #f) right ("No" "0" ("I said choose yes!" ("OK" ,(lambda ()
                                                                                                         ;; Don't allow No, set it to blank
                                                                                                         (dbclear 'aradio)
                                                                                                         ;; Don't go to another page
                                                                                                         #f)))))
   ;; Checkbox that appears only if above radiobox is Yes
   ,(lambda ()
      (if (string=? (dbget 'aradio "") "1")
        '(checkbox id checky indent 0.3 text "Visible if you said Yes")
        '(spacer height 0)))
   (spacer)
   ;; Dropdown
   (dropdown id dropcolours label "Pick an option:" entries ("Option 1" "Choice B" "Alternative iii"))
   (spacer)
   ;; Button with action callback that returns the page you want to go to
   (button h 75 size header indent 0.05 rounded #t text "Go Back" action ,(lambda () 'main))
   (spacer)
    ;; slider 
   (slider id sliderval number #t min 0 max 100 default 50 labels ("min" "max"))
   (spacer)
   (label text "For more features that you can include in a script see the LNhealth app \"Demo Widgets\". Look at the main.sx file in the sandbox folder which is located at https://github.com/part-cw/LNhealth/tree/master/apps/WidgetDemo" align left size small)
  )
  (about
   "About"
   ("Back" main)
   #f
   (spacer height 50)
   (label text "This is a demo app of the uiform module in LambdaNative, a cross-platform development environment written in Scheme. See lambdanative.org")
   (spacer)
   (label text "Copyright (c) 2009-2018\nUniversity of British Columbia")
  )
 )))

(define gui #f)
(define form #f)

(main
;; initialization
  (lambda (w h)
    (make-window 480 800)
    (glgui-orientation-set! GUI_PORTRAIT)
    (set! gui (make-glgui))

    (let ((aw (glgui-width-get))
          (ah (glgui-height-get)))
      (glgui-box gui 0 0 aw ah DarkGreen)
      (set! form (glgui-uiform gui 0 0 aw ah)))

    ;; Set the sandbox up to be the current directory and use the above example as the script
    (glgui-widget-set! gui form 'sandbox (system-directory))
    (glgui-widget-set! gui form 'uiform demouiform:example)

    ;; Set the fonts
    (glgui-widget-set! gui form 'fnt ascii_18.fnt)
    (glgui-widget-set! gui form 'smlfnt ascii_14.fnt)
    (glgui-widget-set! gui form 'hdfnt ascii_24.fnt)
    (glgui-widget-set! gui form 'bigfnt ascii_40.fnt)

    ;; Create the table to store data (default location for widget values)
    (glgui-widget-set! gui form 'database (make-table))

  )
;; events
  (lambda (t x y)
    (if (= t EVENT_KEYPRESS) (begin
      (if (= x EVENT_KEYESCAPE) (terminate))))
    (glgui-event gui t x y))
;; termination
  (lambda () #t)
;; suspend
  (lambda () (glgui-suspend))
;; resume
  (lambda () (glgui-resume))
)

;; eof
