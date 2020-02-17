#|
LambdaNative - a cross-platform Scheme framework
Copyright (c) 2009-2016, University of British Columbia
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

;; basic hybrid app example

(define index-js
#<<EOJS

// Send HTTP GET to back end
function get_async(url,fun) {
  var xhr = new XMLHttpRequest();
  xhr.onreadystatechange = function() {
    if (xhr.readyState == XMLHttpRequest.DONE) {
      if (xhr.status == 200) {
        fun(decodeURI(xhr.responseText));
      } else {
        console.log('** get_async FAILED, status=' + xhr.status + ' [' + xhr.statusText + ']');
      }
    }
  }
  xhr.open('GET', url, true);
  xhr.send(null);
};

// Ask the Scheme backend to calculate the inner rectangle dimensions
function calculateRect() {
  // Get width, height, and percent from the fields
  var w = document.getElementById("width").value;
  var h = document.getElementById("height").value;
  var p = document.getElementById("percent").value;
  var ajax_cmd = "calculate-rect.cgi?w=" + w + "&h=" + h + "&p=" + p;
  console.log(ajax_cmd);
  get_async(ajax_cmd, function(data) {
    var resultp = document.getElementById("result");
    if (data.localeCompare("MISSING_PARAMS") == 0) {
      resultp.innerHTML = "Error, something went wrong. Missing parameters!";
    } else if (data.localeCompare("NOT_NUMBERS") == 0) {
      resultp.innerHTML = "Error. Please enter a number in each box for width, height, and percent of area."
    } else if (data.localeCompare("NOT_POSITIVE") == 0) {
      resultp.innerHTML = "Error. All values must be positive (> 0)."
    } else {
      var returned_data = false;
      try { returned_data = JSON.parse(data); } catch(err) { console.log("error calculating") };
      if (returned_data) {
        var area = returned_data["area"];
        var narea = returned_data["narea"];
        var new_w = returned_data["neww"];
        var new_h = returned_data["newh"];
        resultp.innerHTML = "The original rectangle has an area of " + area + ". The new one has an area of " + narea +
                            ", " + p + "% of the original. The new rectangle has <b>width " + new_w + "</b> and <b>height " + new_h + "</b>.";
      }
    }
  });
}

EOJS
)

;; Defining CSS
(define (css f) `(link (@ (href ,f) (rel "stylesheet"))))

;; Basic webpage
(website-addhook #f "/index.html"
  (lambda (args) 
     (string->u8vector (with-output-to-string "" 
       (lambda () (sxml->xml
         `(html (head ,(css "css/style.css"))
           (body (h1 "Hello from LambdaNative")
            (p "This is HTML served from a hybrid app.")
            (p "It contains an example of communication between the front end HTML, Javascript and back end Scheme.")
            (br)
            (p "Given a rectangle, the below calculator finds the width and height of a rectangle that takes up a given percentage of the area of the original (that can be centered within it, with the same border all the way around). While unnecessary, the calculations are done in Scheme.")
            ;; Form calls Javascript function when Calculate button pressed
            ;; Return false in order to not do any other action from the form submit
            (form (@ (onsubmit "calculateRect(); return false;"))
              (table
                (tr
                  (th "Width:")
                  (th "Height:")
                  (th "Percent of area:"))
                (tr
                  (td
                    (input (@ (id "width")
                              (type "number")) ""))
                  (td
                    (input (@ (id "height")
                              (type "number")) ""))
                  (td
                    (input (@ (id "percent")
                              (type "number")) ""))))
              (br)
              (br)
              (input (@ (type "submit")
                        (value "Calculate"))))
            ;; Result will be shown here
            (p (@ (id "result")) "")
            ;; Include Javascript here, inside the body
            (script (@ (type "text/javascript")) ,index-js)))))))))

(website-serve #f 8080)

;; Back-end respond to GET here
(website-addhook #f "/calculate-rect.cgi" (lambda (x)
  (let* ((qstring (assoc "QUERY_STRING" x))
         (params (getargs->list (if qstring (cadr qstring) '()))))
     (if params
       (let ((wentry (assoc "w" params))
             (hentry (assoc "h" params))
             (pentry (assoc "p" params)))
         (if (and wentry hentry pentry)
           (let ((w (string->number (cadr wentry)))
                 (h (string->number (cadr hentry)))
                 (p (string->number (cadr pentry))))
             (if (and w p h)
               (if (and (> w 0) (> h 0) (> p 0))
                 ;; Calculate the new rectangle dimensions and return them
                 ;; in JSON format converted to a u8vector
                 (string->u8vector (json-encode (calculate-rect w h p)))
                 ;; Return error because one of the numbers is 0 or negative
                 (string->u8vector "NOT_POSITIVE"))
               ;; Return error if converting to numbers failed
               (string->u8vector "NOT_NUMBERS")))
           ;; Return error if for some reason any of the parameters are missing
           (string->u8vector "MISSING_PARAMS")))
       (string->u8vector "MISSING_PARAMS")))))

;; Given a rectangle, this procedure calculates the width and height of a rectangle that takes up a
;; given percentage of the area of the original (that can be centered within it,
;; with the same border all the way around).
;; w is the width of the original rectangle
;; h is the hieght of the original rectangle
;; p is the percent of area to use for the new rectangle
;; This procedure returns a list or pairs with values for the neww and newh
(define (calculate-rect w h p)
  (let* ((area (* w h))
         ;; Calculate new (target) area
         (narea (* area (/ p 100)))
         (diff (- h w))
         ;; Use quadratic formula with a= 1, b = diff, and c = new area
         (nw (abs (/ (+ (* diff -1) (sqrt (- (* diff diff) (* -4 narea)))) 2)))
         (nh (abs (/ (- (* diff -1) (sqrt (- (* diff diff) (* -4 narea)))) 2))))
    (list (cons "area" (float->string area 2)) (cons "narea" (float->string narea 2))
          (cons "neww" (float->string nw 2)) (cons "newh" (float->string nh 2))))
)

;; Convert a string of arguments into a list of lists
(define (getargs->list str)
  (if (string? str)
    (let ((tmp (string-split str #\&)))
      (map (lambda (s)
             (let ((sp (string-split s #\=)))
               (if (fx= (length sp) 1)
                 (append sp (list ""))
                 sp)))
           tmp))
  '()))

;; eof
