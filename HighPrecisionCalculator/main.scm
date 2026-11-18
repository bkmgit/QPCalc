#|
LambdaNative - a cross-platform Scheme framework
Copyright (c) 2009-2013, University of British Columbia
Copyright (c) 2026, Benson Muite
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

;; This calculator app uses the endlessbits library

(define gui #f)
(define calculator-display-num1 #f)
(define calculator-display-operation #f)
(define calculator-display-num2 #f)
(define calculator-display-answer #f)


(define square-width 40)
(define square-border 2)
(define inner-square-width (- square-width (* square-border 2)))

(define keypad-button-symbols
  `( "7" "8" "9" "/"
     "4" "5" "6" "*"
     "1" "2" "3" "-"
     "0" "." "=" "+"))

(define calculator-operations `("/" "*" "-" "+"))

(define calculator-digits `("0" "1" "2" "3" "4" "5" "6" "7" "8" "9"))

(define keypad-buttons
  `(#f #f #f #f
    #f #f #f #f
    #f #f #f #f
    #f #f #f #f))

(define answer #f)
(define operation #f)
(define val1 "")
(define val2 "")

(define (xind button-number)
  (fxremainder button-number 4))

(define (yind button-number)
  (fxquotient button-number 4))

(define (get-button-x x)
  (- (fxquotient x square-width) 4))

(define (get-button-y y)
  (- 11 (fxquotient y square-width) 1))

(define (button-callback gui b t x y)
  (set! button-number (+ (get-button-x x)
                         (* (get-button-y y) 4)))
  (calculator-input (list-ref keypad-button-symbols button-number)))

(define (create-button button-number)
  (list-set! keypad-buttons button-number
             (glgui-button-string gui
                                  (+ (* (xind button-number) square-width) square-border)
                                  (- (glgui-height-get) (+ 120 (* square-width (yind button-number))))
                                  inner-square-width
                                  inner-square-width
                                  (list-ref keypad-button-symbols (+ (xind button-number)  (* (yind button-number) 4)))
                                  calc_14.fnt   
				  button-callback)))


(define (calculator-input in)
  (cond
    ((member in calculator-operations)
     (if (not operation)
       (begin
	 (set! operation in)
	 (glgui-widget-set! gui calculator-display-operation 'label in)
         (glgui-widget-set! gui calculator-display-num1 'focus #f)
         (glgui-widget-set! gui calculator-display-num2 'focus #t))))
    ((member in `("="))
     (if (and (not (eq? operation #f)) (not (eq? (string->list val1) `())) (not (eq? (string->list val2) `())))
       (begin
         (glgui-widget-set! gui calculator-display-num2 'focus #f)
         (glgui-widget-set! gui calculator-display-answer 'label "getting answer"))))
    ((member in calculator-digits) 
     (if (not operation)
       (begin
         (set! val1 (string-append val1 in))
	 (glgui-widget-set! gui calculator-display-num1 'label val1))
       (begin
         (set! val2 (string-append val2 in))
         (glgui-widget-set! gui calculator-display-num2 'label val2))))
    ((member in `("."))
     (if (not operation)
       (if (not (member #\. (string->list val1)))
         (begin
           (set! val1 (string-append val1 in))
	   (glgui-widget-set! gui calculator-display-num1 'label val1)))
       (if (not (member #\. (string->list val2)))
         (begin
           (set! val2 (string-append val2 in))
	   (glgui-widget-set! gui calculator-display-num2 'label val2))))) ))


;; ------------------

(main
;; initialization
  (lambda (w h)
    (make-window 320 480)
    (glgui-orientation-set! GUI_PORTRAIT)
    (set! gui (make-glgui))
    (let* ((w (glgui-width-get))
           (h (glgui-height-get)))
      (set! calculator-display-num1 (glgui-label gui 0 (- h 25) w 25 "" calc_14.fnt White))
      (glgui-widget-set! gui calculator-display-num1 'align GUI_ALIGNRIGHT)
      (glgui-widget-set! gui calculator-display-num1 'focus #t)
      (set! calculator-display-operation (glgui-label gui 0 (- h 45) w 25 "" calc_14.fnt White))
      (glgui-widget-set! gui calculator-display-operation 'align GUI_ALIGNLEFT)
      (set! calculator-display-num2 (glgui-label gui 0 (- h 65) w 25 "" calc_14.fnt White))
      (glgui-widget-set! gui calculator-display-num2 'align GUI_ALIGNRIGHT)
      (set! calculator-display-answer (glgui-label gui 0 (- h 85) w 25 "" calc_14.fnt White))
      (glgui-widget-set! gui calculator-display-answer 'align GUI_ALIGNRIGHT)
      ;; Create buttons
      (let loop((button-number 16))
        (if (= button-number 0)
          (set! button-number 0) ;; exit
          (begin (create-button (- button-number 1))
                 (loop (- button-number 1)))))
    ))
;; events
  (lambda (t x y)
    (if (= t EVENT_KEYRELEASE)
      (cond
        ((= x EVENT_KEYESCAPE)     (terminate))
      ))
    (glgui-event gui t x y))
;; termination
  (lambda () #t)
;; suspend
  (lambda () (glgui-suspend))
;; resume
  (lambda () (glgui-resume))
)

;; eof
