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

;;Details of DDW chart, allows for easy update of application in the case of new chart data

;;Instructions

;;remove everything from excel file except chart data (remove any instructions)
;;ensure new chart is turned into a .csv file
;;name this file DDW.csv
;;put DDW.csv into the data file
;adjust chart variables below to reflect any changes in the chart

;*Note* The variables used below reflect the rows shown in the excel file starting at 1
;	The rows will automatically be modified to start at 0 so that the lists will be read properly

;START OF CHART VARIABLES
(define ageinc 1) 	;;value that age increments by
(define htinc 5) 	;;value that height increments by
(define agemin 5) 	;;min age value
(define agemax 19) 	;;max age value

(define boysrow 1) 	;;row that boys starts at
(define boyshtmin 100) 	;;min value of boys height
(define boyshtmax 195) 	;;max value of boys height

(define girlsrow 18) 	;;row that girls starts at
(define girlshtmin 100) ;;min value of girls height
(define girlshtmax 180) ;;max value of girls height
;END OF CHART VARIABLES

;Forced Order Option
;#t means inputs appear and are only accessible once the previous one has been settings
;#f means all inputs are visible
(define ForcedOrder #f)
(define Input #f)

(include "embed.scm")

(define gui #f)

;;variables
(define gender 0) ;0-Male, 1-Female
(define age #f)

(define row #f)
(define col #f)

(define data #f)
(define datacol0 #f)
(define finalweight #f)
(define weight #f)
(define height #f)

;Temporary storage for widget variables
(define HeightVal #f)
(define HeightValMin #f)
(define HeightValMax #f)
(define HeightHidden #f)
(define AgeVal #f)
(define WeightVal #f)
(define WeightValMin #f)
(define WeightValMax #f)

;;CDC Growth Charts: US Weight-for-age 50th percentile used to estimate weight (ages 2 to 20)
(define cdcboy50 (list 13 15 16 18 21 23 26 28 32 36 41 45 51 56 61 64 67 69 71))
(define cdcgirl50 (list 12 14 16 18 20 23 26 29 33 37 42 46 49 52 54 55 56 57 58))
;;low is a value chosen below the 5th percentile as a bottom end input
(define cdcboylow (list 3 5 6 8 10 12 14 16 18 20 23 26 30 34 38 42 45 45 45))
(define cdcgirllow (list 3 5 6 8 10 12 14 16 18 20 23 26 30 33 36 38 40 40 40))
;;high is a value chosen above the 95th percentile as a top end input
(define cdcboyhigh (list 20 23 27 30 33 38 42 46 54 60 67 75 81 87 91 95 100 100 100))
(define cdcgirlhigh (list 20 23 27 30 33 38 42 47 55 62 68 75 80 83 84 86 90 90 90))
;;if a larger input range is needed cdc low and high can be modified to accomodate this.


;;Used when the max and min height values are changed to ensure value stays within this range
;;if the height value is hidden and therefore not yet set, the value will be centered in this range
(define (check-valbound)
	(if (string=? Input "wheel")
	    (begin
	    	(set! HeightValMin (glgui-widget-get gui wgtht 'valmin))
		(set! HeightValMax (glgui-widget-get gui wgtht 'valmax))
		(set! HeightVal (glgui-widget-get gui wgtht 'value))
		(set! HeightHidden (glgui-widget-get gui wgtht 'hidden))
	    )
	    (if (string=? Input "slider")
	        (begin
    	    	    (set! HeightValMin (glgui-widget-get gui sliderHeight 'valmin))
		    (set! HeightValMax (glgui-widget-get gui sliderHeight 'valmax))
		    (set! HeightVal (glgui-widget-get gui sliderHeight 'value))
		    (set! HeightHidden (glgui-widget-get gui sliderHeight 'hidden))
	        )
	        (begin
		    (set! HeightValMin (glgui-widget-get gui valPickerHeight 'valmin))
		    (set! HeightValMax (glgui-widget-get gui valPickerHeight 'valmax))
		    (set! HeightVal (glgui-widget-get gui valPickerHeight 'value))
		    (set! HeightHidden (glgui-widget-get gui valPickerHeight 'hidden))
	        )
	     )
	)

    
	   (if HeightHidden
	      (set! HeightVal
		(begin (+ HeightValMin 
		     (begin 
			(* 
			  (begin 
			     (fix 
			        (begin 
				   (/ 
				      (begin 
					(/ 
					   (begin 
					      (- HeightValMax HeightValMin
					      )
					   ) 
					   htinc
					)
				      ) 
				      2
				   )
				)
			     )
			  ) 
			  htinc
			)
		     )
		  )
  	        )
	      )
	    )

	    (if (> HeightVal HeightValMax) (set! HeightVal HeightValMax))
	    (if (< HeightVal HeightValMin) (set! HeightVal HeightValMin))

	    (if (string=? Input "wheel")
	    	(glgui-widget-set! gui wgtht 'value HeightVal)
	        (if (string=? Input "slider")
	    	    (glgui-widget-set! gui sliderHeight 'value HeightVal)
		    (glgui-widget-set! gui valPickerHeight 'value HeightVal)
		)
	    )
	

)
;;Toggle main page visibility
;;state controls Gender and Age display
;;state2 controls Weight, Height and the Calculate button
;;These are seperated incase ForcedOrder is used
;;override will control all Input widgets instead of just the selected one if set to true
(define (hide-main state state2 override)
	;;Control labels and buttons, always will be turned on or off
	(glgui-widget-set! gui wgtexit 'hidden state)  	
    	(glgui-widget-set! gui wgtgend 'hidden state)
    	(glgui-widget-set! gui gendlbl 'hidden state)
    	(glgui-widget-set! gui agelbl 'hidden state)
    	(glgui-widget-set! gui weightlbl 'hidden state2)
    	(glgui-widget-set! gui weightlbl2 'hidden state2)
    	(glgui-widget-set! gui htlbl 'hidden state2)
    	(glgui-widget-set! gui htlbl2 'hidden state2)
    	(glgui-widget-set! gui wgtcalc 'hidden state2)

	(glgui-widget-set! gui settingsButton 'hidden state)

	;;if wheel type is selected control wheel widgets
	(if (or (string=? Input "wheel") override)
	    (begin
    		(glgui-widget-set! gui wgtage 'hidden state)
		(glgui-widget-set! gui wgtweight 'hidden state2)
		(glgui-widget-set! gui wgtht 'hidden state2)
	    )
	)
	;;if slider type is selected control slider widgets
	(if (or (string=? Input "slider") override)
	    (begin
    		(glgui-widget-set! gui sliderAge 'hidden state)
		(glgui-widget-set! gui sliderWeight 'hidden state2)
		(glgui-widget-set! gui sliderHeight 'hidden state2)
	    )
	)
	;;if valPicker type is selected control horizontalvalpicker widgets
	(if (or (string=? Input "valPicker") override)
	    (begin
		(glgui-widget-set! gui valPickerAge 'hidden state)
		(glgui-widget-set! gui valPickerWeight 'hidden state2)
		(glgui-widget-set! gui valPickerHeight 'hidden state2)
	    )
	)
)
;;Toggle results page visibility
(define (hide-result state)
    (glgui-widget-set! gui wgtback 'hidden state)
    (glgui-widget-set! gui gendreslbl 'hidden state)
    (glgui-widget-set! gui gendres 'hidden state)
    (glgui-widget-set! gui agereslbl 'hidden state)
    (glgui-widget-set! gui ageres 'hidden state)
    (glgui-widget-set! gui weightreslbl 'hidden state)
    (glgui-widget-set! gui weightres 'hidden state)
    (glgui-widget-set! gui heightreslbl 'hidden state)
    (glgui-widget-set! gui heightres 'hidden state)
    (glgui-widget-set! gui DDWreslbl 'hidden state)
    (glgui-widget-set! gui DDWres 'hidden state)
    (glgui-widget-set! gui wgtnew 'hidden state)
    (glgui-widget-set! gui WeightUsedlbl 'hidden state)
    (glgui-widget-set! gui WeightUsed 'hidden state)
    (glgui-widget-set! gui BMIlbl 'hidden state)
    (glgui-widget-set! gui BMI 'hidden state)
    (glgui-widget-set! gui BMIlbl2 'hidden state)
)
;Toggle settings page visibility
(define (hide-settings state)
    (glgui-widget-set! gui selForcedWidget 'hidden state)
    (glgui-widget-set! gui selForcedLbl 'hidden state)
    (glgui-widget-set! gui selInpWidget 'hidden state)
    (glgui-widget-set! gui selInpLbl 'hidden state)
    (glgui-widget-set! gui settingsLbl 'hidden state)
    (glgui-widget-set! gui confirmSettings 'hidden state)
)

;;Gender has been changed, update gender tracking variable, trigger age-change to update min and max height selection
(define (button-callback-gender g w t x y)
  (let ((img (glgui-widget-get g wgtgend 'image))
        (pos (glgui-widget-get g wgtgend 'value)))
	(if (string=? (car (list-ref img pos)) "Male") (begin (set! gender 0)) (begin (set! gender 1)))
  )
;Gender change will potetially adjust other values, following 2 lines will adjust the values
(age-change gui #f #f #f #f)
(check-valbound)

)

;;Calculate DDW
(define (button-callback-calc g w t x y)
  (let ((g #f)(w #f)(t #f)(x #f)(y #f))
    ;Choose which widget to pull values from based on settings choice
    (if (string=? Input "wheel")
	(begin
	    (set! AgeVal (glgui-widget-get gui wgtage 'value))
	    (set! HeightVal (glgui-widget-get gui wgtht 'value))
	    (set! WeightVal (glgui-widget-get gui wgtweight 'value))
	)
    )
    (if (string=? Input "slider")
	(begin
	    (set! AgeVal (glgui-widget-get gui sliderAge 'value))
	    (set! HeightVal (glgui-widget-get gui sliderHeight 'value))
	    (set! WeightVal (glgui-widget-get gui sliderWeight 'value))
	)
    )
    (if (string=? Input "valPicker")
	(begin
	    (set! AgeVal (glgui-widget-get gui valPickerAge 'value))
	    (set! HeightVal (glgui-widget-get gui valPickerHeight 'value))
	    (set! WeightVal (glgui-widget-get gui valPickerWeight 'value))
	)
    )
     ;;calculate row to take data from
     (set! row (begin 
		   (fix (+
			(if (= gender 0) boysrow girlsrow)		;;adjust row to boys or girls section of table
			(- AgeVal agemin) ;;adjust row to the correct age
                   ))
		)
     )
     ;;calculate column to take data from similarly to the row procedure above
     	   (set! col (begin
		   (fix (/
			(+ 
			    (- 
				HeightVal
				(if (= gender 0) boyshtmin girlshtmin)
		            )
			    htinc
			)
			htinc
		   ))
		)
     	   )
      	;;set patient weight to widget selection
	   (set! weight (begin (fix WeightVal)))
    	;;pick Drug Dosing Weight from chart/data based on row and col selected
     	   (set! DDW (begin (string->number (begin (list-ref (list-ref data row) col)))))
	;;set patient height to widget selection
	   (set! height (begin (fix HeightVal)))
		
	        ;;Set Drug dosing weight to the minimum of actual weight or chart Drug dosing weight.
		   (if (> weight DDW)
			(begin
	      		    (set! finalweight DDW)
			    (glgui-widget-set! gui WeightUsed 'label "Adjusted Weight")
			    (glgui-widget-set! gui WeightUsed 'color Yellow)
			)
			(begin
	      		    (set! finalweight weight)
			    (glgui-widget-set! gui WeightUsed 'label "Actual Weight")
			    (glgui-widget-set! gui WeightUsed 'color Green)
			)
	    	   )
	;;Adjust input data display on results page (*note* Weight Used is adjust above in if statement)
	(glgui-widget-set! gui gendres 'label (if (= gender 0) "Male" "Female"))
	(glgui-widget-set! gui gendres 'color (if (= gender 0) Blue Pink))
	(let ((BMIcalc (begin (round (begin (/ weight (begin (expt (begin (/ height 100.0)) 2.0))))))))
	   (glgui-widget-set! gui BMI 'label (begin (string-append (number->string (fix BMIcalc)) " ")));" kg/m^2")))
	)
	(glgui-widget-set! gui ageres 'label (begin (number->string (fix AgeVal))))
	(glgui-widget-set! gui weightres 'label (begin (string-append (number->string (fix WeightVal)) " kg" )))	
	(glgui-widget-set! gui heightres 'label (begin (string-append (number->string (fix HeightVal)) " cm" )))
	(glgui-widget-set! gui DDWres 'label (begin (string-append (number->string (fix finalweight)) " kg" )))
        ;;Go to results page (hide main, unhide results)
	(hide-main #t #t #f)
	(hide-result #f)  
	
  )
)

;;Adjust settings page widgets to current settings then go to page
(define (button-callback-settings g w t x y)
    (let ((g #f)(w #f)(t #f)(x #f)(y #f))

	(if (string=? Input "wheel")
	    (glgui-widget-set! gui selInpWidget 'current 2)
	    (if (string=? Input "slider")
	        (glgui-widget-set! gui selInpWidget 'current 1)
	        (glgui-widget-set! gui selInpWidget 'current 0)
	     )
	)
	(if ForcedOrder
	    (glgui-widget-set! gui selForcedWidget 'current 0)
	    (glgui-widget-set! gui selForcedWidget 'current 1)
	)
	(hide-main #t #t #f)
	(hide-settings #f)

    )
)

;;Confirm settings selection - Check settings widget selections and save to settings file & variables
(define (button-callback-confirmSettings g w t x y)
    (let ((g #f)(w #f)(t #f)(x #f)(y #f))
	(let ((selInput (glgui-widget-get gui selInpWidget 'current)))
	    (if (= selInput 0)
		(settings-set! "InputType" "valPicker")
		(if (= selInput 1)
			(settings-set! "InputType" "slider")
			(settings-set! "InputType" "wheel")
		)
	    )
	    (set! Input (settings-ref "InputType" #f))
	)
	(let ((selForced (glgui-widget-get gui selForcedWidget 'current)))
	    (if (= selForced 0)
		(settings-set! "ForcedOrder" #t)
		(settings-set! "ForcedOrder" #f)
	    )
	    (set! ForcedOrder (settings-ref "ForcedOrder" #f))
	)

	(hide-settings #t)
	(if ForcedOrder
	    (hide-main #f #t #f)
	    (hide-main #f #f #f)
	)
    )
)

;;Go back to main page and leave inputs as is
(define (button-callback-back g w t x y) ;;Modify
    (let ((g #f)(w #f)(t #f)(x #f)(y #f))
	(hide-main #f #f #f)
	(hide-result #t)
    )
)
;;Go back to main page and reset inputs
(define (button-callback-new g w t x y)
    (let ((g #f)(w #f)(t #f)(x #f)(y #f))
	(if ForcedOrder
	    (hide-main #f #t #f)
	    (hide-main #f #f #f)
	)
	(hide-result #t)
	(hide-main #t #t #f)
	(glgui-widget-set! gui wgtage 'value 12)
	(glgui-widget-set! gui sliderAge 'value 12)
	(glgui-widget-set! gui valPickerAge 'value 12)
	(age-change g w t x y)
	(if ForcedOrder
	    (hide-main #f #t #f)
	    (hide-main #f #f #f)
	)
    )
)

;;Load data from DDW.csv
(define (get-DDWdata)
   (let ((path (string-append (system-directory) (system-pathseparator) "/data/DDW.csv")))
	(if (file-exists? path)
	;; If the file exists, read from it
	(csv-read path)
 	;; Otherwise return an empty list
	(begin
		(print "DDW-not-found")
		(list)
	)		
	)
   )
)
;;Find the maximum height value from the DDW chart - Used to limit input selection
(define (maxht row colstrt)
	(let ((maxcol (begin
  		(+ (begin
			(/ (begin
		  		(- (begin(if (= gender 0) boyshtmax girlshtmax))
			     	   (begin(if (= gender 0) boyshtmin girlshtmin))
				)) 
			htinc))
		1)
	     )))
	     (let ((maxcolfull (begin
		(if (begin (and
				(= colstrt maxcol)
				(begin
		    		    (integer? 
		   	    		(begin (string->number 
			    		   (begin (list-ref (list-ref data row) colstrt))
	   	   	    		)) 
		    		    )
		    		)
			    )
		    ) #t #f))
	     ))
	    (let loop ((i colstrt))
		(if (begin
		        (or
			    maxcolfull

			    (begin (not
			    	(integer? 
			   	    (begin (string->number 
					(begin (list-ref (list-ref data row) i))
		   	   	    )) 
			    	)
			    ))
			)
		    )
		    (begin ;true (integer found)
			(set! HeightValMax
			   (begin
				(string->number 
				   (begin (list-ref (list-ref data 
					(begin 
					   (if (= gender 0) 
						(begin (- boysrow 1)) 
						(begin (- girlsrow 1))
					   )
					)) (begin (- i (begin 
								(if maxcolfull 0 1)
							)
						  )
					   )
		   	   	   )) 
			  	)
			    )
			)
			(if (string=? Input "wheel")
	    	    	    (glgui-widget-set! gui wgtht 'valmax HeightValMax)
			    (if (string=? Input "slider")
	     	    	        (glgui-widget-set! gui sliderHeight 'valmax HeightValMax)
			        (glgui-widget-set! gui valPickerHeight 'valmax HeightValMax)
			    )
			)
			(check-valbound)
		    )
		    (begin ;false (integer not found, check next column)
			(loop (fx+ i 1))
		    )
		)))
	   )	
)

;;Find the minimum height from the DDW chart - used to limit input selection
(define (minht row colstrt)

    (let loop ((i colstrt))
	(if (begin
		(integer? 
		   (begin (string->number 
			(begin (list-ref (list-ref data row) i))
	   	   )) 
		)
	    )
	    (begin ;true (integer found)
		(set! HeightValMin
		   (begin
			(string->number 
			   (begin (list-ref (list-ref data 
				(begin 
				   (if (= gender 0) 
					(begin (- boysrow 1)) 
					(begin (- girlsrow 1))
				   )
				)) i)
	   	   	   ) 
		  	)
		    )
		)
		(if (string=? Input "wheel")
	    	    (glgui-widget-set! gui wgtht 'valmin HeightValMin)
		    (if (string=? Input "slider")
	     	        (glgui-widget-set! gui sliderHeight 'valmin HeightValMin)
		        (glgui-widget-set! gui valPickerHeight 'valmin HeightValMin) ;;default - arrows
		    )
		)
		(maxht row i) ;;min found, next find max
		(check-valbound)
	    )
	    (begin ;false (integer not found, check next column)
		(loop (fx+ i 1))
	    )
	)
    )
	
)
;;This function finds the row that needs to be calculated from the age value selected
;;It then makes sure that there is valid data in the row
(define (get-minht)
        (if (string=? Input "wheel")
	    (set! AgeVal (glgui-widget-get gui wgtage 'value))
	    (if (string=? Input "slider")
	        (set! AgeVal (glgui-widget-get gui sliderAge 'value))
	        (set! AgeVal (glgui-widget-get gui valPickerAge 'value))
	    )
        )
	(set! row (begin 
		   (fix (+
			(if (= gender 0) boysrow girlsrow)
			(- AgeVal agemin)
                  ))
		)
	)
	(set! datacol0 (list-ref (list-ref data row) 0))
	(set! datacol0 (begin (string->number datacol0)))
	
	(if (begin (integer? datacol0))
	   (if 
	   	(begin 
		   (and 
			(>= datacol0 agemin) 
			(<= datacol0 agemax)
		   )
	    	) 
	        (begin
		    (minht row 1)
		    
		)
	        (display "(minht row 1) Failed: This row does not start with a valid age. \n")
	   ) 
 	   (display "(minht row 1) Failed: The data in first column of this row is not an integer. Not chart data \n")
 	
	)
)
;;This function adjust min and max values for height and weight if age is changed
;;If height and weight are hidden it will also center their selected value between the range
(define (age-change g w t x y)
 (let ((g #f)(w #f)(t #f)(x #f)(y #f))

    (if (string=? Input "wheel")
	(begin
	    (set! AgeVal (glgui-widget-get gui wgtage 'value))
	    (set! WeightValMin (glgui-widget-get gui wgtweight 'valmin))
	    (set! WeightValMax (glgui-widget-get gui wgtweight 'valmax))
	    (set! WeightVal (glgui-widget-get gui wgtweight 'value))
	    (set! WeightHidden (glgui-widget-get gui wgtweight 'hidden))
	)
    )
    (if (string=? Input "slider")
	(begin
	    (set! AgeVal (glgui-widget-get gui sliderAge 'value))
	    (set! WeightValMin (glgui-widget-get gui sliderWeight 'valmin))
	    (set! WeightValMax (glgui-widget-get gui sliderWeight 'valmax))
	    (set! WeightVal (glgui-widget-get gui sliderWeight 'value))
	    (set! WeightHidden (glgui-widget-get gui sliderWeight 'hidden))
	)
    )
    (if (string=? Input "valPicker")
	(begin
	    (set! AgeVal (glgui-widget-get gui valPickerAge 'value))
	    (set! WeightValMin (glgui-widget-get gui valPickerWeight 'valmin))
	    (set! WeightValMax (glgui-widget-get gui valPickerWeight 'valmax))
	    (set! WeightVal (glgui-widget-get gui valPickerWeight 'value))
	    (set! WeightHidden (glgui-widget-get gui valPickerWeight 'hidden))
	)
    )

    (let ((age (begin (fix AgeVal))))
	(set! WeightValMin (begin
	    (fix (begin (list-ref (begin (if (= gender 0) cdcboylow cdcgirllow)) (begin (- age 2)))))))
	(set! WeightValMax (begin
	    (fix (begin (list-ref (begin (if (= gender 0) cdcboyhigh cdcgirlhigh)) (begin (- age 2)))))))
    (if WeightHidden
	(begin	
	    (set! WeightVal (begin
	    	(fix (begin (list-ref (begin (if (= gender 0) cdcboy50 cdcgirl50)) (begin (- age 2)))))))
	    (if (string=? Input "wheel")
		(glgui-widget-set! gui wgtweight 'value WeightVal)
		(glgui-widget-set! gui sliderWeight 'value WeightVal)
    	    )
	)
    )
    (if (< WeightVal WeightValMin) (set! WeightVal WeightValMin))
    (if (> WeightVal WeightValMax) (set! WeightVal WeightValMax))
    (if (string=? Input "wheel")
	(begin
	    (glgui-widget-set! gui wgtweight 'valmin WeightValMin)
	    (glgui-widget-set! gui wgtweight 'valmax WeightValMax)
	    (glgui-widget-set! gui wgtweight 'value WeightVal)
	)
    )
    (if (string=? Input "slider")
	(begin
	    (glgui-widget-set! gui sliderWeight 'valmin WeightValMin)
	    (glgui-widget-set! gui sliderWeight 'valmax WeightValMax)
	    (glgui-widget-set! gui sliderWeight 'value WeightVal)
	)
    )
    (if (string=? Input "valPicker")
	(begin
	    (glgui-widget-set! gui valPickerWeight 'valmin WeightValMin)
	    (glgui-widget-set! gui valPickerWeight 'valmax WeightValMax)
	    (glgui-widget-set! gui valPickerWeight 'value WeightVal)
	)
    )

    )
    
    (get-minht)
	
    (if ForcedOrder
	(begin
	    (if (string=? Input "wheel")
	        (glgui-widget-set! gui wgtweight 'hidden #f)
	        (if (string=? Input "slider")
		    (glgui-widget-set! gui sliderWeight 'hidden #f)
		    (if (string=? Input "valPicker")
			(glgui-widget-set! gui valPickerWeight 'hidden #f)
		    )
	        )
	    )
    	    (glgui-widget-set! gui weightlbl 'hidden #f)
    	    (glgui-widget-set! gui weightlbl2 'hidden #f)
	)
    )
  )
)
;;If weight is adjusted and forced order is on height will appear
(define (weight-change g w t x y)
 (let ((g #f)(w #f)(t #f)(x #f)(y #f))
   (if ForcedOrder
	(begin
	    (if (string=? Input "wheel")
	        (glgui-widget-set! gui wgtht 'hidden #f)
	    )
	    (if (string=? Input "slider")
		(glgui-widget-set! gui sliderHeight 'hidden #f)
	    )
	    (if (string=? Input "valPicker")
	        (glgui-widget-set! gui valPickerHeight 'hidden #f)
	    )
    	    (glgui-widget-set! gui htlbl 'hidden #f)
    	    (glgui-widget-set! gui htlbl2 'hidden #f)
	)
    )
  )
)
;;If height is adjusted and forced order is on the calculate button will appear
(define (height-change g w t x y)
 (let ((g #f)(w #f)(t #f)(x #f)(y #f))
   (if ForcedOrder
	(begin
	    (glgui-widget-set! gui wgtcalc 'hidden #f)
	)
    )
  )
)


;csv-read is not adding an empty element to the end of the read if no element is present in the final col
;this causes a problem when the last column has no number and is attempted to be read since it does not exist
(define (pad-data)	;add an empty string to the end of each row so that it can now be read without error
	(let ((emptystring (begin (list "")))(tempdata (begin (list (begin (append (begin (list-ref data 0)) (begin (list "")))))))
	      (datalength (length data)))
	    (let loop ((i 1))
		(cond
		    ((< i datalength)
			(set! tempdata 
			    (begin (append tempdata 
				(begin (list 
				    (begin 
					(append (begin (list-ref data i)) emptystring)
				    )
				))
			    ))
			)		
			(loop (fx+ i 1))))
		
	    )
	    (set! data tempdata)
	)
)

(main
;; initialization
  (lambda (w h)
    ;initialize data
    (set! data (get-DDWdata))
    (pad-data)

    ;;Initialize settings
    ;;Make sure we have a config directory
    (let ((configdirectory (string-append (system-directory) (system-pathseparator) "config")))
	(if (not (file-exists? configdirectory))
	    (create-directory configdirectory)))
    ;;Initialize the settings file
    (settings-init (list (cons "InputType" "valPicker")
			 (cons "ForcedOrder" #t)))
    ;;Set a variable to our Input type from the settings file for easier access later
    (set! Input (settings-ref "InputType" #f))
    (set! ForcedOrder (settings-ref "ForcedOrder" #f))

    ;;initialize main screen
    (make-window 320 480)
    (glgui-orientation-set! GUI_PORTRAIT)
    (set! gui (make-glgui))
    (glgui-menubar gui 0 (- (glgui-height-get) 44) (glgui-width-get) 44)
    (glgui-menubar gui 0 0 (glgui-width-get) 44)
    (glgui-pixmap  gui 8 (- (glgui-height-get) 32) title.img)
    (let ((bw 150) (bh 50))
       (let ((bx (/ (- (glgui-width-get) bw) 1.8))
             (by (/ (- (glgui-height-get) bh) 1.15)))
	  (set! gendlbl (glgui-label gui 10 (+ by (- (/ bh 2) (/ 16 2))) w 16 "Gender" ascii_24.fnt White))
          (set! wgtgend (glgui-button-string gui bx by bw bh (list "Male" "Female") ascii_18.fnt button-callback-gender))
	  (set! settingsButton (glgui-button gui (+ bx 165) by 50 50 cog.img button-callback-settings))
	)
        
	(let ((bx (/ (- (glgui-width-get) bw) 2.15))
             (by (/ (- (glgui-height-get) bh) 1.45)))
	  (set! agelbl (glgui-label gui 20 (+ by (- (/ bh 2) (/ 16 2))) w 16 "Age" ascii_24.fnt White))
          (set! wgtage (glgui-numberwheel gui bx by 200 bh agemin agemax #f White Black (color-shade White 0.5) ascii_32.fnt ascii_24.fnt #f ageinc))
	  (set! sliderAge (glgui-slider gui bx (- by 10) 200 75 agemin agemax #f White White Orange Black ascii_24.fnt ascii_18.fnt #f White))
	  (set! valPickerAge (glgui-horizontalvaluepicker gui (/ (- (glgui-width-get) bw) 1.5) by 130 60 agemin agemax Grey Blue White Black ascii_24.fnt))
	  (glgui-widget-set! gui sliderAge 'showvalue #t)
	  (glgui-widget-set! gui sliderAge 'showlabels #f)
	  (glgui-widget-set! gui sliderAge 'hidden #t))

	(let ((bx (/ (- (glgui-width-get) bw) 2.15))
             (by (/ (- (glgui-height-get) bh) 2)))
	  (set! weightlbl (glgui-label gui 10 (+ by (- (/ bh 2) (/ 16 2))) w 16 "Weight" ascii_24.fnt White))
	  (set! weightlbl2 (glgui-label gui 285 (+ by (- (/ bh 2) (/ 16 2))) w 16 "kg" ascii_24.fnt White))
          (set! wgtweight (glgui-numberwheel gui bx by 200 bh 10 100 #f White Black (color-shade White 0.5) ascii_32.fnt ascii_24.fnt #f 1))
	  (set! sliderWeight (glgui-slider gui bx (- by 10) 200 75 10 100 #f White White Orange Black ascii_24.fnt ascii_18.fnt #f White))
	  (set! valPickerWeight (glgui-horizontalvaluepicker gui (/ (- (glgui-width-get) bw) 1.5) by 130 60 10 100 Grey Blue White Black ascii_24.fnt))
	  (glgui-widget-set! gui sliderWeight 'showvalue #t)
	  (glgui-widget-set! gui sliderWeight 'showlabels #f)
	  (glgui-widget-set! gui sliderWeight 'hidden #t))

       (let ((bx (/ (- (glgui-width-get) bw) 2.15))
             (by (/ (- (glgui-height-get) bh) 3.1)))
	  (set! htlbl (glgui-label gui 10 (+ by (- (/ bh 2) (/ 16 2))) w 16 "Height" ascii_24.fnt White))
	  (set! htlbl2 (glgui-label gui 285 (+ by (- (/ bh 2) (/ 16 2))) w 16 "cm" ascii_24.fnt White))
          (set! wgtht (glgui-numberwheel gui bx by 200 bh 100 195 #f White Black (color-shade White 0.5) ascii_32.fnt ascii_24.fnt #f 5))
	  (set! sliderHeight (glgui-slider gui bx (- by 10) 200 75 100 195 #f White White Orange Black ascii_24.fnt ascii_18.fnt #f White 5))
	  (glgui-widget-set! gui sliderHeight 'showvalue #t)
	  (glgui-widget-set! gui sliderHeight 'showlabels #f)
	  (glgui-widget-set! gui sliderHeight 'hidden #t)
	  (set! valPickerHeight (glgui-horizontalvaluepicker gui (/ (- (glgui-width-get) bw) 1.5) by 130 60 100 195 Grey Blue White Black ascii_24.fnt 5))
	  )

       (let ((bx 170) (by (- (glgui-height-get) 44 325 bh)))
          (set! wgtcalc (glgui-button-string gui bx by (- bw 10) bh "Calculate!" ascii_18.fnt button-callback-calc)))
	(let ((bx 10) (by (- (glgui-height-get) 44 325 bh)))
          (set! wgtexit (glgui-button-string gui bx by (- bw 10) bh "Exit" ascii_18.fnt (lambda (x . y) (force-terminate)))))
	;;results page buttons
	(let ((bx 10) (by (- (glgui-height-get) 44 325 bh)))
          (set! wgtback (glgui-button-string gui bx by (- bw 10) bh "Modify" ascii_18.fnt button-callback-back)))
	(let ((bx 170) (by (- (glgui-height-get) 44 325 bh)))
          (set! wgtnew (glgui-button-string gui bx by (- bw 10) bh "New" ascii_18.fnt button-callback-new)))

	;;results page labels
	(let ((bx (/ (- (glgui-width-get) bw) 1.8))
             (by (/ (- (glgui-height-get) bh) 1.15)))
	  (set! gendreslbl (glgui-label gui bx (+ by (- (/ bh 2) (/ 16 2))) w 16 "Gender:" ascii_24.fnt White))
	  (set! gendres (glgui-label gui (+ bx 80) (+ by (- (/ bh 2) (/ 16 2))) w 16 "Male" ascii_24.fnt White))
	)
	(let ((bx (/ (- (glgui-width-get) bw) 1.8))
             (by (/ (- (glgui-height-get) bh) 1.30)))
	  (set! agereslbl (glgui-label gui bx (+ by (- (/ bh 2) (/ 16 2))) w 16 "Age:" ascii_24.fnt White))
	  (set! ageres (glgui-label gui (+ bx 50) (+ by (- (/ bh 2) (/ 16 2))) w 16 "0" ascii_24.fnt White))
	)
	(let ((bx (/ (- (glgui-width-get) bw) 1.8))
             (by (/ (- (glgui-height-get) bh) 1.50)))
	  (set! weightreslbl (glgui-label gui bx (+ by (- (/ bh 2) (/ 16 2))) w 16 "Weight:" ascii_24.fnt White))
	  (set! weightres (glgui-label gui (+ bx 80) (+ by (- (/ bh 2) (/ 16 2))) w 16 "10" ascii_24.fnt White))
	)
	(let ((bx (/ (- (glgui-width-get) bw) 1.8))
             (by (/ (- (glgui-height-get) bh) 1.75)))
	  (set! heightreslbl (glgui-label gui bx (+ by (- (/ bh 2) (/ 16 2))) w 16 "Height:" ascii_24.fnt White))
	  (set! heightres (glgui-label gui (+ bx 80) (+ by (- (/ bh 2) (/ 16 2))) w 16 "0" ascii_24.fnt White))
	)
	(let ((bx (/ (- (glgui-width-get) bw) 1.8))
             (by (/ (- (glgui-height-get) bh) 2.3)))
	  (set! BMIlbl (glgui-label gui bx (+ by (- (/ bh 2) (/ 16 2))) w 16 "BMI:" ascii_24.fnt White))
	  (set! BMI (glgui-label gui (+ bx 55) (+ by (- (/ bh 2) (/ 16 2))) w 16 "0" ascii_24.fnt White))
	  (set! BMIlbl2 (glgui-pixmap gui (+ bx 85) (+ by 10) bmi.img))
	)
	(let ((bx (/ (- (glgui-width-get) bw) 6.50))
             (by (/ (- (glgui-height-get) bh) 2.85)))
	  (set! WeightUsedlbl (glgui-label gui bx (+ by (- (/ bh 2) (/ 16 2))) w 16 "Weight Used:" ascii_24.fnt White))
	  (set! WeightUsed (glgui-label gui (+ bx 130) (+ by (- (/ bh 2) (/ 16 2))) w 16 "0" ascii_24.fnt White))
	)
	(let ((bx (/ (- (glgui-width-get) bw) 6.50))
             (by (/ (- (glgui-height-get) bh) 3.75)))
	  (set! DDWreslbl (glgui-label gui bx (+ by (- (/ bh 2) (/ 16 2))) w 16 "Drug Dosing Weight:" ascii_24.fnt White))
	  (set! DDWres (glgui-label gui (+ bx 210) (+ by (- (/ bh 2) (/ 16 2))) w 16 "0" ascii_24.fnt White))
	)

	;;settings page
	(let ((bx (/ (- (glgui-width-get) bw) 2.0))
             (by (/ (- (glgui-height-get) bh) 1.25)))
	  (define field_gradient (list (color:shuffle #xe8e9eaff) (color:shuffle #xe8e9eaff) (color:shuffle #xfefefeff) (color:shuffle #xfefefeff)))
	  (set! selForcedWidget (glgui-dropdownbox gui bx (- by 55) 150 40 
	    (map (lambda (str)
     	      (lambda (lg lw x y w h s) (if s (glgui:draw-box x y w h Grey))
       		(glgui:draw-text-left (+ x 5) y (- w 10) h str ascii_18.fnt Black)))
     	      (list "Default: On" "Off"))
   	    Black field_gradient Black)
	  )
	  (set! selForcedLbl (glgui-label gui (- bx 10) by w 16 "Forced Input Order" ascii_24.fnt White))
	)
	(let ((bx (/ (- (glgui-width-get) bw) 2.0))
             (by (/ (- (glgui-height-get) bh) 1.8)))
	  (define field_gradient (list (color:shuffle #xe8e9eaff) (color:shuffle #xe8e9eaff) (color:shuffle #xfefefeff) (color:shuffle #xfefefeff)))
	  (set! selInpWidget (glgui-dropdownbox gui bx (- by 50) 150 40 
	    (map (lambda (str)
     	      (lambda (lg lw x y w h s) (if s (glgui:draw-box x y w h Grey))
       		(glgui:draw-text-left (+ x 5) y (- w 10) h str ascii_18.fnt Black)))
     	      (list "Arrows" "Slider" "Wheel"))
   	    Black field_gradient Black)
	  )
	  (set! selInpLbl (glgui-label gui (- bx 10) by w 16 "Selecter Widget" ascii_24.fnt White))
	)

	(let ((bx (/ (- (glgui-width-get) bw) 1.70))
             (by (/ (- (glgui-height-get) bh) 1.10)))
	  (set! settingsLbl (glgui-label gui bx by w 16 "Settings" ascii_32.fnt White))
	)


	(let ((bx (/ (- (glgui-width-get) bw) 2.0))
             (by (/ (- (glgui-height-get) bh) 6.5)))
	(set! confirmSettings (glgui-button-string gui bx by 150 50 (list "Confirm") ascii_18.fnt button-callback-confirmSettings)))

	(glgui-widget-set! gui selForcedWidget 'hidden #t)
	(glgui-widget-set! gui selForcedLbl 'hidden #t)
	(glgui-widget-set! gui selInpWidget 'hidden #t)
	(glgui-widget-set! gui selInpLbl 'hidden #t)
	(glgui-widget-set! gui settingsLbl 'hidden #t)
	(glgui-widget-set! gui selInpWidget 'callback confirmSettings)
	(glgui-widget-set! gui confirmSettings 'hidden #t)
	

    )

    (glgui-widget-set! gui wgtage 'value (begin (+ agemin (begin (/ (begin (- agemax agemin)) 2)))))
    (glgui-widget-set! gui sliderAge 'value (begin (+ agemin (begin (/ (begin (- agemax agemin)) 2)))))
    (glgui-widget-set! gui valPickerAge 'value (begin (+ agemin (begin (/ (begin (- agemax agemin)) 2)))))

    ;;set widget callback functions
    (glgui-widget-set! gui wgtage 'callback age-change)
    (glgui-widget-set! gui wgtweight 'callback weight-change)
    (glgui-widget-set! gui wgtht 'callback height-change)

    (glgui-widget-set! gui sliderAge 'callback age-change)
    (glgui-widget-set! gui sliderWeight 'callback weight-change)
    (glgui-widget-set! gui sliderHeight 'callback height-change)

    (glgui-widget-set! gui valPickerAge 'callback age-change)
    (glgui-widget-set! gui valPickerWeight 'callback weight-change)
    (glgui-widget-set! gui valPickerHeight 'callback height-change)
    

    (age-change gui #f #f #f #f)
	
    ;;Hide everything
    (hide-main #t #t #t)
    ;;show necessary widgets
    (if ForcedOrder
	(hide-main #f #t #f)
	(hide-main #f #f #f)
    )
    (hide-result #t)

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
