; console program
; Lisp / Common Lisp / SBCL 
; based on : 
; http://www.mostlymaths.net/2009/08/lavaurs-algorithm.html
; lisp code by R Berenguel
; reducible angle is not angle of lower period 
; for example 9/15 = 3/5 has period 4 






;
; Rules by Lavaures :
; no arc crosses any other arc
; arc connects 2 angles with the same period : first and second
; first angle is the smallest angles not yet connected, and second angle is the next smallest angle not yet connected
;
; orthogonal circles  (x1,y1,r1) and (x2,y2,r2)
; r1^2 + r2^2 = (x2-x1)^2 +(y2-y1)^2
; http://planetmath.org/encyclopedia/OrthogonalCircle.html
; http://classes.yale.edu/fractals/Labs/NonLinTessLab/BasicConstr3.html
; 
; example of use : 
; 
; sbcl 
; (load "l.lisp")
; (load (compile-file "l.lisp"))
; (give-pairs  *period*)
;  (give-cardioid-pairs  *period*)    
;  
; Adam Majewski
; fraktal.republika.pl
; 2010.09.04- 11.17
;
;
;;  This program is free software: you can redistribute it and/or
;;  modify it under the terms of the GNU General Public License as
;;  published by the Free Software Foundation, either version 3 of the
;;  License, or (at your option) any later version.

;;  This program is distributed in the hope that it will be useful,
;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;;  General Public License for more details.

;;  You should have received a copy of the GNU General Public License
;;  along with this program. If not, see
;;  <http://www.gnu.org/licenses/>.
















 





(defun doubling-map (ratio-angle)
" period doubling map =  The dyadic transformation (also known as the dyadic map, 
 bit shift map, 2x mod 1 map, Bernoulli map, doubling map or sawtooth map "
(let* ((n (numerator ratio-angle))
       (d (denominator ratio-angle)))
  (setq n  (mod (* n 2) d)) ; (2 x n) modulo d = doubling
  (/ n d)))



(defun give-period (ratio-angle)
	"gives period of angle in turns (ratio) under doubling map"
	(let* ((n (numerator ratio-angle))
	       (d (denominator ratio-angle))
	       (temp n)) ; temporary numerator
	  
	  (loop for p from 1 to 100 do 
		(setq temp  (mod (* temp 2) d)) ; (2 x n) modulo d = doubling)
		when ( or (= temp n) (= temp 0)) return p )))

(defun give-list (period)
  "Returns a list of all  angles of  given period 
   without angles with lower periods, which divide period.
   period is integer > 0 "
  (let* ((angles-list '())
	 (den (- (expt 2 period) 1 )) ; denominator of angle = (2^period)-1
	  a ) ; angle
    (when (> period 0) 
      (dotimes  (num (+ den 1)) ; from 0 to den
	(setq a (/ num den ))
	(when (= period (give-period a)) ; 
         (setq angles-list (append angles-list (list a)))))) ; 
      angles-list))


(defun not-crosses (new-pair old-pair)
"checks if new arc ( = between new-angle-1 and new-angle-2)
crosses old arc ( = between (first old-pair) and (second old-pair)).
It is a part of Lavaurs algorithm.
Angles are external angles mesured in turns 
angle-1 < angle-2 
test : 
(setq old-p '(1/3 2/3))
(setq new-p '(4/15 6/15))
(not-crosses new-p old-p) 
(not-crosses (list 1/7 2/7) old-p) ; t
(not-crosses (list 1/7 3/7) old-p) ; nil
"
(let ((old-angle-1 (first old-pair))
	(old-angle-2 (second old-pair))
	(new-angle-1 (first new-pair))
	(new-angle-2 (second new-pair)))

; check the order of input angles
(when (< new-angle-2 new-angle-1) (rotatef new-angle-1 new-angle-2))
(when (< old-angle-2 old-angle-1) (rotatef old-angle-1 old-angle-2))
(cond
	((< new-angle-1 new-angle-2 old-angle-1 old-angle-2) t) 
	((< old-angle-1 old-angle-2 new-angle-1 new-angle-2) t) 
	((< new-angle-1 old-angle-1 old-angle-2 new-angle-2) t) 
	((< old-angle-1 new-angle-1 new-angle-2 old-angle-2) t)
	(t nil))))



(defun not-crosses-all (new-pair old-list)
"checks if new pair of rays do not crosses any of pairs from old list
test : 
(setq old-pairs '((1/3 2/3) (1/7 2/7)))
(not-crosses-all (list 4/15 6/15) old-pairs) ; nil
"
(let ((b-result T)) 
(loop for old-pair in old-list do (setq b-result (and b-result (not-crosses new-pair old-pair))))
b-result ))



(defun give-pairs-up-to (period)
"gives list of external angles  (pairs landing on the same root point)
for periods up to input period
period >= 3 
examples of use : 
(give-pairs-old-old 3)
(give-pairs-old-old 7)
"
 (let* ((pairs (list (list 1/3 2/3))) ; list for period 2 
	angles-list ; temporary list
	i	
	new-first-angle
	new-second-angle) 

	( loop for p from 3 to period do 
  		(setq angles-list (give-list p))
		(loop for j from  0 to (/ (- (length angles-list) 1) 2)  do 

  			(setq new-first-angle (pop angles-list)) ; pop removes angle from list
			; find second ( conjugate) angle
			(setq i 0)
			(loop  do ;for i from 0 to (- (length angles-list) 1) do  ; first = nth 0 
 		
				(setq new-second-angle (nth i angles-list)) ; nth do not removes angle from list
				(setq i (+ i 1)) 
  			until (not-crosses-all (list new-first-angle new-second-angle) pairs))
		(setq angles-list (remove new-second-angle angles-list))
	(push (list new-first-angle new-second-angle)  pairs))) ; save new pair to pairs list

(reverse pairs)))




; it should be the same as number of components
;(loop for p from 3 to 10 do (print (length (give-pairs p))))
;3 
;6 
;15 
;27 
;63 
;120 
;252 
;495 


; 
(defun give-pairs (period-max)
"gives list of external angles  (pairs landing on the same root point)
for period = pariod-max
period >= 2 
examples of use : 
(give-pairs 3)
(give-pairs 7) ; 
(time (give-pairs 16))
(compile 'give-pairs)
 
"
 (let* ((pairs (list (list 1/3 2/3))) ; list for period 2 
	angles-list ; temporary list
	(previous-list pairs) ; list for "previous period" = (period -1)
	i	
	new-first-angle
	new-second-angle) 
 
	( loop for period from 3 to period-max do 
  		(setq angles-list (give-list period)) ; find all angles for given period
		(setq previous-list pairs) ; update previous list
		; match pairs of angles 
		(loop for j from  0 to (/ (- (length angles-list) 1) 2)  do 
 
  			(setq new-first-angle (pop angles-list)) ; pop removes angle from list
			; find second ( conjugate) angle
			(setq i 0)
			(loop  do ;for i from 0 to (- (length angles-list) 1) do  ; first = nth 0 
 
				(setq new-second-angle (nth i angles-list)) ; nth do not removes angle from list
				(setq i (+ i 1)) 
  			until (not-crosses-all (list new-first-angle new-second-angle) pairs))
			(setq angles-list (remove new-second-angle angles-list))
			(push (list new-first-angle new-second-angle)  pairs))); save new pair to pairs list
 
 
	(setq pairs (set-difference pairs previous-list  :test 'equal))	; remove previous angles
	(reverse pairs)))


(defun give-pair-diff (pair)
"gives a difference between second and first elemant 
 of rational angles pair
 examples of use 
 (give-pair-diff '(1/9 4/9))

"
(let* ((n1  (numerator (first pair)) )
       (n2 (numerator (second pair))))
; check order of numeratore
 (if (< n1  n2)  
     (- n2 n1 )  ; then 
     (- n1 n2)))) ; else 


(defun is-pair-not-on-antenna (pair)
" input is a pair = list of 2 ratios
 checking if both angles are < or > 1/2 
( tip of the main antenna 
  is the landing point of the external ray 1/2)
"
(let* ((p (sort pair #'<))) ; sort list in ascending order

 (if ( and (> 1/2 (first p)) ; first < 1/2
           (< 1/2 (second p))) ; second > 1/2 
      nil    ; on the antenna 
      t)     ; not on the antenna 
 
))








(defun give-cardioid-pairs (period-max)
"gives a list of external angles = (pairs landing on the same root point)
 which land on the main cardioid of Mandelbrot set ( difference = 1 )
"
 (let* ((pairs (list (list 1/3 2/3))) ; list for period 2 
	angles-list ; temporary list
	(previous-list pairs) ; list for "previous period" = (period -1)
	i	
	new-first-angle
	new-second-angle
        pair ) 

	( loop for period from 3 to period-max do 
  		(setq angles-list (give-list period)) ; find all angles for given period
		(setq previous-list pairs) ; update previous list
		; match pairs of angles 
		(loop for j from  0 to (/ (- (length angles-list) 1) 2)  do 

  			(setq new-first-angle (pop angles-list)) ; pop removes angle from list
			; find second ( conjugate) angle
			(setq i 0)
			(loop  do ;for i from 0 to (- (length angles-list) 1) do  ; first = nth 0 
 		
				(setq new-second-angle (nth i angles-list)) ; nth do not removes angle from list
				(setq i (+ i 1)) 
  			until (not-crosses-all (list new-first-angle new-second-angle) pairs))
			(setq angles-list (remove new-second-angle angles-list))
                        (setq pair (list new-first-angle new-second-angle) ) ; 
                        (give-pair-diff pair)
			(if (and (> 2 (give-pair-diff pair)) (is-pair-not-on-antenna pair)) (push pair pairs)))); if diff(pair)<2 then save new pair to pairs list
		
	 
	( setq pairs (set-difference pairs previous-list  :test 'equal))	; remove previous angles
	(reverse pairs)   ))




(defun give-all-cardioid-pairs (period)
"  
(give-all-cardioid-pairs 1 ) 

"
(cond
  (( > 1 period) () (print " period should be > 0 ; there is no 0 or negative period components "))
  (( = 1 period) (list 0/1 1/1)) ; 
  (( = 2 period) (list 1/3 2/3))
  (t (give-cardioid-pairs period)) ; if period > 2 
 )

)


;----------global var ----------------------
 
(defparameter *period* 4 " maximal period. It is an integer >= 2 ")


 

;======================= run =====================================================================

;

