;;;; zonehopper.lisp

(in-package #:zonehopper)

; Turns out postgis can do all this biz for me! Time to play around with that
(defparameter *earth-radius-miles* 3959 "Radius of the earth in miles")
(defparameter *earth-radius-km* 6371 "Radius of the earth in kilometers")
(defparameter *rad-converter* (/ pi 180) "Constant part of radian conversion")
(defparameter *deg-converter* (/ 180 pi) "Constant part of degree conversion")

(defun deg->rad (degrees)
  (* degrees *rad-converter*))

(defun rad->deg (rads)
  (* rads *deg-converter*))

(defun haversine (theta)
  (expt (sin (/ theta 2)) 2))

(defun distance-rads (lat1 lon1 lat2 lon2)
  (let* ((h-lat (haversine (- lat2 lat1)))
	 (h-lon (haversine (- lon2 lon1)))
	 (alpha (+ h-lat (* h-lon (cos lat1) (cos lat2)))))
    (* 2 *earth-radius-km* (atan (sqrt alpha) (sqrt (- 1 alpha))))))

(defun distance-degs (lat1 lon1 lat2 lon2)
  (distance-rads (deg->rad lat1)
		 (deg->rad lon1)
		 (deg->rad lat2)
		 (deg->rad lon2)))

(defun distance-list (coord-list)
  (let ((lat1 (first (first coord-list)))
	(lon1 (second (first coord-list)))
	(lat2 (first (second coord-list)))
	(lon2 (second (second coord-list))))
    (distance-degs lat1 lon1 lat2 lon2)))

(defun newstrlist->plist (strlist)
  "Create a new plist and convert a list of strings into it"
  (progn
    (setq plist (gensym))
    (strlist->plist plist strlist)))

;; (labels ((plistify (plist slist)
;; 	   "Take a string list slist' and input all the items
;;  into the plist 'sym'"
;; 	   (if (not slist) plist
;; 	       (progn
;; 		 ((setf (get plist (car slist)) (cadr slist))
;; 		  (plistify plist (cddr slist)))))))
;;   (plistify (defvar sym (gensym)) strlist))

(defun strlist->plist (plist strlist)
"Convert 'strlist' into 'plist' as a plist"
  (if (not strlist)
      plist
      (progn
	(setf (get plist (car strlist)) (cadr strlist))
	(strlist->plist plist (cddr strlist)))))

(defun strlist->alist (alist strlist)
"Convert 'strlist' into 'alist' as a alist"
  (if (not strlist)
      alist
      (strlist->alist (cons (cons (car strlist) (cadr strlist)) alist)
		      (cddr strlist))))

(defun tags-assoc (alist tag)
  "Find the string tag"
  (assoc tag alist :test #'equalp))

(defun tags (alist)
  "List all available tags"
  (let ((result '()))
    (dolist (tag alist result)
      (push (car tag) result))))

(defun tag-values (alist)
  "List all available tag values"
  (let ((result '()))
    (dolist (tag alist result)
      (push (cdr tag) result))))
