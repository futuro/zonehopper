;;;; zonehopper.lisp

(in-package #:zonehopper)

(defun strlist->alist (strlist)
  "Convert 'strlist' into an alist"
  (labels ((alistify (alist slist)
	   (if (not slist)
	       alist
	       (alistify (cons (cons (car slist) (cadr slist)) alist)
			       (cddr slist)))))
    (alistify '() strlist)))

(defun strvector->alist (strvec)
  "Convert a vector of strings to an alist"
  (strlist->alist (map 'list (lambda (x) x) strvec)))

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

(defun get-routes (alist tag-key)
  "A general way to get the list of routes from the tags of a mass transit stop node"
  (cdr (tags-assoc alist tag-key)))

(defun get-metcouncil-routes (alist)
  "Get the routes of a TC metrotransit route."
  (get-routes alist "metcouncil:routes"))

; This is probably not the best way to do this
(defun strings->list (strings)
  "Convert a space delimited string list of integers (e.g. \"3 4\")
 of routes to a proper list"
  (if strings
   (with-input-from-string
       (schar strings)
     (loop
	for x = (read schar nil :end)
	until (eq x :end)
	collect x))))

;; Code borrowed from Rosetta Code section for flattening lists
(defun flatten (structure)
  (cond ((null structure) nil)
	((atom structure) (list structure))
	(t (mapcan #'flatten structure))))

;; 'distance' could be meters, or it could be whatever unit the geography
;; is in. Currently I don't care.
(pomo:defprepared-with-names transit-stops-within (origin_id distance type)
	((:with
	  (:as 'origin
	       (:select '*
			:from 'public.planet_osm_point
			:where (:= 'osm_id '$1)))
	  (:select 'point.osm_id 'point.highway 'point.name
		   'point.operator 'point.way 'nodes.tags
		   :from (:as 'public.planet_osm_point 'point)
		   :inner-join 'origin
		   :on (:st_dwithin 'origin.way 'point.way '$2)
		   :inner-join (:as 'public.planet_osm_nodes 'nodes)
		   :on (:= 'point.osm_id 'nodes.id)
		   :where (:= 'point.highway '$3))) origin_id distance type)
	:alists)

#+NIL (transit-within 622228699 1000.0 "bus_stop")
; Bus stops 1000 units around Coffman Union
#+NIL (transit-within 600583817 1000.0 "bus_stop")
; Bus stops 1000 units around Patina on Selby & Snelling
