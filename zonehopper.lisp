;;;; zonehopper.lisp

(in-package #:zonehopper)

(defun newstrlist->plist (strlist)
  "Create a new plist and convert a list of strings into it"
  (progn
    (setq plist (gensym))
    (strlist->plist plist strlist)))

(defun strlist->plist (plist strlist)
"Convert 'strlist' into 'plist' as a plist"
  (if (not strlist)
      plist
      (progn
	(setf (get plist (car strlist)) (cadr strlist))
	(strlist->plist plist (cddr strlist)))))

(defun strlist->alist (strlist)
  "Convert 'strlist' into an alist"
  (labels ((alistify (alist slist)
	   (if (not slist)
	       alist
	       (alistify (cons (cons (car slist) (cadr slist)) alist)
			       (cddr slist)))))
    (alistify '() strlist)))


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

;; 'distance' could be meters, or it could be whatever unit the geography
;; is in. Currently I don't care.
(pomo:defprepared-with-names transit-within (origin_id distance type)
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
