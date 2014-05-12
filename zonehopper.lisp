;;;; zonehopper.lisp

(in-package #:zonehopper)

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
