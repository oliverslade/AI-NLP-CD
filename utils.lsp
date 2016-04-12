#| utils-2b

general purpose utilities
includes...
    association list handlers
    set operators

Author: SCL
020417 modification of set ops to provide new :test arg strategy
011017 >->, -> & set ops

|# 


;=================================
; asl access
;=================================

(defun >-> (alist slots &key (test #'eq))
    (labels ((massoc (slot asl)  ; massoc avoids stray symbols
              (find-if #'(lambda (x)
                           (and (consp x) (funcall test (car x) slot)))
                asl))
             )
	(if (null slots) alist
        (>-> (cdr (massoc (car slots) alist)) (cdr slots))
    )))

(defun -> (alist &rest slots)
   ; caller for >-> without :test
   ; arg structure may be preffered by some
   (>-> alist slots))


;; SET->
;; a non destructive fn to update an asl
;; slots is complete path to new value
;; NB: it is an error to try to extend an asl by converting -
;;    a value into an asl sublist

(defun set>-> (val asl slots)
   (labels ((build (slots val)
              (if (null slots)
                 val
                 (list (cons (car slots) (build (cdr slots) val)))
                 ))
            )
     (cond ((null slots)
            val
            )
           ((assoc (car slots) asl)
            (cons (cons (car slots)
                    (add>-> val (-> asl (car slots)) (cdr slots) ))
              (remove (assoc (car slots) asl) asl))
            )
           ( t
            (append (build slots val) asl)
            ))
     ))

(defun set-> (val asl &rest slots)
   ;; last slots is value to set
  (add>-> val asl slots))

(defun add-> (val asl &rest slots)
  (set>-> (cons val (>-> asl slots)) asl slots))

;; untested
(defun add>-> (val asl slots)
  (set>-> (cons val (>-> asl slots)) asl slots))

;=================================
; set handling
;=================================

;; general equal fn used by set operators
(defvar *$test-fn* #'equal)

(defun $= (x y &rest args)
   (and (apply #'subsetp x y (append args `(:test ,*$test-fn*)))
        (apply #'subsetp y x (append args `(:test ,*$test-fn*)))))

(defun $/= (x y &rest args)
   (not (apply #'$= x y (append args `(:test ,*$test-fn*)))))

(defun $<= (x y &rest args)
   (apply #'subsetp x y (append args `(:test ,*$test-fn*))))

(defun $>= (x y &rest args)
   (apply #'subsetp y x (append args `(:test ,*$test-fn*))))

(defun $< (x y &rest args)
   (and (apply #'subsetp x y (append args `(:test ,*$test-fn*)))
        (not (apply #'subsetp y x (append args `(:test ,*$test-fn*))))) )

(defun $> (x y &rest args)
   (apply #'< y x (append args `(:test ,*$test-fn*))))

(defun $++ (x s &rest args)     ;add x to set s
   (apply #'adjoin s x (append args `(:test ,*$test-fn*))))

(defun $-- (x s &rest args)     ;remove x from set s
   (apply #'remove s x :count 1 (append args `(:test ,*$test-fn*))))

(defun $+ (&rest args)
  ; args test makes this reduce friendly
  (cond ((null args) nil)
        ((= 1 (length args)) (first args))
        ((apply #'union (append args `(:test ,*$test-fn*))))
        ))

(defun $* (&rest args)
  ; args test makes this reduce friendly
  ; NB: ($* x) -> x  is unlikely but consistent with reduce
  (cond ((null args) nil)
        ((= 1 (length args)) (first args))
        ((apply #'intersection (append args `(:test ,*$test-fn*))))
    ))

(defun $- (x y &rest args)
   (apply #'set-difference x y (append args `(:test ,*$test-fn*))))

(defun $~* (x y &rest args)
   (apply #'set-exclusive-or x y (append args `(:test ,*$test-fn*))))

(defun $>> (s x &rest args)
   (apply #'member x s (append args `(:test ,*$test-fn*))))

(defun $<< (x s &rest args)
   (apply #'member x s (append args `(:test ,*$test-fn*))))



