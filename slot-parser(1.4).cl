
;; scl.98 -- a rewrite

(defun specialp (x) 
  (and (consp x)
       (member (car x) '(=> ALL=>))
       x))   ; must return x due to specialp's use as matcher predicate

(defun parse-sub-slots (detail &optional (c-names *c-names*))
  (setf *c-names* c-names)
  (labels 
      ((pslots (detail)
               ; see external doc for details
               (cond ((null detail) nil)
                     ((atom detail)
                      (let ((parsed (parse-slot-entry detail c-names)))
                        (if (listp parsed)  ; it was a dotted form
                            parsed
                          `',parsed)
                        ))
                     
                     ;; remaining clauses...
                     ;; car detail is atomic so either head of special form or literal.

                     ((eq (car detail) 'quote)   ; allows c-names to be quoted
                      detail            ; to supress their expansion
                      )
                     ((special-slotp (car detail))
                      (rule-special-slot detail))
                     (t `(cons ,(pslots (car detail)) 
                               ,(pslots (cdr detail))))
                     
                     ))
       
       (edit (tree) ;; this edits some conses to safe-appends
             (cond ((null tree) nil)
                   ((atom tree) tree)
                   ((or (mlet ('(cons ?(x specialp) ?y) tree)
                              `(safe-append ,#?x ,(edit #?y)))
                        (cons (edit (car tree)) (edit (cdr tree)))
                        ))
                   ))
       )
    
    ;; pslots wraps an extra (cons ... nil) round the semantics
    ;; this is a hack to remove it!!
   ; (list (edit (mlet ('(cons ?sems nil) (pslots detail))
   ;                   #?sems)))
    (let ((tmp (edit (pslots detail))))
      (or (mlet ('(cons ?sems nil) tmp) #?sems)
          tmp))
    ))

(defun parse-slots (&rest args)
  ; this does a parse-sub-slots then wraps it
  ; it's not a hack, frames need an extra list wrapping
  (list (apply #'parse-sub-slots args)))




;; yet another redefinition of multiple assoc to allow * to select all
;; and ? to be a wild card
;; one problem is that of collecting .v. appending results
;; only the latest */? wildcard should collect, the rest should append

(defun multiple-assoc (alist slots &key (test #'eq))
  (labels ((massoc (slot asl)  ; massoc avoids stray symbols
              (find-if #'(lambda (x)
                           (and (consp x) (funcall test (car x) slot)))
                asl))
           
           (ma (alist slots n-wildcards)      ;; this is the core fn
               (cond ((null slots) alist)     ;; n-wildcards is the no. of */? in slots
                     ((atom alist) nil)
                     ((eq '* (car slots))
                      (remove nil
                              (loop for sub-asl in alist
                                  if (> n-wildcards 1)   ;; not the last one
                                  append (ma(list sub-asl) (cdr slots) (1- n-wildcards))
                                    else
                                  collect (ma (list sub-asl) (cdr slots) (1- n-wildcards))
                                    )))
                     ((eq '? (car slots))
                      (remove nil
                              (loop for sub-asl in alist
                                  if (> n-wildcards 1)   ;; not the last one
                                  append (when (consp sub-asl)
                                           (ma (cdr sub-asl) (cdr slots) (1- n-wildcards)))
                                    else
                                  collect (when (consp sub-asl)
                                            (ma (cdr sub-asl) (cdr slots) (1- n-wildcards)))
                                    )))
                     
                     (t
                      (ma (cdr (massoc (car slots) alist)) (cdr slots) n-wildcards)
                      )))
           )
    (ma alist slots (count-if #'(lambda(x) (member x '(* ?))) slots))
    ))


