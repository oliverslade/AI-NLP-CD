


;----------------------------------------------------------
; GENERAL UTIL FNS
;----------------------------------------------------------

#| ACCEPTED X SPEC
spec: targets/components of a grammar rule/edge
x:    syntactic category
returns list of modified specs if X is possible first category 
of spec allows for use of ? and * optional categories in spec
|#


(defun categ-of (edge entry)
   ;; returns category of numbered or sv accessed entry
   ;; in (components SELF)
   ;; NB: comps is modified with dummy so sv indexing is consistent
   (let ((comps (cons 'dummy (components edge))))
      (cond ((numberp entry)
             (car (elt comps entry))
             )
            ((schema-var-name-p entry)   ; use 1st associated number
             (car (elt comps
                    (car (assoc2 entry (schema-vars edge)))
                   )))
            )))

(defun acceptable-svar-type (sv xedge accepting)
   (or (null accepting)  ; no accepting edge, no retrictions
       (eq (category xedge) (categ-of accepting (name sv)))
       ))

; NB: raw-accepted is called directly from raw-trigger-rules
; in this case there is no accepting edge & so no history of
; svar bindings

(defun raw-accepted (xedge spec &optional (accepting nil))
  (if (null spec)
    nil
    (let ((x (category xedge))
          (elem (name (car spec)))
         )
      (ccase (tkey (car spec))
        ('=cat  (and (eq elem x) `(,spec)))
        ('=dvar `(,spec))
        ('=svar (and (acceptable-svar-type (car spec) xedge accepting)
                     `(,spec)))
        ('=word (and (eq (rule-id xedge) 'word)
                     (wordeq (car (words xedge)) elem)
                     `(,spec)))
        ('?cat  (if (eq elem x)
                   (cons spec (raw-accepted xedge (cdr spec) accepting))
                   (raw-accepted xedge (cdr spec) accepting)))
        ('?dvar (cons spec (raw-accepted xedge (cdr spec) accepting)))
        ('?svar (if (acceptable-svar-type (car spec) xedge accepting)
                   (cons spec (raw-accepted xedge (cdr spec) accepting))
                   (raw-accepted xedge (cdr spec) accepting)))
        ('?word (if (and (eq (rule-id xedge) 'word)
                         (wordeq (car (words xedge)) elem))
                   (cons spec (raw-accepted xedge (cdr spec) accepting))
                   (raw-accepted xedge (cdr spec) accepting)))
        ('*cat  (if (eq elem x)
                   (cons `(,(car spec) ,@spec)
                     (raw-accepted xedge (cdr spec) accepting))
                   (raw-accepted xedge (cdr spec) accepting)))
        ('*dvar (cons `(,(car spec) ,@spec)
                  (raw-accepted xedge (cdr spec) accepting)))
        ('*svar (if (acceptable-svar-type (car spec) xedge accepting)
                   (cons `(,(car spec) ,@spec)
                     (raw-accepted xedge (cdr spec) accepting))
                   (raw-accepted xedge (cdr spec) accepting)))
        ('*word (if (and (eq (rule-id xedge) 'word)
                         (wordeq (car (words xedge)) elem))
                   (cons `(,(car spec) ,@spec)
                     (raw-accepted xedge (cdr spec) accepting))
                   (raw-accepted xedge (cdr spec) accepting)))
        ))))

(defun accepted (incoming accepting)
   ; incoming is edge to be accepted/rejected by targets
   ; of accepting, accepting is edge with active targets
   (raw-accepted incoming (targets accepting) accepting))


(defmethod wordeq ((w1 word) w2!)
  ;; w2! is prefixed by '!
  ;; numbers wont coerce so cannot compare
  ;; this is a restriction
  (if (numberp (word w1)) nil
   (string= (string (word w1)) (subseq (string w2!) 1))))

(defmethod wordeq ((weird t) x) (declare (ignore x)) nil)

(defun add-assoc (key val alist)
   (cons `(,key ,(cons val (assoc2 key alist)))
         (remove key alist :key #'car)) )

(defun assoc2 (key alist)
   (second (massoc key alist)))

(defun get-keyarg-val (arglist key otherkeys)
   ; gets values of key args in listed form ie:
   ;> (get-keyarg-val '(s d g :x y z :a b c) ':x '(:a :b))
   ;(Y Z)
   (let* ((start (cdr (member key arglist)))
          (stop (position-if
                  #'(lambda (x) (member x otherkeys))
                  start))
          )
      (subseq start 0 stop)))

(defun all-optional (lis)
   ;; true if all categories in list are optional
   ;; ie: prefixed by ? or *
   (every #'(lambda(x) (optional-ptcip x))
          lis))

(defun update-sv-list (sv-obj n sv-asl)
   ; update any s/dvars (sv-obj itself or aliases)
   ; to reference n in sv-asl
   (if (schema-var-p sv-obj)
       (setf sv-asl (add-assoc (name sv-obj) n sv-asl)))
   (dolist (x (alias sv-obj))
      (setf sv-asl (add-assoc x n sv-asl))
      )
   sv-asl)


;----------------------------------------------------------
; CHART PARSER UTILITIES
;----------------------------------------------------------

; lookup word etc reform words as lists (rather than clos
; objects). The reason is partly historic, partly to free
; parser from using complex clos objects with information overkill

(defun find-definition (word)
   (or (lookup-word word)    ; try to find it
       (unknown-word word))  ; default for unknown words
   )

(defmethod lookup-word ((word symbol))
   (gethash word *lextable*))

(defmethod lookup-word ((n number))
   ; return the standard numeric form
   (list (make-instance 'word
           :word n 
           :cat 'number
           :usr `((value . ,n) (type number))
           )))

(defmethod lookup-word ((weird t))
   ; not a symbol or a number - return nil
   nil)

(defun unknown-word (w)
  #-DDE (format t "~&word not found in lexicon: ~a~&" w)
   #-DDE (list (make-instance 'word 
                 :word w 
                 :cat 'undefined
                 :usr *unknown-word-slots*))
   #+DDE (throw 'DDE-parse-error
            `(error (cause unrecognised-word)
                    (doing find-definition)
                    (involving ,w)))
   )

(defun rev-cons (lis atm)
   (append lis (list atm)))

(defun activep (e)
   (eq (type-of e) 'active-edge))

(defun complete (e)
   (eq (type-of e) 'complete-edge))

(defun specifier (e)
   (eq (type-of e) 'specifier-edge))

(defun fail(x)
   (eq x 'fail))

;----------------------------------------------------------
; CHART PARSER FNS
;----------------------------------------------------------

(defun main-loop ()
   (do* ( (next (car *unexplored*) (car *unexplored*))
          new-edges )
        ( (null *unexplored*) )
      ;; following is carried out as discrete steps to 
      ;; allow other fns to modify *unexplored*
;;; (pprint 'main-loop) (graph next) (pprint *unexplored*)
      (setf *unexplored* (cdr *unexplored*))
      (setf new-edges (trigger-new-edges next))
      (push-on-unexplored new-edges)
      (setf *edge-list* (cons next *edge-list*))
      ))

(defmethod trigger-new-edges ((edge edge))
   (loop
     for x in *edge-list*
       append (trigger edge x)
     ))

(defmethod trigger-new-edges ((edge complete-edge))
   (nconc (call-next-method edge)
          (trigger-rules edge)))


(defun trigger-rules (edge)
   ; ?* compatible version
   (loop
     for new-edge in (raw-trigger-rules edge)
       append (completion-of-?* new-edge)
     ))

#| RAW-TRIGGER-RULES EDGE
used with ?*rules
collects raw rule-edges triggered by supplied edge
NB: raw edges are not checked for completion
|#

(defun raw-trigger-rules (edge)
   (loop for rule in *grammar*
     append
       (loop for spec in (raw-accepted edge (components rule))
         collect
            (make-instance 'active-edge
              :rule    (rule-id rule)
              :categ   (category rule)
              :words   (words edge)
              :glitches (glitches edge)
              :startpt (startpt edge)
              :endpt   (endpt edge)
              :comps   (list (components edge))
              :sems    `(,(category edge) ,(semantics edge))
              :targets (cdr spec)
              :fn      (fn-form rule)
              :schema-vars (update-sv-list (car spec) 1 nil)
              )
      )))

(defmethod trigger ((a edge)(b edge))
   ; default
   nil)

(defmethod trigger ((edge active-edge) (x complete-edge))
   (if (and (= (endpt edge) (startpt x))
            (accepted x edge))
       (combine-edges edge x)))

(defmethod trigger ((edge complete-edge) (x active-edge))
   (if (and (= (endpt x) (startpt edge))
            (accepted edge x))
       (combine-edges x edge)))

(defmethod trigger ((edge complete-edge) (x specifier-edge))
   (if (and (= (endpt edge) (startpt x))
            (accepted edge x))
       (combine-edges edge x)))

(defmethod trigger ((edge specifier-edge) (x complete-edge))
   (if (and (= (endpt x) (startpt edge))
            (accepted x edge))
       (combine-edges x edge)))


(defmethod combine-edges ((x edge) (y edge))
   ;; generic, over-ridden for specifier edges
   ;; make a new edge from two others
   ;; NB: assume y immediately follows x
   (loop for spec in (accepted y x)
     append
      (completion-of-?*
        (make-instance 'active-edge    ; corrected later if neccess.
          :rule    (rule-id x)
          :categ   (category x)
          :words   (append (words x) (words y))
          :glitches (append (glitches x) (glitches y))
          :startpt (startpt x)
          :endpt   (endpt y)
          :comps   (rev-cons (components x) (components y))
          :sems    (rev-cons (semantics x) (semantics y))
          :targets (cdr spec)
          :fn      (fn-form x)
          :schema-vars (update-sv-list (car spec)
                                       (length (semantics x))
                                       (schema-vars x))
                   ))))

(defmethod combine-edges ((x edge) (s specifier-edge))
   ;; specific for specifier edges
   ;; make a new edge from two others
   ;; NB: assume s immediately follows x
   (loop for spec in (accepted x s)
     append
      (completion-of-?*
        (make-instance 'specifier-edge    ; corrected later if neccess.
          :rule    (rule-id s)
          :categ   (category s)
          :words   (append (words x) (words s))
          :glitches (append (glitches x) (glitches s))
          :startpt (startpt x)
          :endpt   (endpt s)
          :comps   (rev-cons (components s) (components x))
          :sems    (rev-cons (semantics s) (semantics x))
          :targets (cdr spec)
          :fn      (fn-form s)
          :specifiers  (specifiers s)
          :complements (complements s)
          :schema-vars (update-sv-list (car spec)
                                       (length (semantics s))
                                       (schema-vars s))
                   ))))

(defun completion-of-?* (edge)
   (cond ((null (targets edge))
          (list (check-completion edge))
          )
         ((all-optional (targets edge))
          (let ((e2 (copy-obj edge)))
             (setf (targets e2) nil)
             (list edge
                   (check-completion e2))
             ))
         (t (list edge))
         ))

(defmethod check-completion ((edge active-edge))
   ;; if edge is complete mark it so & run fn
   ;; return edge (modified or otherwise)
   (when (null (targets edge))
         #+TRACE-ON (semantic-trace-in edge)
         (change-class edge 'complete-edge)
         (let ( (f (catch 'fail 
                          (funcall (fn-form edge) edge)
                     )))

            (if (fail f)   ; catch fails on (throw 'fail 'fail)
                (change-class edge 'failed-edge)
                (setf (semantics edge) (cons (category edge) f))
               ))
         (setf (components edge)
              (cons (category edge) (components edge)))
         #+TRACE-ON (semantic-trace-out edge)
      )
   edge)

(defmethod check-completion ((e specifier-edge))
   ;; if edge has completed it becomes an active-edge
   ;; see associated documentation
   (cond ((null (targets e))
          (setf (semantics e) (cons (category e) (reverse (semantics e))))
          (setf (components e) (reverse (components e)))
          (setf (targets e)    (complements e))
          ; now remap all  SV -> P int  mappings
   #|       (setf (schema-vars e)
                (let ((k (1+ (length (semantics e)))))
                   (mapcar #'(lambda (sv)
                               `(,(car sv)       ; the sv name
                                 ,(mapcar #'(lambda (n) (- k n))
                                    (second sv)   ; the sv numeric mappings
                                    )))
                     (schema-vars e)
                     )))
     |#     (change-class e 'active-edge)
          (check-completion e)   ; it's an active-edge now!
          )
         (t e)  ;; otherwise return it as is
         ))


;----------------------------------------------------------
; CHART PARSER - LEXICAL RULES
;----------------------------------------------------------


(defun split-components (comps)
   (let ((posn (position *sv$$* comps :test #'svequal)))
      (values          ; return multiple values
        (1+ posn)                     ; 1st binding is *sv$$* index
        (subseq comps 0 posn)         ; 2nd binding is specifiers
        (subseq comps (1+ posn))      ; 3rd binding is complements
        )))

(defun trigger-lexical-rule (rule edge)
   (multiple-value-bind ($$ind specs cmpts) (split-components (components rule))
      (let ((new-edge
              (make-instance 'specifier-edge
                :rule        (rule-id rule)
                :categ       (category rule)
                :words       (words edge)
                :startpt     (startpt edge)
                :endpt       (endpt edge)
                :comps       (list (components edge))
                :sems        (list (semantics edge))
                :targets     specs
                :fn          (fn-form rule)
                :schema-vars `(($$ (,$$ind)))
                :specifiers  specs
                :complements cmpts
                )))
         (check-completion new-edge)
         )))


;----------------------------------------------------------
; CHART PARSER - PASS 1
;----------------------------------------------------------

(defun pass1 (sentence)
   ;; make initial set of edges from the lexicon
   (do* ( (i 0 (1+ i))
          (word (car sentence) (car sentence))
          (sentence (cdr sentence) (cdr sentence))
         )
        ((null word))
      (append-on-unexplored
        (mapcar
          #'(lambda (form)
              (let ((new-edge
                      (make-instance 'complete-edge
                        :rule    'word
                        :categ   (cat form)
                        :words   `(,form)
                        :startpt i
                        :endpt   (1+ i)
                        :comps   `(,(cat form) ,word)
                        :sems    (cons (cat form) (usr form))
                        :targets nil
                        :fn      nil
                        )))
                 (when (auto form)
                    (append-on-unexplored
                      (mapcar #'(lambda (lxr)
                                  (trigger-lexical-rule lxr new-edge))
                              (auto form))
                      ))
                 new-edge))
          (find-definition word)))))


;----------------------------------------------------------
; CHATTY & TRACE HANDLERS
;----------------------------------------------------------

#+TEST
(defun chatty (keys &rest args)
   (if (intersection keys *chatty*)
      (dolist (a args)
         (print a))
      ))


;----------------------------------------------------------
; DISPLAY METHODS
;----------------------------------------------------------

#+TEST
(defmethod display ((e edge))
   (format t "~&~a ~a ~a ~a ~a~20t~a~40t~a"
     (type-of e)
     (startpt e) (endpt e)
     (rule-id e) (category e)
     (mapcar #'word (words e))
     (targets e))
   (if (glitches e)
      (format t "~&~5tGlitches: ~a" (glitches e))))

#+TEST
(defmethod display ((r rule))
   (format t "~&~a~5t~a -> ~a" (rule-id r) (category r) (compt-text r)))

#+TEST
(defmethod display ((lis list))
   (mapc #'display lis)
   t)

#+TEST
(defmethod display ((x sv-obj))
   (format t "~&~a ~a <~a>, ~a ~a"
     (ptcip x)
     (tcode x)
     (tkey  x)
     (name  x)
     (alias x)
     ))

#+TEST
(defmethod graph ((e edge))
   ;; complete display of edge & all components
   (print '_____________)
   (display e)
   (display (find-if #'(lambda (r)
                         (eq (rule-id r) (rule-id e)))
              *grammar*))
   (format t "~%Syntax")
   (pprint (components e))
   (format t "~%Semantics")
   (pprint (semantics e))
  ; (print '_____________)
   t)

#+TEST
(defmethod graph ((e specifier-edge))
   ;; bit messy but what to do
   (call-next-method e)
   (format t "~%Specifiers & Complements: ~a ~a"
     (specifiers e) (complements e))
   )

#+TEST
(defmethod graph ((l list))
   (mapc #'graph l))

;----------------------------------------------------------
; TOP LEVEL CONTROL
;----------------------------------------------------------

#+TEST
(defun ptest (s)
  (setf *unexplored* nil
     *edge-list*  nil)
  (pass1 s)
  (main-loop)
  (print 'EDGELIST>>)
  (display *edge-list*)
  t)


(defun parse* (cat s)
   (setf *unexplored* nil
         *edge-list*  nil)
   (pass1 s)
   (main-loop)
   (setf *parses* 
         (remove-if-not #'(lambda (e)
                            (and (eq (category e) cat)
                                 (complete e)
                                 (= (startpt e) 0)
                                 (= (endpt e) (length s))
                                 ))
                         *edge-list*)))


#+TEST
(defun parse (cat s)
   (parse* cat s)
   (mapc #'graph *parses*)
   (and *parses* t))

