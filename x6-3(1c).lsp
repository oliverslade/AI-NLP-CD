

;----------------------------------------------------------
; RULE TRACERS
;----------------------------------------------------------

(defun rule-tracer (x)
   (if (member (rule-id x) *traced-rules*)
      (display x)))

(defun semantic-trace-in (x)
   (cond ((or(member (rule-id x) *traced-semantics*)
             (member (rule-id x) *traced-rules*))
          (format t "~%EDGE COMPLETES")
          (graph x)
          )))

(defun semantic-trace-out (x)
   (cond ((or(member (rule-id x) *traced-semantics*)
             (member (rule-id x) *traced-rules*))
          (format t "~%New Semantics")
          (pprint (semantics x))
          (format t "~%----completed edge ends----")
         )))

(defmethod trace-rule ((x list))
   (mapc #'trace-rule x)
   'ok)

(defmethod trace-rule ((x t))
   (setf *traced-rules* (cons x *traced-rules*))
   'ok)

(defmethod untrace-rule ((x list))
   (mapc #'untrace-rule x)
   'ok)

(defmethod untrace-rule ((x t))
   (setf *traced-rules* (remove x *traced-rules*))
   'ok)

(defun untrace-all-rules ()
   (setf *traced-rules* nil)
   (untrace-all-semantics) 
   'ok)

(defun trace-semantic (x)
   (setf *traced-semantics* (cons x *traced-semantics*))
   'ok)

(defun untrace-semantic (x)
   (setf *traced-semantics* (remove x *traced-semantics*))
   'ok)

(defun untrace-all-semantics ()
   (setf *traced-semantics* nil)
   'ok)


;----------------------------------------------------------
; RULE & LEXICON BUILDERS
;----------------------------------------------------------

; rule structure:
;    ( id (cat -> ..components..) ..sem-handling.. )
; eg:
;  (S1 (S -> NP VP) 
;      `((actor ,(=> NP))
;        ,(>=> VP action)
;        ,(>=> VP object) )))

; old build-new-rule used matches:
;(defun build-new-rule (text)
;   (mlet ('(?id (?cat -> ??parts) ??fn-parts) text)
;     (make-instance 'rule
;       :id       id
;       :categ    cat
;       :comps    parts
;       :fn       fn-parts
;       :fn-form  (append '(lambda(self)) fn-parts)
;     )))

#-RUNTIME-SYSTEM
(defun build-new-rule (text)
   ;; this version includes fn text
   ;; runtime version doesnt
   (and (eq (cadadr text) '->)   ; ie: text(2)(2)
        (let ((fn-parts (cddr text))
              (cat (caadr text))    ; text(2)(1)
              )
          (make-instance 'rule
            :id       (car text)
            :categ    cat
            :compt-text (cddadr text) ; text(2)(3...)
            :comps    (translate-schema (cddadr text))   ; text(2)(3...)
          ;;  :fn       fn-parts
            :fn-form  (if (check-prefix cat '$)
                         `(lambda(self)
                            (set-type-to (phrase-type ,cat))
                            ,@fn-parts)
                         `(lambda(self) ,@fn-parts))
            )
     )))

#+RUNTIME-SYSTEM
(defun build-new-rule (text)
   (error "runtime build-new-rule is buggy")
      (and (eq (cadadr text) '->)   ; ie: text(2)(2)
        (let ((fn-parts (cddr text)))
          (make-instance 'rule
            :id       (car text)
            :categ    (caadr text)    ; text(2)(1)
            :compt-text (cddadr text) ; text(2)(3...)
            :comps    (translate-schema (cddadr text))   ; text(2)(3...)
            ;;  dont duplicate fn text
            ;;  :fn       fn-parts
            :fn-form  (eval `(function
                               ,(append '(lambda(self)) fn-parts)))
            )
     )))
 
(defun build-grammar (rules)
   (setf *grammar* (mapcar #'usr-grammar-rule rules)))  


;----------------------------------------------------------
; RULE SCHEMA TRANSLATION
;----------------------------------------------------------

#| TRANSLATE-SCHEMA
   1. place null alias lists where none are given
   2. translate schema lists into lists of sv-obj
   3. expand mandatory-many objects (prefixed +)
   4. for typekey from participation and typecode

|#

(defun translate-schema (slis)
   (mapcar #'generate-typekey
     (expand+types
       (translate-schema-list
         (plant-null-aliases slis) ))))


(defun plant-null-aliases (l)
   (cond ( (null l)
           nil
          )
         ( (consp (second l))  ; alias list provided
           `(,(first l) ,(second l) ,@(plant-null-aliases (cddr l)))
          )
         ( t ; no alias so plant one
           `(,(first l) nil ,@(plant-null-aliases (cdr l)))
           )
         ))

(defun translate-schema-list (s)
   (if (null s) nil
      (multiple-value-bind (part type name) (translate-one (first s))
         (cons (make-instance 'sv-obj
                 :ptcip part
                 :tcode type
                 :name  name
                 :alias (second s))
               (translate-schema-list (cddr s))
           ))))

(defun expand+types (s)
   ; s is list of sv-obj's
   (cond ((null s) nil)
         ((eq (ptcip (car s)) '+)
          (let ((opt* (make-instance 'sv-obj
                        :ptcip '*
                        :tcode (tcode (car s))
                        :name  (name (car s))
                        :alias (alias (car s)) ))
                )
             (setf (ptcip (car s)) '=)
             `(,(car s) ,opt* ,@(expand+types (cdr s)))
             ))
         ( t
           (cons (car s) (expand+types (cdr s)))
           )))

(defmethod generate-typekey ((s sv-obj))
   ; simple concatenation of ptcip & tcode
   (setf (tkey s)
         (intern (concatenate 'string
                   (string (ptcip s)) (string (tcode s)) ))
         )
   s)

#|
  MAPPING FOR CATEGORY TRANSLATION

  prefix strip-off example

   none    none    NP     =cat   NP
    $      none    $x     =svar  $x
    _      none    _x     =dvar  _x
    !      none    !the   =word  !the
    ?       ?      ?NP    ?cat   NP
    ?$      ?      ?$x    ?svar  $x
    ?_      ?      ?_x    ?dvar  _x
    ?!      ?      ?!the  ?word  !the
    *       *      *NP    *cat   NP
    *$      *      *$x    *svar  $x
    *_      *      *_x    *dvar  _x
    *!      *      *!the  *word  !the
|#

(defun translate-one (x)
  (let* ((str #- :MODERN (string-upcase (string x))
              #+ :MODERN (string x)
              )
         (ch1 (elt str 0))
         )
      (if (member ch1 '(#\* #\? #\+))
          (setf str (subseq str 1))
          (setf ch1 #\=))
      (let* ((ch2 (elt str 0))
             (key (case ch2
                    (#\! 'word)
                    (#\$ 'svar)
                    (#\_ 'dvar)
                    ( t  'cat )) ))
         (values (intern (string ch1)) key (intern str))
         )))


(defmethod svequal ((x sv-obj) (y sv-obj))
  ; checks tkey (a combination of ptcip & tcode)
  ; and name, doesn't check alias lists
  (and (eq (tkey x) (tkey y))
       (eq (name x) (name y))
       ))


;----------------------------------------------------------
; BUILDING THE LEXICON
;----------------------------------------------------------

#| lexicon building
 | this process is complicated in two ways
 | 1. word defns can refer to other word defns so require defn editing,
 | checks for circularity etc (not implemented yet)
 | 2. any and all :LXR :AUTO & :LXR :IDLE entries need compiling as Grules.
 | In the lexicon all :LXR :AUTO will be listed in a single :LXR-AUTO slot;
 | all :LXR :IDLE will be listed in a single :LXR-IDLE slot.
 |
 | Word definintion (a reminder!)
 | usr-slots any slots/expressions the user needs for handling
 |           semantics etc
 | lxr-autos lexical rules automatically loaded into the parser 
 |           when the word is looked up
 | lxr-idles lexical rules that remain idle until/unless activated
 |           by another rule
 |
 | (defclass word ()
 |     ((word      :accessor word :initarg :word)
 |      (category  :accessor cat  :initarg :cat )
 |      (usr-slots :accessor usr  :initarg :usr )
 |      (lxr-autos :accessor auto :initarg :auto :initform nil)
 |      (lxr-idles :accessor idle :initarg :idle :initform nil)
 |      ))
 |

(setf sink
`(sank verb ((sem sink-sems0)
             (:lxr :auto (Vl1 (S -> NP($a) $$ NP($o))
                         `((actor ,(=> $a))
                           (object ,(=> $o))
                           )))
             (sem sink-sems1)
             (:lxr :auto (Vl2 (S -> NP $$)
                          `((actor ,(=> $a))
                            (object null)
                           )))
             (sem sink-sems2)
             (:lxr :idle (IV1 (inf -> !to $$)
                          `((action infinitive))
                          ))
             (sem sink-sems3)
             (:lxr :idle (IV2 (neginf -> !not !to $$)
                          `((action neg-infinitive))
                          ))
             (sem sink-sems4)
             ))
  )

 | lexical defn of word is: (word cat (...assocs...))
 | assocs can contain multiple :LXR entries. These are removed
 | & dealt with. LXR entries will typically be rules in textual form 
 | BUT may be precompiled grammar rules (since if they are used in
 | more than one place it is more efficient to precompile them
 | using build new rule.
 | 
 |#
                                               
                                               
(defvar *need-refining* nil)
                                               
(defun build-lexicon (words)
  (clrhash *lextable*)
  (setf *need-refining* nil)
  (add-words-to-lexicon words)
  (mapc #'(lambda(pair)
            (lookup&refine-word-def (car pair) (cdr pair)))
    *need-refining*)
  )

(defun add-words-to-lexicon (words)
   (mapc #'add-word-def words))

(defun add-word-def (spec)
  ; (first spec) is the word
  ; (second spec) is its category
  (let* ((ule (usr-lexicon-entry spec))
         (wrd (word ule))
         (cat (cat ule))
         )
    (setf (gethash wrd *lextable*)
      (cons ule (gethash wrd *lextable*)))
    (when (member :use (usr ule) :test #'(lambda(x seq) 
                                           (and (consp seq) (eq x (car seq)))))
      ;; this one needs refining later
      (setf *need-refining* (cons (cons wrd cat) *need-refining*)))
    ))

(defun lookup&refine-word-def (word-name categ)
  ; refines a definition of a whole word
  ; NB: assumes no circularity, even the weird sort where..
  ; (foo noun) -> (bar adj) -> (foo verb)
  (let* ((defs (gethash word-name *lextable*))
         (w (remove-if-not #'(lambda(w) (eq (cat w) categ)) defs))
         )
    (setf (gethash word-name *lextable*)
      (append (mapcar #'refine-word-def w) ($- defs w)))
    ))

(defun refine-word-def (word)
  (setf (usr word) (refine-word-slots (usr word)))
  word)

; TO DO: allow use to 'inherit' :lxr's
(defun refine-word-slots (slots)
  (append 
   (remove ':use slots :key #'car)
   (reduce #'append
           (foreach ('(:use ?word ?cat ??uses) slots)
                    (let ((wlis (remove-if-not
                                 #'(lambda(w) (eq (cat w) #?cat))
                                 (progn (lookup&refine-word-def #?word #?cat)
                                   (gethash #?word *lextable*)))
                                ))
                      (when (/= 1 (length wlis))
                        (error "missing or ambiguous word in :use ~a ~a" #?word #?cat))
                      ; NB: must list all slot names in $- call because use 
                      ; of {:key #'car} in set-difference is applied to both
                      ; args & here (-> #?uses :but) gives symbolic slot names
                      ($- (usr (only wlis))
                          (mapcar #'list (-> #?uses :but)) :key #'car)
                      ))
           )))




(defun split-lexical-assocs (alis)
   (flet ((car-eq-lxr (x) (eq (car x) :lxr))
          (cdar-auto (x) (eq (cadr x) :auto))
          (cdar-idle (x) (eq (cadr x) :idle))
          (check&build-rule (x)
             (if (eq (type-of x) 'rule)
                 x
                 (build-new-rule x)))
          )
     (let* ((lxrs (remove-if-not #'car-eq-lxr alis))
            (alis (remove-if     #'car-eq-lxr alis))
            ; auto lxrs just built & listed
            (auto (mapcar #'check&build-rule
                    (mapcar #'caddr (remove-if-not #'cdar-auto lxrs))))
            ; idle lxrs built & listed
            (idle (mapcar #'check&build-rule
                    (mapcar #'caddr (remove-if-not #'cdar-idle lxrs))))
            )
        (values alis auto idle)
        )))

(defun make-word (w)
   ; word defn should be (word category semantics)
   ; typically semantics is a list (perhaps containing lxrs)
   ; but it is also valid to be symbolic
   ; No error checking here!
   (if (atom (caddr w)) ; symbolic semantics
       (make-instance 'word
         :word (car w)
         :cat  (cadr w)
         :usr  (caddr w))
       (multiple-value-bind
             (usr auto idle)
             (split-lexical-assocs (caddr w))
          (make-instance 'word
            :word (car w)
            :cat  (cadr w)
            :usr  usr
            :auto auto
            :idle idle))
      ))
   

;----------------------------------------------------------
; SYSTEM INTERFACE
;----------------------------------------------------------

; frame squasher: appends items following @ symbol

(defun squash-frame (f)
  (if* (atom f) then f   ; covers nil as well
   elseif (eq '@ (car f))
     then (append (squash-frame (cadr f))
                  (squash-frame (cddr f)))
     else (cons (squash-frame (car f))
                (squash-frame (cdr f)))
          ))


(defun sys-grammar-rule (&key id cat schema semantics)
   (make-instance 'rule
     :id       id
     :categ    cat
     :compt-text schema
     :comps    (translate-schema schema)
     :fn-form  (if (check-prefix cat '$)
                  `(lambda(self)
                     (set-type-to (categ-of self ',cat))
                     (remove nil (squash-frame
                                  ,@semantics)))
                 `(lambda(self) (remove nil (squash-frame ,@semantics))))
     ))

(defun sys-lexical-idle-rule (rule)
   ; temporarily make 'idle.<RULE> dotted pair
   ; NB: rule may already be a compiled rule
   (cons 'idle (if (eq (type-of rule) 'rule)
                  rule
                  (usr-grammar-rule rule))))


(defun sys-lexical-auto-rule (rule)
   ; temporarily make 'auto.<RULE> dotted pair
   ; NB: rule may already be a compiled rule
   (cons 'auto (if (eq (type-of rule) 'rule)
                  rule
                  (usr-grammar-rule rule))))

(defun sys-lexicon-entry (&key word cat (slots nil))
   (let ((slots (loop for s in slots
                  ; following or protects atomic case
                  collect (or (usr-lexical-slot s) s)
                  )))
      (multiple-value-bind
             (usr auto idle)
             (split-lexical-slots slots)
          (make-instance 'word
            :word word
            :cat  cat
            :usr  usr
            :auto auto
            :idle idle))
      ))

(defun split-lexical-slots (slots)
   ; this replaces fn:split-lexical-assocs
   (labels ((is-auto (x) (and (consp x)(eq (car x) 'auto)))
            (is-idle (x) (and (consp x)(eq (car x) 'idle)))
            (is-lxr (x) (or (is-auto x) (is-idle x))))
     (let ((auto (mapcar #'cdr (remove-if-not #'is-auto slots)))
           (idle (mapcar #'cdr (remove-if-not #'is-idle slots)))
           (alis (remove-if #'is-lxr slots))
           )
        (values alis auto idle)
        )))

;;; DEPRECATED
(defun sys-useword-slot (&key word cat override 
                         assoc-del slot-del slot-add)
   ; creates a useage instance which is processed later
   (make-instance 'word-use-detail
     :word           word
     :cat            cat
     :assoc-override override
     :assoc-remove   assoc-del
     :slot-remove    slot-del
     :slot-add       slot-add
   ))


