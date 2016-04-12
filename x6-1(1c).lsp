

#|
revisions
boris.12
    minor repackage & loader change
scl.98+4
    1. replace calls to elt with xelt to bug-fix move to CL6
    2. test use of general matcher to replace older version wrapped with Lkit
    3. modify components formation of check-completion(active-edge) and pass1
       (section re: word -> initial edge) to produce less 'listed' syntax
       output from parser
    4. glitch-test-macro now returns nil to prevent noise in semantics of rules
    5. sys-grammar-rule removes nils from semantics
scl.98(7)
    1. translation of rule categories at load time for efficiency
       & adjustments made so multiple possibilities are searched on if
       accepted finds different valid edges IE: rules like (s -> a *_X *_Y)
       work as they should on sentences like '(A1 B1 B2)
    2. bug fix on numbered slots in => macro
    3. problem noticed: how to reference the semantics of multipley named category
       in a schema of unknown length eg: A in the following: (s -> A ?_X ?_Y A) 
       cannot refer to 2nd A as slot 4 cause it might not be!
       A way around it is:
              (r4 (s -> a ?_X ?_Y a) 
                    `((A ,(let* ((both (ALL=> A))
                                 (1stA (first both))
                                 (2ndA (second both)))
                            `(1st= ,1stA 2nd= ,2ndA)))
                      (X ,(for-each _X *IT*))
                      (Y ,(for-each _Y *IT*)) ))

       With sample lexicon: '((a1 A sa1) (a2 A sa2) (a3 A sa3) (a4 A sa4)
                              (b1 B sb1) (b2 B sb2) (b3 B sb3) (b4 B sb4))
       BUT... nested backquote & comma & lets are going to upset casual users!
    4. lexicon held as hash table
scl.98(6)
    1. -> Xchart5d modified structure of (fn-form edge) so
       check-completion does not need to access the compiler.
       (fn-form edge) is now a compiled fn.
    2. duplicate version of build-new-rule for runtime version
       & adaptation of *features* to make use of runtime features
scl.98(5)
    1. efficiency modification to accepted
scl.98(4)
    1. modification of ALL=> to support SV's
scl.98(3) Xchart4?.lsp -> Xchart5a.lsp
    incorporates the extra fns, macros & defs for using ?* options
    - these were handled as a set of redefs loaded over Xchart4?
    also incorporates SCHEMA VARIABLES see following notes
scl.98(2) Xchart4a.lsp -> Xchart4b.lsp
    1. class alterations (i) edge :initarg for :glitches to simplify copy
       (ii) edge schema-vars slot
    2. redefn of => macro to allow schema-vars
scl.98(1)
    1. fail-test-macro used by fail-if etc returns 'fail
       check-completion tests for this
    2. pass1 now uses find-definition which can deal with numbers
    3. parse* built for parse & DDE controlller
    4. use of *features* added
    5. use of matcher removed (to reduce size of compiled image)
       required redefn of build-new-rule


RE: SCHEMA VARS (SVs)
1. SVs are always prefixed with underscore, this is not stripped off
2. SVs are associated with ELT no.s of corresponding entries in
   (semantics self)
3. if rule is triggered with match to an SV this always corresponds to
   (elt (semantics self) 1) elt0 is category of current rule
4. when active edge is extended by matching SV this corresponds to
   (elt (semantics self) (length (semantics old-active-edge))
5. optional SVs (IE: ?_X and *_Y) should be used with caution & will
   cause a fault if used as the first target in a rule (since this
   generates endless edge production).
   Caution is needed for 2 reasons both relating to efficiency
   (i) use of ?* options are costly (in terms of edge production) this
       is restricted when they are category specific but unrestricted
       (& so v.costly) with ?*/SV forms
   (ii) ?*/SV can be ambiguous requiring generation of muliple edges
       (ie: search) to resolve this. This search is not implemented so
       it only makes sense to use ?*/SV as the last syntactic category
       in a rule. Simple sentences can cause explosive
       edge production with a couple of poorly built rules.


RECOGNISED *FEATURES*
   :DDE  assumes parser is used as server via MS windows DDE link
   :TEST without DDE it sets up stuff for interactive calls to parse
        & utilities for building grammar rules: chatty etc
   :USE-SV to use ?* and schema vars
   :RUNTIME-SYSTEM minimal build for compiled version
|#

;; include this by default
(setf *features* (append '(:TEST :USE-SV :TRACE-ON) *features*))
#-runtime-system (setf *features* (cons ':TEST *features*))

;----------------------------------------------------------
; CLASS DEFINITIONS
;----------------------------------------------------------

(defclass rule ()
    ((rule-id    :accessor rule-id    :initarg :id    )
     (category   :accessor category   :initarg :categ )
     (components :accessor components :initarg :comps )
     (components-text 
                 :accessor compt-text :initarg :compt-text )
     (fn-text    :accessor fn-text    :initarg :fn    )
     (fn-form    :accessor fn-form    :initarg :fn-form) ; remove initarg!!!
    ))

(defclass edge ()
    ((rule-id     :accessor rule-id     :initarg :rule   )
     (category    :accessor category    :initarg :categ  )   ; syntactic category
     (state       :accessor state       :initarg :state  )   ; active/complete/fail
     (glitches    :accessor glitches    :initarg :glitches :initform nil    )
     (words       :accessor words       :initarg :words  )
     (startpt     :accessor startpt     :initarg :startpt)
     (endpt       :accessor endpt       :initarg :endpt  )
     (components  :accessor components  :initarg :comps  )
     (semantics   :accessor semantics   :initarg :sems   )
     (targets     :accessor targets     :initarg :targets)
     (fn-form     :accessor fn-form     :initarg :fn     )
     (schema-vars :accessor schema-vars :initarg :schema-vars :initform nil )
    ))

(defclass word ()
    ((word      :accessor word :initarg :word)
     (category  :accessor cat  :initarg :cat )
     (usr-slots :accessor usr  :initarg :usr )
     (lxr-autos :accessor auto :initarg :auto :initform nil)
     (lxr-idles :accessor idle :initarg :idle :initform nil)
     ))

#|
    participation for sv-obj's is...
        ?    optional, single
        *    optional, many
        +    mandatory, many
        =    mandatory, single
|#

(defclass sv-obj ()
    ; note use of typecode & type key. The typecode is as
    ; in supporting documentation ie: svar, dvar, word etc
    ; type key is a combination of participation and typecode
    ; used to optimise a couple of functions
    ((participation :accessor ptcip :initarg :ptcip)
     (typecode      :accessor tcode :initarg :tcode)
     (typekey       :accessor tkey  :initarg :tkey )
     (name          :accessor name  :initarg :name )
     (aliases       :accessor alias :initarg :alias)
     ))

;;; DEPRECATED
(defclass word-use-detail ()
    ((word           :accessor :word           :initarg :word           )
     (cat            :accessor :cat            :initarg :cat 
                     :initform nil)
     (assoc-override :accessor :assoc-override :initarg :assoc-override )
     (assoc-remove   :accessor :assoc-remove   :initarg :assoc-remove   )
     (slot-remove    :accessor :slot-remove    :initarg :slot-remove    )
     (slot-add       :accessor :slot-add       :initarg :slot-add       )
     ))

(defclass failed-edge    (edge) ())
(defclass complete-edge  (edge) ())
(defclass active-edge    (edge) ())
(defclass specifier-edge (edge)
     ((complements :accessor complements :initarg :complements)
      (specifiers  :accessor specifiers  :initarg :specifiers)
   ))


(defmethod copy-obj ((e edge))
   ;; generic for all edges
   (make-instance (type-of e)
            :rule        (rule-id e)
            :categ       (category e)
            :glitches    (glitches e)
            :words       (words e)
            :startpt     (startpt e)
            :endpt       (endpt e)
            :comps       (components e)
            :sems        (semantics e)
            :targets     (targets e)
            :fn          (fn-form e)
            :schema-vars (schema-vars e)
            ))

(defmethod copy-obj ((e specifier-edge))
   (let ((new (call-next-method e)))
      (setf (specifiers new) (specifiers e))
      (setf (complements new) (complements e))
      new
      ))


(defparameter *unknown-word-slots* nil)  ; default slots for unrecognised words
(defparameter *SV-tcodes* '(svar dvar word))
(defparameter *SV-strict-tcodes* '(svar dvar))
(defparameter *optional-ptcip* '(? *))
(defparameter *sv$$*  (make-instance 'sv-obj
                         :name  '$$
                         :ptcip '=
                         :tcode 'svar
                         :tkey  '=svar
                         :alias nil))

;; *lextable* is lexicon hash table
(defvar *lextable* (make-hash-table :test #'eq))

(defvar *unexplored* nil)
(defvar *edge-list* nil)
(defvar *parses*    nil)
(defvar *chatty*    nil)
(defvar *grammar*   nil)
(defvar *traced-rules*     nil)
(defvar *traced-semantics* nil)

;; *IT* is used in for-each, if-there-is & IT=>
(defvar *IT*)

;----------------------------------------------------------
; INLINE FNS & DEFS FOR MACROS
;----------------------------------------------------------


(defun check-prefix (sym prefix)
   ;; called with eg (check-prefix '??spud '??) returns T/nil
   ;; if symbol is not prefixed correctly then
   ;; nil is returned. 
   (and (symbolp sym)
        (let* ((prestr (string prefix))
               (symstr (string sym))
               (prelen (length prestr))
               (symlen (length symstr))
              )
          (and (> symlen prelen)
               (string= prestr (subseq symstr 0 prelen))
               ))))


(declaim (inline
          only
          xelt
          char-prefix
           schema-var-p
           strict-schema-var-p
           optional-ctype
           ))

(defun only (lis)
  ; takes only element from a list, checks for errors
  ; NB: still ok with null list though
  (when (cdr lis) ; more than one element
    (error "multiple element list in only: ~a" lis))
  (car lis))

(defun xelt (seq i)
  (when (and (numberp i) (>= i 0))
    (elt seq i)
    ))

(defun schema-var-name-p (x)
   ; _x, $x & !blah type entries are treated like SVs
   (and (symbolp x)
       (let ((1stch (elt (string x) 0)))
          (or (char= 1stch #\_)
              (char= 1stch #\$)
              (char= 1stch #\!))
          )))

(defun schema-var-p (x)
   ; _x & !blah type entries are treated like SVs
   (member (tcode x) *SV-tcodes*))

;(defun strict-schema-var-consp (x)
;   ; only _x entries are treated as strict SVs
;   (member (tcode x) *SV-strict-tcodes*))

(defun optional-ptcip (x)
   (member (ptcip x) *optional-ptcip*))

;----------------------------------------------------------
; SLOT ACCESSOR MACROS & FNS
;----------------------------------------------------------

(defmacro => (&rest args)
   ;; returns slot value of named slot
   ;; assumes only one slot exists with given name
   ;; accepts use of schema var for 1st named slot
   ;; accepts numbered value
   (let ((primary (car args))
         (slots (cdr args))
         )
      (cond ((numberp primary)
             `(multiple-assoc (cdr (xelt (semantics self) ,primary)) ',slots)
             )
            ((schema-var-name-p primary)   ; use 1st associated number
             `(multiple-assoc
                (cdr (xelt (semantics self) (car (assoc2 ',primary (schema-vars self)))))
                ',slots)
             )
            ( t
              `(multiple-assoc (semantics self) ',args)
              )
            )))


(defmacro >=> (&rest slots)
   ;; returns entire slot (name value)
   `(cons ',(car (last slots)) (=> ,@slots)))


(defmacro ALL=> (&rest slots)
   #| returns all slot values for named slots
      only terminal slot may be multiple
      2 cases handled by macro, others handled by called fns
      these are (i) there is only one slot given & it's an SV
      (ii) anything else. NB: single numbered slot is not catered
      for cos it's daft to use one.
   |#
   (if (and (= (length slots) 1) (schema-var-name-p (car slots)))
      `(mapcar #'cdr
         (mapcar #'(lambda (n) (xelt (semantics self) n))
                 (assoc2 ',(car slots) (reverse (schema-vars self)))
           ))  ; above use of reverse is a bit of a hack, SVs have
               ; multiple slot values assembled in reverse - normally
               ; this doesn't matter but here it's re-reversed so
               ; output is ordered as users would expect
      `(mapcar #'cdr
         (remove-if-not
            #'(lambda (x)
                 (and (consp x)  ; valid rules can create atoms here
                      (eql (car x) ',(car (last slots))) ))
            (=> ,@(butlast slots))
        ))))

;; extra macros for ?* forms

;; for-each & if-there-is etc bind *IT* variable

(defvar @ nil)  ;; HACK FOR EMERGENCY RELEASE OF LKIT UNDER LISP 5
;; try later (defvar @ '@)

(defmacro for-each (slot &rest body)
   ; this WAS a nice simple macro but users wanted facility to
   ; append (instead of collect) results, this is indicated by
   ; '@ before last Sexp in body
   ; ie: (for-each ADJ @(IT=> sem))    use append
   ;     (for-each ADJ (IT=> sem))     use collect
   ; NB: no need to remove @ it won't affect progn
   (if (eq '@ (xelt body (- (length body) 2)))
      `(loop for *IT* in (ALL=> ,slot)
         append (progn ,@body))
      `(loop for *IT* in (ALL=> ,slot)
         collect (progn ,@body))
      ))

;; NB: if-there-is should be used ,@(if-there-is ...) to prevent
;; nil's occurring when there-isn't. IE: use the @

(defmacro if-there-is (slot &rest body)
   ; see for-each comment
   (if (eq '@ (xelt body (- (length body) 2)))
      `(when (setf *IT* (=> ,slot)) (progn ,@body))
      `(when (setf *IT* (=> ,slot)) (list (progn ,@body)))
      ))

(defmacro if-there-isnt (slot &rest body)
   ; see for-each comment
   (if (eq '@ (xelt body (- (length body) 2)))
      `(unless (setf *IT* (=> ,slot)) (progn ,@body))
      `(unless (setf *IT* (=> ,slot)) (list (progn ,@body)))
      ))

(defmacro IT=> (&rest args)
   `(multiple-assoc *IT* ',args))
 
(defmacro phrase-type (sv-name)
   `(categ-of self ',sv-name))

(defmacro set-type-to (categ)
   ;; needs macro for scoping of SELF
   `(setf (category self) ,categ))

;;; MASSOC IS A HACK FOR FAST RELEASE OF LKIT UNDER LISP5
(defun massoc (slot asl)
  (find-if #'(lambda (x) (and (consp x) (eq (car x) slot))) asl))

#| ---------------------------------------------------------
      multiple-assoc redefined now in lk2 rule iface
   
(defun multiple-assoc (alist slots)
   ;; alist is hierarchical association list, slots represents route
   ;; through the hierarchy
   (if (null slots)
       alist
       (multiple-assoc (cdr (massoc (car slots) alist)) (cdr slots))
    ))
------------------------------------------------------------- |#

(defun assoc-all (key alist)
   (remove-if-not #'(lambda (x) (eq key (car x))) alist))


(defmacro glitch-test-macro (test-clause test glitch-sym)
   `(,test-clause ,test
                  (setf (glitches self) (cons ,glitch-sym (glitches self)))
                  nil   ; return nil to avoid disruption to semantics
     ))


(defmacro glitch-if (test sym)
    `(glitch-test-macro when ,test ,sym))

(defmacro glitch-if-not (test sym)
    `(glitch-test-macro unless ,test ,sym))


(defmacro fail-test-macro (test-clause test)
    `(,test-clause ,test
        (throw 'fail 'fail)
    ))

(defmacro fail-if (test)
    `(fail-test-macro when ,test))

(defmacro fail-if-not (test)
    `(fail-test-macro unless ,test))

(defmacro fire-lexical-rule (categ)
   `(lexical-trigger ',categ self))

(defun lexical-trigger (label edge)
   (let ((lxr-idles (idle (word-of edge label))))
      (when lxr-idles
         (push-on-unexplored
           (mapcar #'(lambda (lxr)
                       (trigger-lexical-rule lxr edge))
             lxr-idles))
         )))

(defun append-on-unexplored (lis)
   (setf *unexplored* (nconc *unexplored* lis))
   t)

(defmethod queue-on-unexplored ((e edge))
   ; NB: *unexplored* is always nconc'd for economy
   (setf *unexplored* (nconc *unexplored* (list e)))
   t)  ; return T to prevent upset when called from a rule

(defmethod queue-on-unexplored ((edges list))
   ; NB: *unexplored* is always nconc'd for economy
   ; this assumes a list of edges
   (setf *unexplored* (nconc *unexplored* edges))
   t)  ; return T to prevent upset when called from a rule

(defmethod stack-on-unexplored ((e edge))
   (setf *unexplored* (cons e *unexplored*))
   t)  ; return T to prevent upset when called from a rule

(defmethod stack-on-unexplored ((edges list))
   ; nconc is used here also
   (setf *unexplored* (append edges *unexplored*))
   t)  ; return T to prevent upset when called from a rule

(defun push-on-unexplored (x)
   ;; the default is breadth first
#+TRACE-ON (mapc #'rule-tracer x)
   (queue-on-unexplored x))
