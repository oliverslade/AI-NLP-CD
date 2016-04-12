

;----------------------------------------------------------
; PROGRAMMER INTERFACE
;----------------------------------------------------------



#|
NB: *c-names* (used throughout) are the names of all component parts of a rule
without any ?/* qualifiers but including any $x or _x forms
so a rule (xx -> *NP ?VP($x))
would have c-names (NP VP $x)

c-names is produced by "borrowing" fn translate-schema - see utils below
|#


;=========================================
; lexicon interface
;=========================================

;(makunmatch 'usr-grammar-rule)
;(makunmatch 'usr-lexicon-entry)
;(makunmatch 'usr-lexical-slot)

(defmatch usr-lexicon-entry ((?word ?cat))
  ; word only has syntactic category provided
  (sys-lexicon-entry
    :word #?word
   :cat  #?cat
   :slots '(undef)  ; this is necessary to enable words with no semantics
                    ; to be checked for existence (if there is etc) in rules
                    ; by using something like (=> word)
   ))

(defmatch usr-lexicon-entry ((?word ?cat ?(slot atom)))
  ; word has an atomic extended definition
  (sys-lexicon-entry
    :word  #?word
    :cat   #?cat
    :slots (list #?slot)
   ))

;(defmatch usr-lexicon-entry ((?word ?cat ?slots))
;  ; definition of word as single list
;  (sys-lexicon-entry
;    :word  #?word
;    :cat   #?cat
;    :slots #?slot
;   ))

(defmatch usr-lexicon-entry ((?word ?cat ??slots))
  ; definition of word as listed slots
  (sys-lexicon-entry
    :word  #?word
    :cat   #?cat
    :slots #?slots
   ))

(defmatch usr-lexicon-entry ((==))
  ; anything else is illegal
  (error "badly constructed lexicon entry ~a" #?*match))


(defmatch usr-lexical-slot ((:lxr :idle ?rule))
  (sys-lexical-idle-rule #?rule))

(defmatch usr-lexical-slot ((:lxr :auto ?rule))
  (sys-lexical-auto-rule  #?rule))

(defmatch usr-lexical-slot ((:lxr ==))
  (error "badly constructed lexical rule ~a" #?*match))

;;; DEPRECATED
#|
(defmatch usr-lexical-slot ((:use ?word :cat ?cat ??use-slots))
  (sys-useword-slot
   :word     #?word
   :cat      #?cat
   :override (get-keyarg-val #?slots ':but '(:add :del :but))
   :slot-del (get-keyarg-val #?slots ':del '(:add :del :but))
   :slot-add (get-keyarg-val #?slots ':add '(:add :del :but))
   ))

(defmatch usr-lexical-slot ((:use ?word ??use-slots))
  (sys-useword-slot
   :word     #?word
   :override (get-keyarg-val #?slots ':but '(:add :del :but))
   :slot-del (get-keyarg-val #?slots ':del '(:add :del :but))
   :slot-add (get-keyarg-val #?slots ':add '(:add :del :but))
   ))

(defmatch usr-lexical-slot ((:use ==))
  (error "badly constructed lexical use slot ~a" #?*match))
|#

(defmatch usr-lexical-slot ((==))
  ; otherwise just return the slot
  #?*match)



;=========================================
; grammar interface
;=========================================

;(makunmatch 'usr-grammar-rule)
;(makunmatch 'usr-lexicon-entry)
;(makunmatch 'usr-lexical-slot)

(defvar *c-names*)

(defmatch usr-grammar-rule ((?id (?cat -> ??schema) ??semantics))
  (setf *c-names* (make-c-names #?schema))  ; re-made with each grammar rule
  (sys-grammar-rule
   :id        #?id
   :cat       #?cat
   :schema    #?schema
   :semantics (parse-slots #?semantics *c-names*)
   ))

(defmatch usr-grammar-rule ((==))
  (error "badly constructed rule ~a" #?*match))



;=========================================
; declarations for semantics interface
;=========================================

(defvar *special-slots*)
(setf *special-slots* '(fail glitch exists !exists lisp do for quote if))
(defun special-slotp (x) (member x *special-slots*))

(defvar *special-values*)
(setf *special-values* '(it))
(defun special-valuep (x) (member x *special-values*))

(defvar *operators*)
(setf *operators*
  '((=  . equal)
    (/= . (lambda(x y) (not (equal x y))))
    ))


;=========================================
; semantics level matches
;=========================================

(defmatch rule-semantics( nil )
  nil)

;(defmatch rule-semantics( (?only-one) c-names)
;  (list (rule-semantic-slot #?only-one c-names)))

(defmatch rule-semantics( (??rest ?last) )
  (nconc (mapcar #'(lambda (r) (rule-semantic-slot r))
           #?rest)
         (list (rule-semantic-slot #?last))
         ))


;=========================================
; rule semantic slots
;=========================================

; this is now dealt with in the slot-rewrite mechanism
; deemed so fundamental to the v2 interface that it isnt
; handled by a matcher layer

(defmatch rule-semantic-slot( ?x )
  #?x)


;=========================================
; special slots - fails & glitches
;=========================================
; (makunmatch  'rule-special-slot)

;; atomic expr

(defmatch rule-special-slot( (fail if not ?expr) )
  `(fail-if-not ,(parse-if-cond #?expr)))

(defmatch rule-special-slot( (fail if ?expr) )
  `(fail-if ,(parse-if-cond #?expr)))

;; non-atomic expr

(defmatch rule-special-slot( (fail if not ??expr) )
  `(fail-if-not ,(parse-if-cond #?expr)))

(defmatch rule-special-slot( (fail if ??expr) )
  `(fail-if ,(parse-if-cond #?expr)))

;; atomic expr

(defmatch rule-special-slot( (glitch ?id if not ?expr) )
  `(glitch-if-not ,(parse-if-cond #?expr) ',#?id))

(defmatch rule-special-slot( (glitch ?id if ?expr) )
  `(glitch-if ,(parse-if-cond #?expr) ',#?id))

;; non-atomic expr

(defmatch rule-special-slot( (glitch ?id if not ??expr) )
  `(glitch-if-not ,(parse-if-cond #?expr) ',#?id))

(defmatch rule-special-slot( (glitch ?id if ??expr) )
  `(glitch-if ,(parse-if-cond #?expr) ',#?id))


;=========================================
; special slots - exists & for
;=========================================

;; don't need this with new multiple-assoc
; (defmatch rule-special-slot( (for ?form ??body) )
;   (match-bind '(loop for IT in ?form
;                   append (progn ??body))))


;=========================================
; special slots - lisp calls
;=========================================

(defmatch rule-special-slot( (lisp ??code) )
  `(progn ,@(parse-tree #?code)))

;; get rid of do in the next cleanup - its silly
; (defmatch rule-special-slot( (do ??code) )
;   (match-bind '(progn ??code nil)))


;=========================================
; special slots - if
;=========================================

(defmatch rule-special-slot( (if ?cond ?then ?else) )
  (match-bind `(if ,(parse-if-cond #?cond)
                 ,(parse-sub-slots #?then)
                 ,(parse-sub-slots #?else))))

(defmatch rule-special-slot( (if ?cond ?then) )
  (match-bind `(if ,(parse-if-cond #?cond)
                 ,(parse-sub-slots #?then))))


;=========================================
; special slots - misc
;=========================================

(defmatch rule-special-slot( (quote ?rest) )
  `',#?rest)


;;; error trap
(defmatch rule-special-slot( (==) )
  (error "illegal special slot form ~a" #?*match))


;=========================================
; parse-if-cond
;=========================================

(defmatch parse-if-cond( (?a ?op ?b) )
  ;; replace op from lookup if its there
  (let ((op (or (-> *operators* #?op) #?op)))
    `(,op ,(parse-slot-entry #?a) ,(parse-slot-entry #?b))
    ))

(defmatch parse-if-cond( ?x )
  ;; replace op from lookup if its there
  (parse-slot-entry #?x))


;;; error trap
(defmatch parse-if-cond( (==) )
  (error "illegal condition in slot ~a" #?*match))



;=========================================
; parse-slot-entry
;=========================================

#| if the entry is quoted that quoted value is returned
otherwise if the entry is or starts with a member of c-names
(ie its a . separated => form) it's parsed as such,
if not it is returned unmodified
|#

(defun parse-slot-entry (x &optional (c-names *c-names*))
  ; see general notes on *c-names*
  (if* (and (listp x) (eq (car x) 'quote))
     then x
     else
          (let* ((tokens (parse-string (substitute #\Space #\. (string x))))
                 (header (car tokens))
                 )
            (cond ((member header c-names)
                   (cons '=> tokens))
                  ((eq header '*) 
                   (cons '=> tokens))
                  ((eq header 'IT)
                   (cons 'IT=> (cdr tokens)))
                  (t x)
                  ))))

(defun parse-string (str &optional (start 0))
  (multiple-value-bind (sym n) (read-from-string str nil :eof :start start)
    (if (eq sym :eof)
        nil
      (cons sym (parse-string str n)))
  ))


(defun parse-tree (tree &optional (c-names *c-names*))
  ;; ok unfortunate name, this is not a noun "parse-tree"
  ;; its a verb/fn it parses the tree applying parse-slot-entry to everything
  (cond ((null tree) nil)
        ((atom tree) (parse-slot-entry tree c-names))
        ((eq (car tree) 'quote)   ; allows c-names to be quoted
         tree                     ; to supress their expansion
         )
        (t (cons (parse-tree (car tree) c-names) 
                 (parse-tree (cdr tree) c-names)))
        ))


(defun safe-append (a b)
  (if (atom a)
      (cons a b)
    (append a b)
    ))


;=========================================
; specific utils
;=========================================

(defun make-c-names (schema)
  (mapcar #'name (translate-schema
                  (remove-duplicates (flatten schema)))
    ))


;=========================================
; general utils
;=========================================

(defun last-atom (lis)
  (car (last lis)))

(defun single (x)
  (and (= 1 (length x)) (car x)))

(defun zero-or-one (x)
  (or (null x) (single x)))

(defun flatten (lis)
  (cond ((null lis) nil)
        ((listp (car lis))
         (append (flatten (car lis)) (flatten (cdr lis))))
        ( t
         (cons (car lis) (flatten (cdr lis))))
        ))


