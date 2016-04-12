
#| PATTERN MATCHER PACKAGE

Author: SCL
Latest modification: 03/2013

Revision History: see technical documentation

|#

;----------------------------------------------------------
; PACKAGE SETUP
;----------------------------------------------------------

; [m1.3(g6)/01]

(defpackage :matcher
  
  (:use    common-lisp)
  
  ;; see note below RE: ==
  (:import-from common-graphics-user "==")

  (:export #:matches              ;; functions & macros
           #:match>>              ;; [match>>/01]
           #:match->
           #:match-bind           ;; [match>>/01]
           #:matcher-expand       ;; [matcher-expand/01]
           #:mlet
           #:alet
           #:is-present
           #:foreach
           #:foreach@
           #:all-present
           #:forevery
           #:defmatch
           #:makunmatch
           
           ;; special symbols
           #:*bind                
           #:*match
           
           ;; matcher tags   [exports/01]
           #:==                   ;; name conflict with COMMON-GRAPHICS-USER
           #:=
           #:??
           #:?
           
           )
  )

(in-package :matcher)


;----------------------------------------------------------
; DECLARATIONS & SPECIALS
;----------------------------------------------------------


(eval-when (:compile-toplevel :load-toplevel :execute) ;; [loading/01]
  (declaim (special *matcher-methods*))
  (defvar *matcher-methods*)
  (setf *matcher-methods* nil)
  )


;____ matcher-block & *name-space* ________________________
;----------------------------------------------------------
                                                       ;; [*name-space*/02]
(defvar *name-space* nil)                              ;; [*name-space*/01]

;; set up global/special value matcher-vars name-space
(defvar *always-nil* nil)                              ;; [*name-space*/02]
(define-symbol-macro *name-space* *always-nil*)        ;; [matcher-vars/01]

(defmacro matcher-block (bindings &rest body)          ;; [matcher-block/01]
  `(let ((*name-space* ,bindings))                     ;; [matcher-block/02]
     ,@body))



;----------------------------------------------------------
; MATCHES & ASSOCIATED FUNCTIONS
;----------------------------------------------------------

(defun absorb (lis pat bind 
                   &key (test #'(lambda (x) (declare (ignore x)) t)) (attach nil))
  (do ( (listabs nil) (newbind nil) (res nil) )
      (   (or (and (setf res (funcall test listabs) )
                   (setf newbind
                     (match2 pat lis
                             (if attach
                                 (attach-binding bind attach
                                                 (if (equal t res) listabs res))
                               bind) ))
                   )
              (null lis))
       newbind
       )
    (cond ((consp lis)
           (setf listabs (append listabs (list (car lis))))
           (setf lis (cdr lis))
           )
          (t
           (setf listabs (append listabs lis))
           (setf lis nil)
           ))
    ))


(defun matches (pat lis &optional (bind nil))
  ; matches is inner wrapper for the core matching function
  (match2 pat lis (append `((*bind ,pat)(*match ,lis)) bind)))


(declaim (inline mpred mvar))
(defun mpred (p) (second (cadr p)))
(defun mvar (p) (first (cadr p)))

(defun match2 (pat lis bind)
  ; match2 is a subordinate fn to matches which should never be directly invoked
  ;;;(format t "~&M2 ~a ~a ~a" pat lis bind)
  (let (sym)
  (cond ((atom pat)
         (cond ((eq pat lis)                      ;; caters for both nil
                bind)
             ; ((or (null lis) (null pat))            ;; fail
             ;  nil)
               ((eq pat '=)
                bind)
               ((and (not (setf sym (check-prefix pat '??)))
                     (setf sym (check-prefix pat '?)))
                (if (binding-p bind sym)
                    (and (equal lis (getbinding bind sym))
                         bind)
                  (attach-binding bind sym lis)
                  ))
               (t nil)
               ))
        ((listp lis)                           ;; others are illegal
         (cond ((eq (car pat) '==)
                (absorb lis (cdr pat) bind)
                )
               ((and (eq (car pat) '?) lis)
                (if (binding-p bind (caadr pat))
                    (and (equal (car lis) (getbinding bind (mvar pat)))
                         (funcall (mpred pat) (car lis))
                         (match2 (cddr pat) (cdr lis) bind))
                  (let ((res (funcall (mpred pat) (car lis))))
                    (and res
                         (match2 (cddr pat) (cdr lis)
                                 (attach-binding bind (mvar pat)
                                                 (if (eq t res) (car lis) res)))
                         ))
                  ))
               ((eq (car pat) '??)
                (if (binding-p bind (caadr pat))
                    (and (funcall (mpred pat) (getbinding bind (mvar pat)))
                         (match2 (append (getbinding bind (mvar pat)) (cddr pat))
                                 lis bind))
                  (absorb lis (cddr pat) bind
                          :test (second (cadr pat))
                          :attach (first (cadr pat)))
                  ))
               ((setf sym (check-prefix (car pat) '??))
                (if (binding-p bind sym)
                    (let* ((b (getbinding bind sym))
                           (len (length b)))
                      (and (>= (length lis) len)
                           (equal b (subseq lis 0 len))
                           (match2 (cdr pat) (subseq lis len) bind)
                           ))
                  (absorb lis (cdr pat) bind :attach sym)
                  ))
               ((consp lis)
                (and (setf bind (match2 (car pat) (car lis) bind))
                     (match2 (cdr pat) (cdr lis) bind))
                )
               (t nil)
               )
         ))
    ))



(declaim (inline getbinding binding-p attach-binding ?match-value))

(defun getbinding (alist key)
   (second (assoc key alist)))

(defun binding-p (alist key)
   ; checks to see if key exists in alist
   (assoc key alist))

(defun attach-binding( binding nam val )
  (cons (list nam val) binding))

(defun ?match-value (name binding)                     ; to handle #? vars
  (if (listp name)
      (match-bind name binding)
    (getbinding binding name)))


;____ eval when starts [loading/01] ___________________________

(eval-when (:compile-toplevel :load-toplevel :execute)
  
  (defun check-prefix (sym prefix)
    ;; called with eg (check-prefix '??spud '??) returns
    ;; 'spud, if symbol is not prefixed correctly then
    ;; nil is returned. intern is used to turn string into a 
    ;; symbol in the current package
    (and (symbolp sym)
         (let* ((prestr (string prefix))
                (symstr (string sym))
                (prelen (length prestr))
                (symlen (length symstr))
                )
           (and (> symlen prelen)
                (string= prestr (subseq symstr 0 prelen))
                (intern (subseq symstr prelen))
                ))))
  
  (defun |#?-reader| (stream subchar arg)
    (declare (ignore subchar arg))
    (let* ((obj (read stream t nil t))
           (?obj (check-prefix obj '?)))
      ; (format t "~&#?-reader (~a) obj=<~a>" ?obj obj)
      (if ?obj  ;; expand it twice
          `(?match-value (?match-value ',?obj *name-space*) *name-space*)
        `(or (?match-value ',obj *name-space*)
             (when (boundp ',obj)
               (match-bind (symbol-value ',obj))   ; expanding a normal lisp var containing a pattern
               )))
      ))

  (set-dispatch-macro-character #\# #\? #'|#?-reader|)
  )
;____ eval-when ends ___________________________________


;----------------------------------------------------------
; MATCH-BIND - for output lists only
;----------------------------------------------------------

;; [match>>/01,02]
(defun match-bind-out  (pat bind strict)
   (let (sym)
      (cond ((null pat) nil)
            ((listp (car pat))
             (cons (match-bind-out (car pat) bind strict)
                   (match-bind-out (cdr pat) bind strict))
             )
            ((setf sym (check-prefix (car pat) '??))
             (append (if strict
                         (getbinding bind sym)
                       (or (getbinding bind sym) (car pat)))
                     (match-bind-out (cdr pat) bind strict))
             )
            ((setf sym (check-prefix (car pat) '?))
             (cons (if strict
                         (getbinding bind sym)
                       (or (getbinding bind sym) (car pat)))
                   (match-bind-out (cdr pat) bind strict))
             )
            ( t
             (cons (car pat) (match-bind-out (cdr pat) bind strict))
             ))
     ))


;; [match>>/01]
(defmacro match-> (pat &optional (bind '*name-space*) &key (strict nil))
  `(let ((*name-space* ,bind))
     (match-bind-out ,pat *name-space* ,strict)))

(defmacro match>> (pat &optional (bind '*name-space*) &key (strict t))
  `(let ((*name-space* ,bind))
     (match-bind-out ,pat *name-space* ,strict)))

;; [match>>/01]
(defmacro match-bind (pat &optional (bind '*name-space*) &key (strict t))
  `(let ((*name-space* ,bind))
     (match-bind-out ,pat *name-space* ,strict)))


;; set matcher-expand alias
(setf (symbol-function  'matcher-expand) #'car)       ;; [matcher-expand/01]


;----------------------------------------------------------
; ALET - LET FORM FOR ASL's
;----------------------------------------------------------
#|
 
ALET accepts asl followed by unquoted series of forms to evaluate
in the contexts of association bindings which act on #?matcher vars

EG:
    > (setf a1 '((a 1)(b 2)(c 3)))
    ((A 1) (B 2) (C 3))

    > (alet a1 (print (list #?a #?b #?c)) (+ #?a #?b #?c))
    (1 2 3) 
    6
|#


(defmacro alet (asl &rest body)
   ;; if *name-space* exists shadow it with asl & eval body
  `(let ((*name-space* (append ,asl *name-space*)))
     ;(format t "~&ALET: ~a" *name-space*)
     ;(format t "~&ALET: ~a" ,body)
     ,@body))


;----------------------------------------------------------
; MLET - LET FORM FOR MATCHES
;----------------------------------------------------------
;
; LET form which binds matching variables from matches. MATCHES returns an 
; association list (or nil) restructured into the declaration part
; for a LET expression & then the body of the MLET is eval'd with matching
; vars bound as lisp vars.
;
; MLET has to be a macro so as to encompass the scope of the caller,
; typically unreadable due to (necessary) nested backquote.
;
; The approach taken is to restructure the return value of matches into 
; LET & ALET forms.
;
; MLET uses a structured arg list of the form
;    (mlet (pattern list)
;      ..body..)
;
; EG:
; > (mlet ('(?a ?b ?c) '(spam chips beans))
;    (print #?a)
;    (print #?b)
;    (print #?c)
;    (pprint (list #?a #?b #?c)))
; SPAM 
; CHIPS 
; BEANS 
; (SPAM CHIPS BEANS)


(defmacro mlet ((pat lis &optional (bind '*name-space*)) &rest body)
  `(let ((*name-space* (matches ,pat ,lis ,bind)))
     (if *name-space* (progn t ,@body))
     ))


;----------------------------------------------------------
; FOREACH
;----------------------------------------------------------

; [m1.3(g6)/02]

(defmacro is-present ((p dbase &optional (bind '*name-space*)) &rest body)
  (let ((blk-sym (gensym)))
    `(block ,blk-sym
            (dolist (*match ,dbase nil)
              (let ((*name-space* (matches ,p *match ,bind)))
                (if *name-space*
                    (return-from ,blk-sym (progn t ,@body))
                  ))))))


; ___ foreach ________________
;
; foreach collects results by default
; to limit nesting use 'append for the accumulator
;
(defmacro foreach ((p dbase &optional (bind '*name-space*)
                            &key (accumulator 'collect))
                   &rest body)
  (let ((d (gensym)))
    `(let ((*name-space* ,bind))
       (remove nil ;prune failed nested calls
               (loop for ,d in ,dbase
                   ,accumulator (mlet (,p ,d) ,@body)
                     ))
       )))

(defmacro foreach@ ((p dbase &optional (bind '*name-space*)) &rest body)
  `(foreach (,p ,dbase ,bind :accumulator append) ,@body))


;----------------------------------------------------------
; FOREVERY
;----------------------------------------------------------
#|

eg:
> (defparameter db1
  '((isa b1 box)    (isa b2 box)    (isa b3 box)    (isa b4 box)
    (color b1 red)  (color b2 red)  (color b3 blue) (color b4 blue)
    (size b1 large) (size b2 small) (size b3 small) (size b4 small)
    (supports b1 b2)    (supports b2 b3)
    ))

    > (setf pp1 '((isa ?b box)(color ?b red)))
    ((ISA ?B BOX) (COLOR ?B RED))

    > (forevery (pp1 db1) (print (list 'b==> b)) (list *bind *match))
    (B==> B1) 
    (B==> B2) 
    ((((ISA ?B BOX) (COLOR ?B RED)) ((COLOR B1 RED) (ISA B1 BOX)))
     (((ISA ?B BOX) (COLOR ?B RED)) ((COLOR B2 RED) (ISA B2 BOX))))

    > (setf pp2 '((isa ?b box)(color ?b red)
              (isa ?a box)(size ?a small)))
    ((ISA ?B BOX) (COLOR ?B RED) (ISA ?A BOX) (SIZE ?A SMALL))

    > (forevery (pp2 db1) (print (list 'b==> #?b 'a==> #?a)) #?*match)
    (B==> B1 A==> B2) 
    (B==> B1 A==> B3) 
    (B==> B1 A==> B4) 
    (B==> B2 A==> B2) 
    (B==> B2 A==> B3) 
    (B==> B2 A==> B4) 
    (((SIZE B2 SMALL) (ISA B2 BOX) (COLOR B1 RED) (ISA B1 BOX))
     ((SIZE B3 SMALL) (ISA B3 BOX) (COLOR B1 RED) (ISA B1 BOX)) 
     ((SIZE B4 SMALL) (ISA B4 BOX) (COLOR B1 RED) (ISA B1 BOX))
     ((SIZE B2 SMALL) (ISA B2 BOX) (COLOR B2 RED) (ISA B2 BOX))
     ((SIZE B3 SMALL) (ISA B3 BOX) (COLOR B2 RED) (ISA B2 BOX))
     ((SIZE B4 SMALL) (ISA B4 BOX) (COLOR B2 RED) (ISA B2 BOX)))

|#

   ;; more problematic than the other macros because of
   ;; nested nature of looping. Also the size of pset is not
   ;; known at expansion time, so it's impossible to rewrite 
   ;; this macro by recursively generating loop clauses at
   ;; expansion time
   ;; there is a real difference between the last pattern
   ;; & the rest - the last one needss collecting, the others
   ;; appending

(defparameter *specials* '(:not :guard))

(defun is-special (x)
  (member (car x) *specials*))


(defmacro forevery ((pset dbase &optional (bind '*name-space*)) &rest body)
  `(labels 
       ((extract (label asl)
                 (mapcar #'second
                   (remove-if-not #'(lambda (a) (eq (first a) label))
                                  asl))
                 )
        (consolidate (bnd)
                   ;; combine all *match & *bind forms
                     `((*bind ,(extract '*bind bnd))
                       (*match ,(extract '*match bnd))
                       ,@bnd))
        (fforevery (p* db bnd &key (simple-case nil))
                   ;(format t "~&p*  ~a~&db  ~a~&bd ~a" p* db bnd)
                   (cond ((and (null p*) simple-case) ;ie: called via :not
                          (throw 'not-label t)
                          )
                         ;-----------------------------
                         ((null p*)   ; normal case
                          ;; combine all *match & *bind forms
                          (setf bnd (consolidate bnd))
                          (list (alet bnd ,@body))
                          )
                         ;-----------------------------
                         ((eq (caar p*) :not)
                          ; cannot call all-present because of recursive macro-expansion
                          ; so duplicating all-present activity to some extent
                          (unless
                              (catch 'not-label
                                     (fforevery (cdar p*) db bnd :simple-case t))
                            (fforevery (cdr p*) db bnd)
                            ))
                         ;-----------------------------
                         ((eq (caar p*) :guard)
                          (when (funcall `(lambda (bind) 
                                            (alet bind ,@(cdar p*))) bnd)
                            (fforevery (cdr p*) db bnd)
                            ))
                         ;-----------------------------
                         ( t
                          (foreach ((car p*) db bnd :accumulator append)
                                    (fforevery (cdr p*) db *name-space*))
                          ))
                   ))
     (fforevery ,pset ,dbase ,bind)
     ))


(defmacro all-present ((pset dbase &optional (bind '*name-space*)) &rest body)
  (let ((blk-sym (gensym)))
    `(block ,blk-sym
       (forevery (,pset ,dbase ,bind)
                 (return-from ,blk-sym (progn ,@body))
                 ))
    ))


;----------------------------------------------------------
; DEFMATCH & associated Fns
;----------------------------------------------------------


#| 
 | > (defmatch m--1 ((?a ?b ?c) x y)
 |     (pprint 'm--1-fn))
 | creates:
 |  (lambda (m--1 x y)
 |    (alet m--1 @body))
 | where m--1 is the name of the asl created by matching
 |#


; the definition of makunmatch (below) may seem a little unusual, so explanation...
; there was a curious but persistent bug with the compilation of defmatch forms when
; multiple defmatch forms were highlighted & compiled when a makunmatch form was also
; highlighted - compilation stopped at the makunmatch.
; The makunmatch duo below crack this problem


(defmacro makunmatch (x)
  (eval `(fn-makunmatch ,x))
  t)

(defun fn-makunmatch (x)
  (setf *matcher-methods* (remove x *matcher-methods* :key #'car))
  (fmakunbound x)
  )

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun mreplace (old new lis &key (key #'identity))
    ;; like replace but with a key arg
    (mapcar #'(lambda (x) (if (equal (funcall key x) old) new x)) lis))
  )

(defmacro defmatch (name (pattern &rest arglist) &rest body)
  (let ((newd   ; newd <- `(,pattern . ,lambda-form)
                ; name has dual use, its also the ?var.val asl
         `(,pattern . ,(eval `(function (lambda (,name ,@arglist)
                                          (alet ,name ,@body))
                                        ))))
        (defs (cdr (assoc name *matcher-methods*)))   ; existing defs
        )
    ;(format t "~&defs, newd: ~a ~a~&" defs newd)
    (cond ( defs  ; this is not the first defn for this match method
              (if (member pattern defs :key #'car :test #'equal)
                  ; a redefinition
                  (setf defs (mreplace pattern newd defs :key #'car))
                ; a new definition
                (setf defs (append defs (list newd)))
                )
              (setf *matcher-methods*
                (mreplace name (cons name defs) *matcher-methods* :key #'car))
            )
          ( t ; first defn of new match match method
           (setf *matcher-methods*
             (cons `(,name ,newd) *matcher-methods*))
           (eval `(defun ,name (&rest args)
                    (if (eq :* (car args))
                        (search-fn* ',name (cdr args))
                      (search-fn ',name args)
                      )))
           ))
    ;; return some vaguely meaningful value
    t
    ))


(defun search-fn (name args)
   (labels ((m-srch (asl lis args)
              (if (null asl) 
                 nil
                 (let ((m (matches (caar asl) lis)))
                    (if m
                       (apply (cdar asl) m args)
                       (m-srch (cdr asl) lis args)
                       ))
                 )))
     (m-srch (cdr (assoc name *matcher-methods*)) (car args) (cdr args))
     ))

(defun search-fn* (name args)
  ;; this one searches for first matching form which returns non-null
   (labels ((m-srch (asl lis args)
              (if (null asl) 
                 nil
                 (let ((m (matches (caar asl) lis)))
                    (if m
                       (or (apply (cdar asl) m args)
                           (m-srch (cdr asl) lis args))
                      (m-srch (cdr asl) lis args))
                 ))))
     (m-srch (cdr (assoc name *matcher-methods*)) (car args) (cdr args))
     ))


;----------------------------------------------------------
; MATCH...OVER
;----------------------------------------------------------



#|
(MATCH struct OVER pat {SELECTING form* {WHEN|UNLESS pred*}}
                     | {DOING form {WHEN|UNLESS pred*}}
                     | {REDUCING form* WITH fn } )

|#


;; need macro form because there isnt a defmacro match

(defmacro match (&rest expr)
  (sub-match expr))

;;; selecting...

(defmatch sub-match ((?struct over ?pat selecting ??form when ??pred))
  #?(foreach (?pat ?struct)
             (when (progn ??pred) ??form)))

(defmatch sub-match ((?struct over ?pat selecting ??form unless ??pred))
  #?(foreach (?pat ?struct)
             (unless (progn ??pred) ??form)))

(defmatch sub-match ((?struct over ?pat selecting ??form))
  #?(foreach (?pat ?struct) ??form))


;;; doing ....
;; exactly the same as selecting but it allows a conceptually different label

(defmatch sub-match ((?struct over ?pat doing ??form when ??pred))
  #?(foreach (?pat ?struct)
             (when (progn ??pred) ??form)))

(defmatch sub-match ((?struct over ?pat doing ??form unless ??pred))
  #?(foreach (?pat ?struct)
             (unless (progn ??pred) ??form)))

(defmatch sub-match ((?struct over ?pat doing ??form))
  #?(foreach (?pat ?struct) ??form))


;;; reducing ....

(defmatch sub-match ((?struct over ?pat reducing ??form with ?fn when ??pred))
  #?(reduce (function ?fn)
            (foreach (?pat ?struct)
                     (when (progn ??pred) ??form))))

(defmatch sub-match ((?struct over ?pat reducing ??form with ?fn unless ??pred))
  #?(reduce (function ?fn)
            (foreach (?pat ?struct)
                     (unless (progn ??pred) ??form))))

(defmatch sub-match ((?struct over ?pat reducing ??form with ?fn))
  #?(reduce (function ?fn) (foreach (?pat ?struct) ??form)))


;;; error case

(defmatch sub-match ( ?x )
  (error "bad match over form ~a" #?x))

