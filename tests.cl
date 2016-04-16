;;; ================== TESTING =========================

;;; Load test file
;;; (trace tester) or not 
;;; (tester tests)

(defmatch run-a-test ((?n - ?call => ?result))
  (unless (equal #?result (eval #?call))
    (pprint (match>> '(failed test. ?n -- ?call)))))

(defun tester (tests)
  (mapcar #'run-a-test tests)
  'tests-completed-all-passed)

(defparameter tests

;;;TESTING SIMPLE SENTENCES
      
;;;SHE WENT TO THE GYM      

'((test1 - (parse 'sentence '(she went to the gym)) =>

_____________ 
COMPLETE-EDGE 0 5 S2 SENTENCE (SHE WENT TO THE GYM) NIL
S2   SENTENCE -> (PRONOUN-PHRASE NOUN-PHRASE)
Syntax
(SENTENCE (PRONOUN-PHRASE (PRONOUN SHE) (VERB WENT))
 (NOUN-PHRASE (PREPO TO) (DETERMINER THE) (NOUN GYM)))
Semantics
(SENTENCE (ACTOR SINGULAR FEMALE) (ACTION TRAVEL PAST-TENSE)
 (OBJECT SPECIFIC SINGULAR PLACE))
T
)

;;;HE BURNED THE MEAL

(test2 - (parse 'sentence '(he burned the meal)) => 
                
_____________ 
COMPLETE-EDGE 0 4 S2 SENTENCE (HE BURNED THE MEAL) NIL
S2   SENTENCE -> (PRONOUN-PHRASE NOUN-PHRASE)
Syntax
(SENTENCE (PRONOUN-PHRASE (PRONOUN HE) (VERB BURNED))
 (NOUN-PHRASE (DETERMINER THE) (NOUN MEAL)))
Semantics
(SENTENCE (ACTOR SINGULAR MALE) (ACTION DAMAGE PAST-TENSE)
 (OBJECT SPECIFIC SINGULAR FOOD))
T
)

;;;TESTING COMPLEX SENTENCES

;;;SHE WENT TO THE GYM BECAUSE SHE WANTED TO BECOME HEALTHY

(test3 - (parse 'sentence '(she went to the gym because she wanted to become healthy)) =>
       
_____________ 
COMPLETE-EDGE 0 11 S4 SENTENCE (SHE WENT TO THE GYM BECAUSE SHE WANTED TO BECOME
                                HEALTHY) NIL
S4   SENTENCE -> (SENTENCE ($FIRSTS) CONJU SENTENCE ($SECONDS))
Syntax
(SENTENCE
 (SENTENCE (PRONOUN-PHRASE (PRONOUN SHE) (VERB WENT))
  (NOUN-PHRASE (PREPO TO) (DETERMINER THE) (NOUN GYM)))
 (CONJU BECAUSE)
 (SENTENCE (PRONOUN-PHRASE (PRONOUN SHE) (VERB WANTED))
  (PREPOSITION-PHRASE (PREPO TO)
   (ADJECTIVE-PHRASE (VERB BECOME) (ADJECTIVE HEALTHY)))))
Semantics
(SENTENCE
 ((EFFECT-> (TRAVEL PAST-TENSE) (SPECIFIC SINGULAR PLACE))
  (CAUSE-> ((WANT PAST-TENSE) (CHANGE PRESENT-TENSE)) (GOOD))
  (LINK-> (SEMS . CAUSE))))
T
)
)
)