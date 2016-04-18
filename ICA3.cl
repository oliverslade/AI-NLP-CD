;;; she went to the gym
;;; (parse 'sentence '(she went to the gym because she wanted to become healthy))
;;;                         effect^        conju^               cause^
;;; (parse 'sentence '(he burned the meal because he forgot it was cooking))
;;;
;;; (parse 'sentence '(john wore a coat because it was cold))
;;;
;;; (parse 'sentence '(stop eating bad food in order to become healthy))

(build-lexicon
 `((she     pronoun (sems . female)    (number . singular))
   (he      pronoun (sems . male)      (number . singular))
   (they    pronoun (sems . neutral)   (number . plural))
   (it      pronoun (sems . neutral)   (number . singular))
   
   (john    noun     (sems . person)    (number . singular))
   
   (become  verb     (sems  . change)   (tense . present-tense))
   (burned  verb     (sems  . damage)   (tense . past-tense)    (state . bad))
   (burnt   verb     (sems  . damage)   (tense . past-tense)    (state . bad))
   (chased  verb     (sems  . hunts)    (tense . past-tense))
   (cooking verb     (sems  . prepare)  (tense . past-tense))
   (eating  verb     (sems  . ingest)   (tense . continuous))
   (forgot  verb     (sems  . forgot)   (tense . past-tense))
   (wanted  verb     (sems  . want)     (tense . past-tense))
   (stop    verb     (sems  . end)      (tense . present-tense))
   (was     verb     (sems  . exist)    (tense . passive))
   (went    verb     (sems  . travel)   (tense . past-tense))
   (wore    verb     (sems  . equip)    (tense . past-tense))
   
   (to       prepo   (sems . to))
   
   (and      conju   (sems . join))
   (because  conju   (sems . causation))
   (so       conju   (sems . causing))
   (but      conju   (sems . contrast))
   (in-order conju   (sems . causation))
   (or       conju   (sems . alternate))
   
   (cat     noun     (sems . feline)  (number . singular))
   (coat    noun     (sems . clothes) (number . singular))
   (dog     noun     (sems . canine)  (number . singular))
   (food    noun     (sems . food)    (number . general))
   (gym     noun     (sems . place)   (number . singular))
   (meal    noun     (sems . food)    (number . singular))
   (park    noun     (sems . place)   (number . singular))
   
   (bad     adjective  (sems . bad))
   (cold    adjective  (sems . low-temp))
   (healthy adjective  (sems . (health-good)))
   
   (the     determiner (sems . specific))
   (a       determiner (sems . general))
   ))

(build-grammar
 ;;;    the dog chased the cat
 '((s1  (sentence         -> noun-phrase verb-phrase)
        (actor   . noun-phrase.sems)
        (action  . verb-phrase.action)
        (object  . verb-phrase.object))
   
   ;;;  she went (to) the gym
   (s2  (sentence         -> pronoun-phrase noun-phrase)
        (actor    . pronoun-phrase.actor)
        (action   . pronoun-phrase.action)
        (object   . noun-phrase.sems))

   ;;;  she wanted to become healthy
   (s3  (sentence         -> pronoun-phrase preposition-phrase)
        (actor     . pronoun-phrase.actor)
        (action    . ((pronoun-phrase.action) (preposition-phrase.action)))
        (link      . preposition-phrase.link)
        (action2   . preposition-phrase.action)
        (state     . preposition-phrase.state))
   
   ;;; she went to the gym because she wanted to become healthy
   ;;; john wore a coat because it was cold
;   (s4 (sentence -> sentence($firstS) conju sentence($secondS))
;       (glitch gender-agreement if not $firstS.actor = $secondS.actor)
;       (actor . $firstS.actor)
;       (if (conju.sems = 'causation)
;           (('effect-> ($firstS.action) ($firstS.object))
;            (if $secondS.action
;                ('cause-> ($secondS.action) ($secondS.state))
;              ('cause-> ($secondS.state)))
;              ('link-> conju))
;         ('error))
   ;       )
   
   ;;; Remade S4. Uses nested IFs to check conju semantics.
   ;;; Could use COND, shown below.
   (s4 (sentence -> sentence($firstS) conju sentence($secondS))
       (glitch gender-agreement if not $firstS.actor = $secondS.actor)
       (actor . $firstS.actor)
       (if (conju.sems = 'causation)
           (('effect-> ($firstS.action) ($firstS.object))
            (if $secondS.action
                ('cause-> ($secondS.action) ($secondS.state))
              ('cause-> ($secondS.state)))
            ('link-> conju))
         (if (conju.sems = 'causing)
             ((if $firstS.action
                  ('cause-> ($firstS.action) ($firstS.state))
                ('cause-> ($firstS.state)))
              ('effect-> ($secondS.action) ($secondS.object))
              ('link-> conju.sems))
           ('error))
         )
       )
   
   ;;; COND doesn't work. Prints out the whole statement.
;   (s4 (sentence -> sentence($firstS) conju sentence($secondS))
;       (glitch gender-agreement if not $firstS.actor = $secondS.actor)
;       (actor . $firstS.actor)
;       (cond ((conju.sems = 'causation)
;              (('effect-> ($firstS.action) ($firstS.object))
;               (if $secondS.action
;                   ('cause-> ($secondS.action) ($secondS.state))
;                 ('cause-> ($secondS.state)))
;               ('link-> conju)))
;             ((conju.sems = 'causing)
;              (('effect-> ($secondS.action) ($secondS.object))
;               (if $firstS.action
;                   ('cause-> ($firstS.action) ($firstS.state))
;                 ('cause-> ($firstS.state)))
;               ('link-> conju)))
;             (t ('error))
;             )
;       )
   
   ;;; he burned the food because he forgot
   (s5 (sentence -> sentence conju pronoun-phrase)
       (glitch gender-agreement if not sentence.actor = pronoun-phrase.actor)
       (actor . $firstS.actor)
       (if (conju.sems = 'causation)
             (('effect-> (sentence.action) (sentence.object)) 
              ('cause-> (pronoun-phrase.actor) (pronoun-phrase.action) (sentence.object))
              ('link-> conju))
         ('error)))
   
   ;;;  he forgot it was cooking
   (s6 (sentence -> pronoun-phrase verb-verb-phrase)
       (actor     . pronoun-phrase.actor)
       (action    . ((pronoun-phrase.action) (verb-verb-phrase.action)))
       (tense-ind . verb-verb-phrase.tense-indicator)
       (object    . verb-verb-phrase.actor))
   
   ;;; John wore a coat
   (s7 (sentence -> noun verb-phrase)
       (actor     . noun.sems)
       (action    . verb-phrase.action)
       (object    . verb-phrase.object)
       (number    . verb-phrase.number))
   
   ;;; (it) was cold / (john) was cold
   (s8 (sentence -> ?noun ?pronoun verb adjective)
       (fail if (noun and pronoun))
       (if noun    (actor  . noun))
       (if pronoun (actor  . pronoun))
       (tense-indicator    . verb)
       (state              . adjective.sems))
   
   ;;; stop eating (bad) food
   (s9  (sentence -> verb-verb-phrase ?adjective noun)
        (action . (verb-verb-phrase.tense-indicator verb-verb-phrase.action))
        (if adjective (state . adjective.sems))
       (object . noun.sems))
   
   (s10 (sentence -> sentence conju preposition-phrase)
        (glitch gender-agreement if not sentence.actor = preposition-phrase.actor)
        (actor . sentence.actor)
        (if (conju.sems = 'causation)
              (('effect-> (sentence.action) (sentence.object)) 
               ('cause-> (preposition-phrase.link) (preposition-phrase.action) (preposition-phrase.state))
               ('link-> conju))
          ('error)))
          
   ;;;  become healthy
   (ap  (adjective-phrase -> verb adjective)
        (action . (verb.sems verb.tense))
        (state  . adjective.sems))
   
   ;;;  she went
   (pp  (pronoun-phrase   -> pronoun verb)
        (actor  . (pronoun.number pronoun.sems))
        (action . (verb.sems verb.tense)))
   
   ;;;  to become healthy
   (pr  (preposition-phrase -> prepo adjective-phrase)
        (link   . prepo.sems)
        (action . adjective-phrase.action)
        (state  . adjective-phrase.state))
   
   ;;;  (it) was cooking / (meal) was cooking
   (vvp (verb-verb-phrase -> ?noun ?pronoun verb($firstV) verb($secondV))
        (fail if (noun and pronoun))
        (if noun    (actor  . (noun.sems noun.number)))
        (if pronoun (actor  . (pronoun.sems pronoun.number)))
        (tense-indicator    . ($firstV.sems $firstV.tense))
        (action             . ($secondV.sems $secondV.tense)))

   ;;;  was running / was cooking 
   (vn  (verb-noun -> verb noun)
        (action . verb)
        (sems   . noun))
   
   ;;;  (to) the gym
   (np  (noun-phrase      -> ?prepo determiner noun)
        (if prepo
            (link . prepo.sems))
        (number . (noun.number))
        (sems   . (determiner.sems noun.number noun.sems)))
   
   ;;;  chased the cat
   (vp  (verb-phrase      -> verb noun-phrase)
        (action . verb.sems)
        (object . noun-phrase.sems)
        (number . verb.number))
       ))
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ;;; ================== TESTING =========================
 ;(defparameter tests
 ; 
 ; ;;;TESTING THE EXAMPLE SCENARIO (RECURSIVE & MATCHER)
 ; 
 ; '((test1 - (find-average-matcher '(10 ((30 1) 20) (8 (5 (50 7)) 9) 40)) => 18)
 ;   (test2 - (find-average-head '(10 ((30 1) 20) (8 (5 (50 7)) 9) 40)) => 18)
 ;   (test3 - (find-average-tail '(10 ((30 1) 20) (8 (5 (50 7)) 9) 40)) => 18)
 ;   
 ;   ;;;TESTING NIL AND ZERO INPUT IN FUNCTION (RECURSIVE & MATCHER)
 ;   
 ;   (test4 - (find-average-matcher '()) => nil)
 ;   (test5 - (find-average-head '()) => nil)
 ;   (test6 - (find-average-tail '()) => nil)
 ;   (test7 - (find-average-matcher '(0)) => 0)
 ;   (test8 - (find-average-head '(0)) => 0)
 ;   (test9 - (find-average-tail '(0)) => 0)
 ;   
 ;   ;;;TESTING A FLAT LIST (RECURSIVE & MATCHER)
 ;   
 ;   (test10 - (find-average-matcher '(1 2 3 4 5 6 7 8 9)) => 5)
 ;   (test11 - (find-average-head '(1 2 3 4 5 6 7 8 9)) => 5)
 ;   (test12 - (find-average-tail '(1 2 3 4 5 6 7 8 9)) => 5)
 ;   
 ;   ;;;TESTING A MINUS IN THE LIST (RECURSIVE & MATCHER)
 ;   
 ;   (test13 - (find-average-matcher '(10 ((-30 1) 20) (8 (5 (50 7)) 9) 40)) => 12)
 ;   (test14 - (find-average-head '(10 ((-30 1) 20) (8 (5 (50 7)) 9) 40)) => 12)
 ;   (test15 - (find-average-tail '(10 ((-30 1) 20) (8 (5 (50 7)) 9) 40)) => 12)
 ;   
 ;   ;;;TESTING THE EXAMPLE SCENARIO (ITERATIVE)
 ;   
 ;   (test16 - (iterative-average1 '(10 ((30 1) 20) (8 (5 (50 7)) 9) 40)) => 18)
 ;   (test17 - (iterative-average2 '(10 ((30 1) 20) (8 (5 (50 7)) 9) 40)) => 18)
 ;   
 ;   ;;;TESTING NIL AND ZERO INPUT IN FUNCTION (ITERATIVE)
 ;   
 ;   (test18 - (iterative-average1 '()) => nil)
 ;   (test19 - (iterative-average2 '()) => nil)
 ;   (test20 - (iterative-average1 '(0)) => 0)
 ;   (test21 - (iterative-average2 '(0)) => 0) 
 ;   
 ;   ;;;TESTING A FLAT LIST (ITERATIVE)
 ;   
 ;   (test22 - (iterative-average1 '(1 2 3 4 5 6 7 8 9)) => 5)
 ;   (test23 - (iterative-average2 '(1 2 3 4 5 6 7 8 9)) => 5)
 ;   
 ;   ;;;TESTING A MINUS IN THE LIST (ITERATIVE)
 ;   
 ;   (test24 - (iterative-average1 '(10 ((-30 1) 20) (8 (5 (50 7)) 9) 40)) => 12)
 ;   (test25 - (iterative-average2 '(10 ((-30 1) 20) (8 (5 (50 7)) 9) 40)) => 12)    
 ;   ))
  
 