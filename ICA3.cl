;;; she went to the gym
;;; (parse 'sentence '(she went to the gym because she wanted to become healthy))
;;;                         effect^        conju^               cause^
;;; (parse 'sentence '(he burned the meal because he forgot it was cooking))

(build-lexicon
 `((she     pronoun (sems . female)  (number . singular))
   (he      pronoun (sems . male)    (number . singular))
   (they    pronoun (sems . neutral) (number . plural))
   (it      pronoun (sems . neutral) (number . singular))
   
   (forgot  verb (sems  . forgot)   (tense . past-tense))
   (went    verb (sems  . travel)   (tense . past-tense))
   (wanted  verb (sems  . want)     (tense . past-tense))
   (burned  verb (sems  . damage)   (tense . past-tense)    (state . bad))
   (burnt   verb (sems  . damage)   (tense . past-tense)    (state . bad))
   (become  verb (sems  . change)   (tense . present-tense))
   (chased  verb (sems  . hunts)    (tense . past-tense))
   (was     verb (sems  . exist)    (tense . passive))
   (cooking verb (sems  . prepare)  (tense . past))
   
   (to      prepo (sems . to))
   
   (and     conju (sems . join))
   (because conju (sems . cause))
   (but     conju (sems . contrast))
   (or      conju (sems . alternate))
   
   (gym     noun (sems . place)  (number . singular))
   (park    noun (sems . place)  (number . singular))
   (meal    noun (sems . food)   (number . singular))
   (cat     noun (sems . feline) (number . singular))
   (dog     noun (sems . canine) (number . singular))

   (healthy adjective (sems . good))
   
   (the     determiner (sems . specific))
   (a       determiner )
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
   (s4 (sentence -> sentence($firstS) conju sentence($secondS))
       (glitch gender-agreement if not $firstS.actor = $secondS.actor)
       (if (conju.sems = 'cause)
             (('effect-> ($firstS.action) ($firstS.object)) 
              ('cause-> ($secondS.action) ($secondS.state))
              ('link-> conju))
         ('error))
       )
   
   ;;; he burned the food because he forgot
   (s5 (sentence -> sentence conju pronoun-phrase)
       (glitch gender-agreement if not sentence.actor = pronoun-phrase.actor)
       (if (conju.sems = 'cause)
             (('effect-> (sentence.action) (sentence.object)) 
              ('cause-> (pronoun-phrase.actor) (pronoun-phrase.action))
              ('link-> conju))
         ('error)))
   
   ;;;  he forgot it was cooking
   (s6 (sentence -> pronoun-phrase verb-verb-phrase)
       (actor     . pronoun-phrase.actor)
       (action    . pronoun-phrase.action)
       (tense-ind . verb-verb-phrase.tense-indicator)
       (object    . verb-verb-phrase.actor)
       (state     . verb-verb-phrase.action))
          
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
        (if noun    (actor  . noun))
        (if pronoun (actor  . pronoun))
        (tense-indicator    . $firstV)
        (action             . $secondV))
   
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
  
 