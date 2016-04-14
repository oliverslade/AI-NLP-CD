;;; she went to the gym
;;; (parse 'sentence '(she went to the gym because she wanted to become healthy))
;;;                         effect^        conju^               cause^

(build-lexicon
 `((she     pronoun (sems . female) (number . singular))
   (he      pronoun (sems . male)   (number . singular))
   (they    pronoun (sems . neutral) (number . plural))
   
   (went    verb (sems  . travel)  (tense . past-tense))
   (wanted  verb (sems  . want)    (tense . past-tense))
   (burned  verb (state . bad)    (tense . past-tense))
   (burnt   verb (state . bad)    (tense . past-tense))
   (become  verb (sems  . change) (tense . present-tense))
   (chased  verb (sems  . hunts))
   
   (to      prepo (sems . to))
   
   (and     conju (sems . join))
   (because conju (sems . causes))
   (but     conju (sems . contrast))
   (or      conju (sems . alternate))
   
   (gym     noun (sems . place) (number . singular))
   (park    noun (sems . place) (number . singular))
   (dinner  noun)
   (cat     noun (sems . feline) (number . singular))
   (dog     noun (sems . canine) (number . singular))
   
   (healthy adjective (sems . good))
   
   (the     determiner (sems . specific))
   (a       determiner )
   ))

(build-grammar
 '((s1  (sentence         -> noun-phrase verb-phrase)
        (actor   . noun-phrase.sems)
        (action  . verb-phrase.action)
        (object  . verb-phrase.object))
   
   (s2  (sentence         -> pronoun-phrase noun-phrase)
;       (if (pronoun-phrase.action.sems
        (actor    . pronoun-phrase.actor)
        (action   . pronoun-phrase.action)
        (object   . noun-phrase.sems)
;       (test1    . pronoun-phrase.verb.sems)
;       (test2    . pronoun-phrase.verb.action)
       )
   
   (s3  (sentence         -> sentence conju adjective-phrase))
   
   (s4  (sentence         -> pronoun-phrase preposition-phrase)
        (actor    . pronoun-phrase.actor)
        (action   . pronoun-phrase.action)
        (link     . preposition-phrase.link)
        (action   . preposition-phrase.action)
        (state    . preposition-phrase.state))
   
   (s5 (sentence -> sentence conju sentence)
       (if (conju.sems = 'cause)
           (('effect (sentence.action) (sentence.object)) ('<- conju.sems) ())
         ('badstuff)))
   
   (ap  (adjective-phrase -> verb adjective)
        (action . (verb.sems verb.tense))
        (state  . adjective.sems))
   
   (pr  (preposition-phrase -> prepo adjective-phrase)
        (link   . (prepo.sems))
        (action . adjective-phrase.action)
        (state  . (adjective-phrase.state)))
   
   (np  (noun-phrase      -> ?prepo determiner noun)
        (if prepo
            (link . prepo.sems))
        (number . (noun.number))
        (sems   . (determiner.sems noun.number noun.sems))
        )
   
   (vp  (verb-phrase      -> verb noun-phrase)
        (action . verb.sems)
        (object . noun-phrase.sems)
        (number . verb.number))
   
   (pp  (pronoun-phrase   -> pronoun verb)
        (actor  . (pronoun.number pronoun.sems))
        (action . (verb.sems verb.tense)))
   ))