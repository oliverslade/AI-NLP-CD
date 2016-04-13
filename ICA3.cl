;;; she went to the gym
;;; 

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
   
   (because conju (sems . link))
   
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
        (actor  . noun-phrase.sems)
        (action . verb-phrase.action)
        (object . verb-phrase.object))
   
   (s2  (sentence         -> pronoun-phrase noun-phrase)
        (actor    . pronoun-phrase.actor)
        (action   . pronoun-phrase.action)
        (location . noun-phrase.sems))
   
   (s3  (sentence         -> sentence conju adjective-phrase))
   
   (s4  (sentence         -> pronoun-phrase preposition-phrase)
        (actor    . pronoun-phrase.actor)
        (action   . pronoun-phrase.action)
        (link     . preposition-phrase.link)
        (action   . preposition-phrase.action)
        (state    . preposition-phrase.state))
   
   (ap  (adjective-phrase -> verb adjective)
        (action . (verb.sems verb.tense))
        (state  . adjective.sems))
   
   (ap1 (preposition-phrase -> prepo adjective-phrase)
        (link   . (prepo.sems))
        (action . adjective-phrase.action)
        (state  . (adjective-phrase.state)))
   
   (np1 (noun-phrase      -> determiner noun)
        (number . noun.number)
        (sems   . (determiner.sems noun.number noun.sems)))
   
   (np2 (noun-phrase      -> prepo noun-phrase)
        (sems . noun-phrase))
   
   (vp  (verb-phrase      -> verb noun-phrase)
        (action . verb.sems)
        (object . noun-phrase.sems)
        (number . verb.number))
   
   (pp  (pronoun-phrase   -> pronoun verb)
        (actor  . (pronoun.number pronoun.sems))
        (action . (verb.sems verb.tense)))
   ))