;;; she went to the gym
;;; (parse 'sentence '(she went to the gym because she wanted to become healthy))
;;;                         effect^      conju^               cause^
;;; (parse 'sentence '(he burned the meal because he forgot it was cooking))
;;;                         effect^      conju^               cause^
;;; (parse 'sentence '(john wore a coat because it was cold))
;;;                       effect^      conju^   cause^
;;; (parse 'sentence '(stop eating bad food in-order to become healthy))
;;;                       cause^          conju^      effect^
;;; (parse 'sentence '(she wanted to become healthy so she went to the gym))
;;;                       cause^                conju^  effect^
;;; (parse 'sentence '(since she wanted to become healthy she went to the gym))
;;;                  conju^      effect^                    cause^

(build-lexicon
 `((he       pronoun   (sems . male)      (number . singular))
   (it       pronoun   (sems . neutral)   (number . singular))
   (she      pronoun   (sems . female)    (number . singular))
   (they     pronoun   (sems . neutral)   (number . plural))
   
   (john     noun      (sems  . male)     (number . singular))
   (jane     noun      (sems  . female)   (number . singular))
   
   (become   verb      (sems  . change)   (tense . present-tense))
   (burned   verb      (sems  . damage)   (tense . past-tense)    (state . bad))
   (burnt    verb      (sems  . damage)   (tense . past-tense)    (state . bad))
   (chased   verb      (sems  . hunts)    (tense . past-tense))
   (cooking  verb      (sems  . prepare)  (tense . past-tense))
   (eating   verb      (sems  . ingest)   (tense . continuous))
   (forgot   verb      (sems  . forgot)   (tense . past-tense))
   (wanted   verb      (sems  . want)     (tense . past-tense))
   (stop     verb      (sems  . end)      (tense . present-tense))
   (was      verb      (sems  . exist)    (tense . passive))
   (went     verb      (sems  . travel)   (tense . past-tense))
   (wore     verb      (sems  . equip)    (tense . past-tense))
   
   (to       prepo     (sems . to))
   
   (and      conju     (sems . join))
   (because  conju     (sems . causation))
   (but      conju     (sems . contrast))
   (in-order conju     (sems . causation))
   (or       conju     (sems . alternate))
   (since    conju     (sems . causation))
   (so       conju     (sems . causing))
  
   (cat      noun      (sems . feline)    (number . singular))
   (coat     noun      (sems . clothes)   (number . singular))
   (dog      noun      (sems . canine)    (number . singular))
   (food     noun      (sems . food)      (number . general))
   (gym      noun      (sems . place)     (number . singular))
   (man      noun      (sems . person)    (number . singular))
   (meal     noun      (sems . food)      (number . singular))
   (men      noun      (sems . person)    (number . plural))
   (park     noun      (sems . place)     (number . singular))
   
   (bad     adjective  (sems . (state-bad)))
   (cold    adjective  (sems . (temperature-low)))
   (fit     adjective  (sems . (health-good)))
   (healthy adjective  (sems . (health-good)))
   (stupid  adjective  (sems . (intelligence-low)))
   
   (a       determiner (sems . general))
   (the     determiner (sems . specific))
   ))

(build-grammar
   ;;;  Example: the dog chased the cat
   ;;;  [DETERMINER NOUN] [VERB (PREPOSITION) DETERMINER NOUN]
 '((s1  (sentence         -> noun-phrase verb-phrase)
        (actor   . noun-phrase.sems)
        (action  . verb-phrase.action)
        (object  . verb-phrase.object))
   
   ;;;  Example: she went (to) the gym
   ;;;  [PRONOUN VERB] [(PREPOSITION) DETERMINER NOUN]
   (s2  (sentence         -> pronoun-phrase noun-phrase)
        (actor    . pronoun-phrase.actor)
        (action   . pronoun-phrase.action)
        (object   . noun-phrase.sems))

   ;;;  Example: she wanted to become healthy
   ;;;  [PRONOUN VERB] [PREPOSITION VERB ADJECTIVE]
   (s3  (sentence         -> pronoun-phrase preposition-phrase)
        (actor     . pronoun-phrase.actor)
        (action    . ((pronoun-phrase.action) (preposition-phrase.action)))
        (link      . preposition-phrase.link)
        (action2   . preposition-phrase.action)
        (state     . preposition-phrase.state))
   
   ;;;  Example: he forgot it was cooking
   ;;;  [PRONOUN VERB] [(PRONOUN) (NOUN) VERB VERB]
   (s4  (sentence          -> pronoun-phrase verb-verb-phrase)
        (actor     . pronoun-phrase.actor)
        (action    . ((pronoun-phrase.action) (verb-verb-phrase.tense-indicator) (verb-verb-phrase.action)))
        (tense-ind . verb-verb-phrase.tense-indicator)
        (object    . verb-verb-phrase.actor))
   
   ;;;  Example: john wore a coat
   ;;;  [NOUN] [VERB (PREPOSITION) DETERMINER NOUN]
   (s5  (sentence          -> noun verb-phrase)
        (actor     . (noun.sems noun.number))
        (action    . verb-phrase.action)
        (object    . verb-phrase.object)
        (number    . verb-phrase.number))
   
   ;;;  Example: (it) was cold / (john) was cold
   ;;;  [(NOUN)] [(PRONOUN)] [VERB ADJECTIVE]
   (s6  (sentence          -> ?noun ?pronoun adjective-phrase)
        (fail if (noun and pronoun))
        (if noun    (actor  . (noun.sems noun.number)))
        (if pronoun (actor  . (pronoun.sems pronoun.number)))
        (action             . adjective-phrase.action)
        (state              . adjective-phrase.state))
   
   ;;;  Example: stop eating (bad) food
   ;;;  [(PRONOUN) (NOUN) VERB VERB] [*(ADJECTIVE)] [NOUN]
   (s7  (sentence          -> verb-verb-phrase *adjective noun)
        (action . (verb-verb-phrase.tense-indicator verb-verb-phrase.action))
        (if adjective (object . (*.adjective.sems noun.sems))
          (object . noun.sems)))
   
      
   ;;;  Example: he burned the food because he forgot
   ;;;  [SENTENCE] [CONJUGATION] [PRONOUN VERB]
   (s8  (sentence          -> sentence conju pronoun-phrase)
        (glitch gender-agreement if not sentence.actor = pronoun-phrase.actor)
        (actor . sentence.actor)
        (if (conju.sems = 'causation)
            (('effect-> (sentence.action) (sentence.object)) 
             ('cause-> (pronoun-phrase.actor) (pronoun-phrase.action) (sentence.object))
             ('link-> conju))
          ('error)))
   
   ;;;  Example: ?
   ;;;  [SENTENCE] [CONJUGATION] [PREPOSITION VERB ADJECTIVE]
   (s9  (sentence          -> sentence conju preposition-phrase)
        (glitch gender-agreement if not sentence.actor = preposition-phrase.actor)
        (actor . sentence.actor)
        (if (conju.sems = 'causation)
            (('effect-> (sentence.action) (sentence.object))
             ('cause-> (preposition-phrase.link) (preposition-phrase.action) (preposition-phrase.state))
             ('link-> conju))
          ('error)))
   
   ;;;  Example: she went to the gym because she wanted to become healthy
   ;;;  Example: john wore a coat because it was cold
   ;;;  [SENTENCE] [CONJUGATION] [SENTENCE]
   (s10 (sentence          -> sentence($firstS) conju sentence($secondS))
        (glitch gender-agreement if not $firstS.actor = $secondS.actor)
        (actor . $firstS.actor)
        (if (conju.sems = 'causation)
            (('effect-> ($firstS.action) ($firstS.object))
             (if $secondS.action
                 (if $secondS.State
                     ('cause-> ($secondS.action) ($secondS.state))
                   ('cause-> ($secondS.action)))
               ('cause-> $secondS.state))
             ('link-> conju.sems))
          (if (conju.sems = 'causing)
              ((if $firstS.action
                   ('cause-> ($firstS.action) ($firstS.state))
                 ('cause-> ($firstS.state)))
               ('effect-> ($secondS.action) ($secondS.object))
               ('link-> conju.sems))
            ('error))))
   
   ;;;  Example: because she wanted to become healthy she went to the gym
   ;;;  Example: since she wanted to become healthy she went to the gym
   ;;;  [CONJUGATION] [SENTENCE] [SENTENCE]
   (s11 (sentence -> conju sentence($firstS) sentence($secondS))
        (glitch gender-agreement if not $firstS.actor = $secondS.actor)
        (actor . $firstS.actor)
        (if (conju.sems = 'causation)
            (('effect-> ($secondS.action) ($secondS.object))
             (if $firstS.action
                 ('cause-> ($firstS.action) ($firstS.state))
               ('cause-> ($firstS.state)))
             ('link-> conju.sems))
          ('error)))
   
   
;;;   ;;; COND doesn't quite work.
;;;   (s12 (sentence          -> sentence($firstS) conju sentence($secondS))
;;;        (glitch gender-agreement if not $firstS.actor = $secondS.actor)
;;;        (actor . $firstS.actor)
;;;        (if (conju.sems = 'causation)
;;;            (('effect-> ($firstS.action) ($firstS.object))
;;;             ('cause-> (lisp (cond ((and $secondS.action $secondS.State)
;;;                          (list $secondS.action $secondS.state))
;;;                         ((not(null $secondS.action))
;;;                          $secondS.action)
;;;                         ((not (null $secondS.state))
;;;                          $secondS.state)
;;;                         (t
;;;                          (null)))))
;;;             ('link-> conju.sems))
;;;          (if (conju.sems = 'causing)
;;;              ((if $firstS.action
;;;                   ('cause-> ($firstS.action) ($firstS.state))
;;;                 ('cause-> ($firstS.state)))
;;;               ('effect-> ($secondS.action) ($secondS.object))
;;;               ('link-> conju.sems))
;;;            ('error))))

   ;;;  Example: become healthy
   ;;;  [VERB] [ADJECTIVE]
   (ap  (adjective-phrase -> verb adjective)
        (action . ((verb.sems) (verb.tense)))
        (state  . (adjective.sems)))
   
   ;;;  Example: she went
   ;;;  [PRONOUN] [VERB]
   (pp  (pronoun-phrase   -> pronoun verb)
        (actor  . (pronoun.sems pronoun.number))
        (action . (verb.sems verb.tense)))
   
   ;;;  Example: to become healthy
   ;;;  [PREPOSITION] [VERB ADJECTIVE]
   (pr  (preposition-phrase -> prepo adjective-phrase)
        (link   . prepo.sems)
        (action . adjective-phrase.action)
        (state  . adjective-phrase.state))
   
   ;;;  Example: (it) was cooking
   ;;;  Example: (meal) was cooking
   ;;;  [(NOUN)] [(PRONOUN)] [VERB] [VERB]
   (vvp (verb-verb-phrase -> ?noun ?pronoun verb($firstV) verb($secondV))
        (fail if (noun and pronoun))
        (if noun    (actor  . (noun.sems noun.number)))
        (if pronoun (actor  . (pronoun.sems pronoun.number)))
        (tense-indicator    . ($firstV.sems $firstV.tense))
        (action             . ($secondV.sems $secondV.tense)))

   ;;;  Example: was running
   ;;;  Example: was cooking
   ;;;  [VERB] [NOUN]
   (vn  (verb-noun -> verb noun)
        (action . verb)
        (sems   . noun))
   
   ;;;  Example: (to) the gym
   ;;;  [(PREPOSITION)] [DETERMINER] [NOUN]
   (np  (noun-phrase      -> ?prepo determiner *adjective noun)
        (if prepo
            (link . prepo.sems))
        (number . (noun.number))
        (if adjective
            (sems . (determiner.sems *.adjective.sems noun.number noun.sems))
          (sems   . (determiner.sems noun.number noun.sems))))
   
   ;;;  Example: chased the cat
   ;;;  [VERB] [(PREPOSITION) DETERMINER NOUN]
   (vp  (verb-phrase      -> verb noun-phrase)
        (action . verb.sems)
        (object . noun-phrase.sems)
        (number . verb.number)))
 )