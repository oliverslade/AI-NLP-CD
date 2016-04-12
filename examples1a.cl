;;
;; author SCL
;; a small suite of tet grammars & lexicons
;; see alo LKit documentation
;;

(build-lexicon
  `((a      determiner )
    (cat    noun       )
    (chased verb       )
    (dog    noun       )
    (the    determiner )
    ))

(build-grammar
  '((s1 (sentence -> noun-phrase verb-phrase))
    (np (noun-phrase -> determiner noun))
    (vp (verb-phrase -> verb noun-phrase))
    ))

(parse 'sentence '(the dog chased a cat))

;---------------------------------------

(build-lexicon
  `((a      determiner any     )
    (cat    noun       feline  )
    (chased verb       hunts   )
    (dog    noun       canine  )
    (the    determiner specific)
    ))

(build-grammar
  '((s1 (sentence -> noun-phrase verb-phrase)
        (actor  . noun-phrase)
        (action . verb-phrase.action)
        (object . verb-phrase.object)
        )
    (np (noun-phrase -> determiner noun)
        (determiner . noun)
        )
    (vp (verb-phrase -> verb noun-phrase)
        (action . verb)
        (object . noun-phrase)
        )
    ))

(parse 'sentence '(the dog chased a cat))

;---------------------------------------

(build-lexicon
  `((a      determiner (sems . any)    )
    (all    determiner (sems . every)  )
    (cat    noun       (sems . feline)   (number . sing))
    (cats   noun       (sems . feline)   (number . plur))
    (chase  verb       (sems . hunts)    (number . plur))
    (chases verb       (sems . hunts)    (number . sing))
    (dog    noun       (sems . canine)   (number . sing))
    (dogs   noun       (sems . canine)   (number . plur))
    (the    determiner (sems . specific))
    ))


(build-grammar
  ; comments start with a semi-colon
  ; this grammar ignores numeric agreement with determiners
  '((s1 (sentence -> noun-phrase verb-phrase)
      (actor  . noun-phrase.sems)
      (action . verb-phrase.action)
      (object . verb-phrase.object)
      ; check number of noun-phrase & verb-phrase agree
      (if (noun-phrase.number = verb-phrase.number)	
          numeric-agreement-ok	
          numeric-agreement-bad	
          )	
      )
    (np (noun-phrase -> determiner noun)
      (number . noun.number)
      (sems   . (determiner.sems noun.sems))
      )
    (vp (verb-phrase -> verb noun-phrase)
      (action . verb.sems)
      (object . noun-phrase.sems)
      (number . verb.number)
      )))

(parse 'sentence '(the dog chases a cat))
(parse 'sentence '(the dogs chases a cat))

;---------------------------------------

(build-grammar
 '((s1 (sentence -> noun-phrase verb-phrase)
       (fail if noun-phrase.number /= verb-phrase.number)
       (actor  . noun-phrase.sems)
       (action . verb-phrase.action)
       (object . verb-phrase.object)
       )
   (np (noun-phrase -> determiner noun)
       (number . noun.number)
       (sems   . (determiner.sems noun.sems))
       )
   (vp (verb-phrase -> verb noun-phrase)
       (action . verb.sems)
       (object . noun-phrase.sems)
       (number . verb.number)
       )
   ))

(parse 'sentence '(the dog chases a cat))
(parse 'sentence '(the dogs chases a cat))

;---------------------------------------


(build-grammar
 '((s1 (sentence -> noun-phrase verb-phrase)
       (glitch numeric-agreement	
           if not noun-phrase.number = verb-phrase.number)	
       (actor  . noun-phrase.sems)
       (action . verb-phrase.action)
       (object . verb-phrase.object)
       )
   (np (noun-phrase -> determiner noun)
       (number . noun.number)
       (sems   . (determiner.sems noun.sems))
       )
   (vp (verb-phrase -> verb noun-phrase)
       (action . verb.sems)
       (object . noun-phrase.sems)
       (number . verb.number)
       )
   ))

(parse 'sentence '(the dog chases a cat))
(parse 'sentence '(the dogs chases a cat))

;---------------------------------------



