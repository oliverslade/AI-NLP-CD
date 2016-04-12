;;; This is the IDE's built-in-editor, where you create and edit
;;; lisp source code.  You could use some other editor instead,
;;; though the IDE's menu-bar commands would not be applicable there.
;;; 
;;; This editor has a tab for each file that it's editing.  You can
;;; create a new editor buffer at any time with the File | New command.
;;; Other commands such as Search | Find Definitions will create
;;; editor buffers automatically for existing code.
;;; 
;;; You can use the File | Compile and Load command to compile and
;;; load an entire file, or compile an individual definition by
;;; placing the text cursor inside it and using Tools | Incremental
;;; Compile.  You can similarly evaluate test expressions in the
;;; editor by using Tools | Incremental Evaluation; the returned
;;; values and any printed output will appear in a lisp listener
;;; in the Debug Window.
;;; 
;;; For a brief introduction to other IDE tools, try the
;;; Help | Interactive IDE Intro command.  And be sure to explore
;;; the other facilities on the Help menu.

;;; she went to the gym
;;; 

(build-lexicon
 `((she     pronoun (sems . female) (number . singular))
   (he      pronoun (sems . male)   (number . singular))
   
   (went    verb (sems . travel) (tense . past))
   (wanted  verb      )
   (burned  verb      )
   (to      prepo)
   (because conju)
   (gym     noun (sems . place) (number . singular))
   (park    noun (sems . place) (number . singular))
   (become  verb)
   (healthy adjective)
   (dinner  noun)
   (the     determiner (sems . specific))
   (a       determiner )
   (cat     noun (sems . feline) (number . singular))
   (chased  verb (sems . hunts))
   (dog     noun (sems . canine) (number . singular))
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
   
   (s3  (sentence         -> sentence conju sentence))
   
   (ap  (adjective-phrase -> verb adjective))
   
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