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
 `((she     pronoun (sems . female) )
   (he      pronoun (sems . male)   )
   (went    verb      )
   (wanted  verb      )
   (burned  verb      )
   (to      prepo)
   (because conju)
   (gym     noun (sems . place) (numb . sing))
   (become  verb)
   (healthy adjective)
   (dinner  noun)
   (the     determiner (sems . specific))
   (a       determiner )
   (cat     noun (sems . feline) (numb . sing))
   (chased  verb (sems . hunts))
   (dog     noun (sems . canine) (numb . sing))
   ))

(build-grammar
 '((s1 (sentence         -> noun-phrase verb-phrase)
       (actor  . noun-phrase)
       (action . verb-phrase.action)
       (object . verb-phrase.object))
   (s2  (sentence         -> pronoun-phrase noun-phrase))
   (s3  (sentence         -> sentence conju sentence))
   (ap  (adjective-phrase -> verb adjective))
   (np1 (noun-phrase      -> determiner noun)
        (determiner . noun))
   (np2 (noun-phrase      -> prepo noun-phrase))
   (vp  (verb-phrase      -> verb noun-phrase))
   (pp  (pronoun-phrase   -> pronoun verb))
   ))