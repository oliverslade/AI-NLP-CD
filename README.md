# AI-NLP-CD
Conceptual dependent natural language processing AI 

ToDo:
  - Add attributes to objects/actions.
  - More complex sentences.

Focus on relationships such as: 
  - “She went to the gym BECAUSE she wanted to get healthy” 
  - “He burned the dinner BECAUSE he forgot”
  - “STOP eating bad foods in order to get healthy”
  - “He wanted more money to buy a FAST CAR”
  - "She wanted to get healthy SO she went to the gym"
  - "The Moon has gravitational pull, CONSEQUENTLY the oceans have tides."
  - "SINCE helium rises a helium balloon floats."
  - "Water is formed WHEN two hydrogen atoms and one oxygen atom combine."

Add attributes to objects/actions:
  - "The STUPID man BURNED the meal because he forgot it was cooking."
  
#####The term stupid refers to the concept (stupidity or intelligence) which cannot stand alone conceptually. The concept man can stand alone and is modified, conceptually by ‘stupid’, so it is realised in the network as a governor with its dependant.

Book thingy: http://dl.acm.org/citation.cfm?id=990405 

Load Order:
  - Load Utils
  - Load Matcher
  - Enter: (use-package 'matcher)
  - Load LKIT
  - Load Tests

Research:
  - Roger Schank - A Conceptual Dependancy Parser For Natural Language
    - Real world object with attributes (Big/Small, Fat/Skinny, Smart/Stupid).
    - Real world actions with attributes.
    - Locations.
    - Times.

Resources:
  - WordNet: wordnet.princeton.edu
  - VerbNet: http://verbs.colorado.edu/~mpalmer/projects/verbnet.html
  - FrameNet: https://framenet.icsi.berkeley.edu/fndrupal/ <----- Best
  - Roger Schank: http://dl.acm.org/citation.cfm?id=990405

Report:
  - Cry 
  - Have existential crisis
  - Cry some more
  - Possibility of using Op-Search and Lexicon to transition real world objects between states.
  - Write about Governing Categories / Assisting Categories.
