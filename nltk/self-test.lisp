;;; (c) 2013 Vsevolod Dyomkin

(in-package #:nltk)
(named-readtables:in-readtable rutils-readtable)

(defmacro check (form result &key (short t))
  (with-gensyms (f rez e)
    (once-only (result)
      `(handler-case
           (let* ((,f ,form)
                  (,rez (if (stringp ,result)
                            (re:scan ,f ,result)
                            (equal ,f ,result))))
             (format *debug-io* "~%Checking ~S" ',form)
             (let ((*print-length* (when ,short 3)))
               (format *debug-io* "~%= ~S" ,result))
             (format *debug-io* "~%~:[F~;T~]~%" ,rez)
             (unless ,rez
               (format *debug-io* "Got ~S~%" ,f))
             ,rez)
         (error (,e)
           (format *debug-io* "~%Error ~A~%" ,e))))))


;;; ch 1-1

(load-nltk-texts "data/")

(defvar *moby* (get# :moby *texts*))
(defvar *genesis* (get# :genesis *texts*))
(defvar *chat* (get# :nps-chat *texts*))
(defvar *sense* (get# :sense *texts*))
(defvar *inaugural* (get# :inaugural *texts*))

(check (sub (with-output-to-string (*standard-output*)
              (concordance *moby* "monstrous"))
            0 27)
       "Displaying 11 of 11 matches")

(check (sub (string-trim '(#\Space #\Newline)
                         (with-output-to-string (*standard-output*)
                           (concordance *genesis* "lived" :pass-newlines t)))
            0 159)
       "Displaying 75 of 75 matches
t from Yahweh's presence, and lived in the land of Nod, east of E
 when they were created. Adam lived one hundred thirty years, and")

(check (similar *moby* "monstrous")
       '("mystifying" "subtly" "maddens" "impalpable" "modifies" "vexatious" "candid" "exasperate" "doleful" "delightfully" "trustworthy" "domineering" "abundant" "puzzled" "untoward" "contemptible" "gamesome" "reliable" "mouldy" "determined"))

(check (similar *sense* "monstrous")
       '("amazingly" "vast" "heartily" "extremely" "remarkably" "great" "exceedingly" "sweet" "very" "so" "good" "a" "as"))

(check (common-contexts *moby* "monstrous" "loving")
       '("most_and"))

(check (common-contexts *moby* "monstrous" "mystifying")
       '("most_and"))

(check (apply #'common-contexts *moby* (similar *moby* "monstrous"))
       '("most_and"))

(check (length (text-words *genesis*))
       44671)

(check (common-contexts *sense* "monstrous" "very")
       '("am_glad" "be_glad" "a_pretty" "is_pretty" "a_lucky"))

(check (take 20 (sort (remove-duplicates (text-words *genesis*) :test 'string=)
                      'string<))
       '("!" "\"" "'" "(" ")" "," "-" "." ":" ";" "?" "A" "Abel" "Abida" "Abimael" "Abimelech" "About" "Abraham" "Abram" "Accad"))

(check (length (remove-duplicates (text-words *genesis*) :test 'string=))
       2634)

(check (count "smote" (text-words *genesis*) :test 'string=)
       0)

(check (count "Abraham" (text-words *genesis*) :test 'string=)
       134)

(check (with-slots (words) *inaugural*
         (percentage (count "a" words :test 'string=) (length words)))
       1.4462992)

(check (lexical-diversity *genesis*)
       16.959377)

(check (lexical-diversity *chat*)
       6.9837084)  ; 12.65077


;;; ch 1-2

(defvar sent '(1 2 3 4 5 6 7 8 9 0))

(check (replace sent '("Second" "Third") :start1 1 :end1 9)
       '(1 "Second" "Third" 4 5 6 7 8 9 0))

(check (freq (text-bigrams *moby*) "The whale")
       14)

(check (logprob (text-bigrams *moby*) "The whale")
       -14.255592)

(defvar *1grams* (text-ugrams *moby*))

(check (freq *1grams* "whale")
       906)

(check (take 50 (vocab *1grams* :order-by '>))
       '("," "the" "<S>" "</S>" "." "of" "and" "-" "a" "to" ";" "in" "\"" "that" "'" "his" "it" "I" "!" "s" "is" "he" "with" "was" "as" "all" "for" "this" "at" "by" "but" "not" "him" "from" "be" "on" "?" "so" "whale" "one" "you" "had" "have" "there" "But" "or" "were" "now" "which" "me"))

(check (take 50 (hapaxes (text-ugrams *moby*)))
       '("orphan" "retracing" "sheathed" "padlocks" "dirgelike" "Buoyed" "liberated" "Till" "Ixion" "closing" "suction" "halfspent" "THEE" "ESCAPED" "ONLY" "Epilogue" "thrill" "etherial" "intercept" "incommoding" "tauntingly" "backwardly" "coincidings" "ironical" "intermixingly" "whelmings" "inanimate" "animate" "lookouts" "infatuation" "Morgana" "Fata" "gaseous" "mediums" "bewildering" "bowstring" "mutes" "voicelessly" "THUS" "grapple" "unconquering" "comber" "foregone" "bullied" "uncracked" "unsurrendered" "Diving" "flume" "dislodged" "buttress"))

(check (sort (remove-if #`(<= (length %) 15)
                        (uniq (text-words *moby*)))
             'string<)
       '("CIRCUMNAVIGATION" "Physiognomically" "apprehensiveness" "cannibalistically" "characteristically" "circumnavigating" "circumnavigation" "circumnavigations" "comprehensiveness" "hermaphroditical" "indiscriminately" "indispensableness" "irresistibleness" "physiognomically" "preternaturalness" "responsibilities" "simultaneousness" "subterraneousness" "supernaturalness" "superstitiousness" "uncomfortableness" "uncompromisedness" "undiscriminating" "uninterpenetratingly"))

(check (remove-if #`(re:scan ".*sUser.*" %)
                  (sort (remove-if #`(or (<= (length %) 7)
                                         (<= (freq (text-ugrams *chat*) %) 7))
                                   (vocab (text-ugrams *chat*)))
                       'string<))
       '("Question" "actually" "anything" "computer" "everyone" "football" "innocent" "listening" "remember" "seriously" "something" "talkcity_adults" "thinking" "together" "watching"))

(defvar *moby-lm2* (make-lm 'stupid-backoff-lm
                            :1g (text-ugrams *moby*)
                            :2g (text-bigrams *moby*)))

(check (prob *moby-lm2* "This is a test sentence.")
       6.1382114e-20)

(check (prob *moby-lm2* '("<S>" "Moby" "Dick" "." "</S>"))
       5.0842726e-9)

(check (float (prob (text-bigrams *moby*) '("Moby" "Dick")))
       3.0310222e-4)

(check (length (split #\Space (generate *genesis* :order 2 :n 93)))
       93)

(check (length (split #\Space (generate *genesis* :order 3 :n 93)))
       93)

(check (collocations *inaugural*)
       '(("United" "States") ("fellow" "citizens") ("four" "years") ("years" "ago") ("Federal" "Government") ("General" "Government") ("American" "people") ("Vice" "President") ("Old" "World") ("Almighty" "God") ("Fellow" "citizens") ("Chief" "Magistrate") ("Chief" "Justice") ("God" "bless") ("go" "forward") ("every" "citizen") ("Indian" "tribes") ("public" "debt") ("one" "another") ("foreign" "nations") ("political" "parties") ("State" "governments") ("National" "Government") ("United" "Nations") ("public" "money") ("national" "life") ("beloved" "country") ("upon" "us") ("fellow" "Americans") ("Western" "Hemisphere")))

(defvar *moby-lengths* (index-ngrams 1 (mapcar #'length (text-words *moby*))))

(check (vocab *moby-lengths*)
       '(1 4 2 6 8 9 11 5 7 3 10 12 13 14 16 15 17 18 20))

(check (ngrams-pairs *moby-lengths*)
       '((1 . 58368) (4 . 42273) (2 . 35726) (6 . 17111) (8 . 9966) (9 . 6428) (11 . 1873) (5 . 26595) (7 . 14399) (3 . 49633) (10 . 3528) (12 . 1053) (13 . 567) (14 . 177) (16 . 22) (15 . 70) (17 . 12) (18 . 1) (20 . 1)))

(check (ngrams-max-freq *moby-lengths*)
       58368)

(check (freq *moby-lengths* 3)
       49633)


;;; ch 2-3

(wordnetconnect-wordnet <wordnet>)

(check (princ-to-string (wn:synsets "motorcar"))
       "(#<SYNSET auto.n.1 102958343 {100EF25AD3}>)")

(check (princ-to-string (wordnet:synsets <wordnet> "motorcar"))
       "(#<SYNSET auto.n.1 102958343 {10116B0003}>)")

(check (wn:words (wn:synset "car.n.1"))
       '("auto" "automobile" "car" "machine" "motorcar"))

(check (wordnet:synset-def (wn:synset "car.n.1"))
       "a motor vehicle with four wheels; usually propelled by an internal combustion engine")

(check (wn:examples (wn:synset "car.n.1"))
       '("he needs a car to get to work"))

(check (princ-to-string (wn:lemmas (wn:synset "car.n.1")))
       "(#<LEMMA auto 9953 {100F25C2B3}> #<LEMMA automobile 10063 {100F25EC93}>
 #<LEMMA car 20722 {100F25F9F3}> #<LEMMA machine 80312 {100F260793}>
 #<LEMMA motorcar 86898 {100F2614F3}>)")

(check (princ-to-string (remove-if-not #`(string= "automobile"
                                                  (wordnet:lemma-word %))
                                       (wn:lemmas (wn:synset "car.n.1"))))
       "(#<LEMMA automobile 10063 {.*}>)")

(check (princ-to-string (wn:sense "car~automobile.n.1"))
       "#<SENSE car~auto.n.1 28261 {.*}>")

(check (princ-to-string (wn:synset (wn:lemma 10063)))
       "#<SYNSET auto.n.1 102958343 {.*}>")

(check (with-output-to-string (*standard-output*)
         (dolist (synset (wn:synsets "car"))
           (print (wn:words synset))))
       #/("cable car" "car")
("auto" "automobile" "car" "machine" "motorcar")
("car" "railcar" "railroad car" "railway car")
("car" "elevator car")
("car" "gondola")/#)

(defvar *motorcar* (wn:synset "car.n.1"))
(defvar *types-of-motorcar* (wn:related *motorcar* :hyponym))
(nth 0 *types-of-motorcar*)

(check (princ-to-string (nth 0 *types-of-motorcar*))
       "#<SYNSET ambulance.n.1 102701002 {.*}>")

(check (sort (mapcan #'wn:words *types-of-motorcar*) 'string<)
       '("ambulance" "beach waggon" "beach wagon" "bus" "cab" "compact"
         "compact car" "convertible" "coupe" "cruiser" "electric"
         "electric automobile" "electric car" "estate car" "gas guzzler"
         "hack" "hardtop" "hatchback" "heap" "horseless carriage" "hot rod"
         "hot-rod" "jalopy" "jeep" "landrover" "limo" "limousine" "loaner"
         "minicar" "minivan" "model t" "pace car" "patrol car"
         "phaeton" "police car" "police cruiser" "prowl car" "race car" "racer"
         "racing car" "roadster" "runabout" "s.u.v." "saloon" "secondhand car"
         "sedan" "sport car" "sport utility" "sport utility vehicle" "sports car"
         "squad car" "stanley steamer" "station waggon" "station wagon"
         "stock car" "subcompact" "subcompact car" "suv" "taxi" "taxicab"
         "tourer" "touring car" "two-seater" "used-car" "waggon" "wagon"))

(check (princ-to-string (wn:related *motorcar* :hypernym))
       "(#<SYNSET automotive vehicle.n.1 103791235 {.*}>)")

(defvar *paths* (wn:hypernym-paths *motorcar*))

(check (length *paths*)
       2)

(check (mapcar #'synset-name (nth 0 *paths*))
       '("entity.n.1" "physical entity.n.1" "object.n.1" "unit.n.6"
         "artefact.n.1" "instrumentality.n.3" "conveyance.n.3" "vehicle.n.1"
         "wheeled vehicle.n.1" "self-propelled vehicle.n.1"
         "automotive vehicle.n.1" "auto.n.1"))

(check (mapcar #'synset-name (nth 1 *paths*))
       '("entity.n.1" "physical entity.n.1" "object.n.1" "unit.n.6"
         "artefact.n.1" "instrumentality.n.3" "container.n.1"
         "wheeled vehicle.n.1" "self-propelled vehicle.n.1"
         "automotive vehicle.n.1" "auto.n.1"))

(check (princ-to-string (remove-duplicates (mapcar #'car *paths*)))
       "(#<SYNSET entity.n.1 100001740 {.*}>)")

(check (wn:related (wn:synset "tree.n.1") :substance-meronym)
       nil)

(check (princ-to-string (wn:related (wn:synset "tree.n.1")
                                    :part-meronym :reverse t))
       "(#<SYNSET stump.n.1 113111504 {.*}>
#<SYNSET crown.n.7 113128003 {.*}>
#<SYNSET limb.n.2 113163803 {.*}>
#<SYNSET bole.n.2 113165815 {.*}>
#<SYNSET burl.n.2 113166044 {.*}>)")

(check (princ-to-string (wn:related (wn:synset "tree.n.1")
                                    :substance-meronym :reverse t))
       "(#<SYNSET sapwood.n.1 113097536 {.*}>
#<SYNSET duramen.n.1 113097752 {.*}>)")

(check (princ-to-string (wn:related (wn:synset "tree.n.1")
                                    :member-holonym :reverse t))
       "(#<SYNSET forest.n.1 108438533 {.*}>)")

(check (princ-to-string (wn:related (wn:synset "tree.n.1") :member-meronym))
       "(#<SYNSET forest.n.1 108438533 {.*}>)")

(check (with-output-to-string (*standard-output*)
         (dolist (s (wn:synsets "mint" :pos #\n))
           (format t "~A: ~A~%" (synset-name s) (synset-def s))))
       "mint.n.6: a plant where money is coined by authority of the government
mint.n.5: a candy that is flavored with a mint oil
mint.n.4: the leaves of a mint plant used fresh or candied
mint.n.3: any member of the mint family of plants
mint.n.2: any north temperate plant of the genus Mentha with aromatic leaves and small mauve flowers
batch.n.2: (often followed by `of') a large number or amount or extent")

(check (princ-to-string (wn:related (wn:synset "mint.n.4")
                                    :part-holonym :reverse t))
       "(#<SYNSET mint.n.2 112855042 {.*}>)")

(check (princ-to-string (wn:related (wn:synset "mint.n.4")
                                    :substance-holonym :reverse t))
       "(#<SYNSET mint.n.5 107606278 {.*}>)")

