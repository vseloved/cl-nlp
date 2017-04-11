;;; (c) 2016-2017 Vsevolod Dyomkin

(in-package #:nlp.core)
(named-readtables:in-readtable rutilsx-readtable)


;;; Language-dependent context

(defvar *lang* :en
  "Current language.")

(defvar *lang-ctx-vars* ()
  "A list of global language-dependent variables.")

(defvar *lang-profiles* #h()
  "A table of known language-dependent variables values per lang.")

(defmacro def-lang-profile (lang &rest vars-forms)
  "Define profile for language LANG with "
  (with-gensyms (ctx var form)
    `(let ((,ctx #h()))
       (loop :for (,var ,form) :on (list ,@vars-forms) :by #'cddr :do
         (let ((,var (mkeyw ,var)))
           (if (member ,var *lang-ctx-vars*)
               (:= (? ,ctx ,var) ,form)
               (warn "Unknown language variable: ~A" ,var))))
       (:= (? *lang-profiles* ,lang) ,ctx))))

(defmacro def-lang-var (name init docstring &key eager)
  "Define a function NAME, that will return a singleton object,
   initialized lazily with INIT on first call.
   Also define a symbol macro <NAME> that will expand to (ACCESS-NAME).
   When EAGER will initialize the variable at definition time."
  (with-gensyms (singleton)
    (let ((var (mksym name :format "*~A*"))
          (fn (mksym name :format "ACCESS-~A")))
      `(progn
         (defvar ,var nil ,docstring)
         (defun ,fn ()
           ,docstring
           (or ,var
               (:= ,var ,init)))
         (define-symbol-macro ,(mksym name :format "<~A>") (,fn))
         ,(when eager `(,fn))
         (pushnew (mkeyw ',name) *lang-ctx-vars*)))))

(defun init-lang (lang)
  "Load language profile and other necessary data for LANG.
   The initialization data should be defined in the file langs/LANG/LANG.lisp."
  (load (lang-file lang (fmt "~(~A~).lisp" lang))))

(defun in-lang (lang)
  "Switch current lang to LANG if the appropriate language profile is defined."
  (let ((ctx (? *lang-profiles* lang)))
    (if ctx
        (progn (:= *lang* lang)
               (dolist (ctx-var *lang-ctx-vars*)
                 (when-it (? ctx ctx-var)
                   (:= (symbol-value (mksym ctx-var :format "*~A*")) it))))
        (error "No language profile for: ~A (~A)" lang (iso-lang lang)))))

(defmacro with-lang ((lang) &body body)
  "Execute BODY in the constant of current lang set to LANG
   if the appropriate language profile is defined."
  (once-only (lang)
    `(if-it (? *lang-profiles* ,lang)
            (let ((*lang* ,lang)
                  ,@(flat-map (lambda (var)
                                (when-it (? it var)
                                  `((,(mksym var :format "*~A*") ,it))))
                              *lang-ctx-vars*))
              ,@body)
            (error "No language profile for: ~A (~A)" ,lang (iso-lang ,lang)))))


;;; Language ISO codes

(defun iso-lang (iso)
  "Language name for its ISO code."
  (? +iso-639-1+ iso))

(defun lang-iso (lang)
  "Iso code of LANG."
  (dotable (iso language +iso-639-1+)
    (when (some ^(string-equal lang %)
                (split #\/ language))
      (return iso))))

(defparameter +iso-639-1+
  #h(:aa "Afar"
     :ab "Abkhazian"
     :af "Afrikaans"
     :ak "Akan"
     :sq "Albanian"
     :am "Amharic"
     :ar "Arabic"
     :an "Aragonese"
     :hy "Armenian"
     :as "Assamese"
     :av "Avaric"
     :ae "Avestan"
     :ay "Aymara"
     :az "Azerbaijani"
     :ba "Bashkir"
     :bm "Bambara"
     :eu "Basque"
     :be "Belarusian"
     :bn "Bengali"
     :bh "Bihari"
     :bi "Bislama"
     :bs "Bosnian"
     :br "Breton"
     :bg "Bulgarian"
     :my "Burmese"
     :ca "Catalan/Valencian"
     :ch "Chamorro"
     :ce "Chechen"
     :zh "Chinese"
     :cu "Church Slavic"
     :cv "Chuvash"
     :kw "Cornish"
     :co "Corsican"
     :cr "Cree"
     :cy "Welsh"
     :cs "Czech"
     :da "Danish"
     :de "German"
     :dv "Maldivian"
     :nl "Dutch/Flemish"
     :dz "Dzongkha"
     :en "English"
     :eo "Esperanto"
     :et "Estonian"
     :eu "Basque"
     :ee "Ewe"
     :fo "Faroese"
     :fa "Persian"
     :fj "Fijian"
     :fi "Finnish"
     :fr "French"
     :fy "Western Frisian"
     :ff "Fulah"
     :ka "Georgian"
     :de "German"
     :gd "Gaelic"
     :ga "Irish"
     :gl "Galician"
     :el "Greek"
     :gn "Guarani"
     :gu "Gujarati"
     :ht "Haitian"
     :ha "Hausa"
     :he "Hebrew"
     :hz "Herero"
     :hi "Hindi"
     :ho "Hiri Motu"
     :hr "Croatian"
     :hu "Hungarian"
     :hy "Armenian"
     :ig "Igbo"
     :is "Icelandic"
     :io "Ido"
     :ii "Sichuan Yi"
     :iu "Inuktitut"
     :ie "Interlingue Occidental"
     :ia "Interlingua"
     :id "Indonesian"
     :ik "Inupiaq"
     :is "Icelandic"
     :it "Italian"
     :jv "Javanese"
     :ja "Japanese"
     :kl "Greenlandic"
     :kn "Kannada"
     :ks "Kashmiri"
     :kr "Kanuri"
     :kk "Kazakh"
     :km "Central Khmer"
     :ki "Kikuyu"
     :rw "Kinyarwanda"
     :ky "Kirghiz"
     :kv "Komi"
     :kg "Kongo"
     :ko "Korean"
     :kj "Kuanyama"
     :ku "Kurdish"
     :lo "Lao"
     :la "Latin"
     :lv "Latvian"
     :li "Limburgan"
     :ln "Lingala"
     :lt "Lithuanian"
     :lb "Luxembourgish"
     :lu "Luba-Katanga"
     :lg "Ganda"
     :mk "Macedonian"
     :mh "Marshallese"
     :ml "Malayalam"
     :mi "Maori"
     :mr "Marathi"
     :ms "Malay"
     :mk "Macedonian"
     :mg "Malagasy"
     :mt "Maltese"
     :mn "Mongolian"
     :mi "Maori"
     :my "Burmese"
     :na "Nauru"
     :nv "Navajo"
     :nr "South Ndebele"
     :nd "North Ndebele"
     :ng "Ndonga"
     :ne "Nepali"
     :nl "Dutch/Flemish"
     :nn "Norwegian Nynorsk"
     :nb "Norwegian Bokmål"
     :no "Norwegian"
     :ny "Chichewa"
     :oc "Occitan"
     :oj "Ojibwa"
     :or "Oriya"
     :om "Oromo"
     :os "Ossetian"
     :pa "Panjabi"
     :fa "Persian"
     :pi "Pali"
     :pl "Polish"
     :pt "Portuguese"
     :ps "Pushto"
     :qu "Quechua"
     :rm "Romansh"
     :ro "Romanian/Moldavian"
     :rn "Rundi"
     :ru "Russian"
     :sg "Sango"
     :sa "Sanskrit"
     :si "Sinhala"
     :sk "Slovak"
     :sl "Slovenian"
     :se "Northern Sami"
     :sm "Samoan"
     :sn "Shona"
     :sd "Sindhi"
     :so "Somali"
     :st "Southern Sotho"
     :es "Spanish"
     :sq "Albanian"
     :sc "Sardinian"
     :sr "Serbian"
     :ss "Swati"
     :su "Sundanese"
     :sw "Swahili"
     :sv "Swedish"
     :ty "Tahitian"
     :ta "Tamil"
     :tt "Tatar"
     :te "Telugu"
     :tg "Tajik"
     :tl "Tagalog"
     :th "Thai"
     :bo "Tibetan"
     :ti "Tigrinya"
     :to "Tonga"
     :tn "Tswana"
     :ts "Tsonga"
     :tk "Turkmen"
     :tr "Turkish"
     :tw "Twi"
     :ug "Uighur"
     :uk "Ukrainian"
     :ur "Urdu"
     :uz "Uzbek"
     :ve "Venda"
     :vi "Vietnamese"
     :vo "Volapük"
     :cy "Welsh"
     :wa "Walloon"
     :wo "Wolof"
     :xh "Xhosa"
     :yi "Yiddish"
     :yo "Yoruba"
     :za "Zhuang"
     :zh "Chinese"
     :zu "Zulu")
  "ISO 639-1 language names.")
