# How to write an English POS tagger with CL-NLP

## State-of-the-art

The problem of POS tagging is a sequence labeling task:
assign each word in a sentence the correct part of speech.
For English, it is considered to be more or less solved,
i.e. there are taggers that have around 95% accuracy.
It also has a rather high baseline: assigning each word its most probable tag
will give you up to 90% accuracy to start with.
Also, only around 10% of the words have more than 1 possible tag.
However, their usage accounts for around 40% of total text volume.

Let's start our exercise by first verifying these facts in a data-driven manner.
This will also allow us to examine the basic building blocks for our solution.

POS taggers are usually built as statistical models trained on some existing pre-labeled data.
For English language there is quite a lot of it already available.
For other languages, that don't possess such data,
an unsupervised or a rule-based approach can be applied.


## Data sources

The standard dataset that is used not only for training POS taggers,
but, most importantly, for evaluation is the
[Penn Tree Bank](https://catalog.ldc.upenn.edu/LDC99T42) Wall Street Journal dataset.
It contains of not only POS tag, but also noun phrase and parse tree annotations.

Here's an example of the combined POS tag and noun phrase annotations from this corpus:

    [ Pierre/NNP Vinken/NNP ]
    ,/,
    [ 61/CD years/NNS ]
    old/JJ ,/, will/MD join/VB
    [ the/DT board/NN ]
    as/IN
    [ a/DT nonexecutive/JJ director/NN Nov./NNP 29/CD ]
    ./.

The tagset used in the annotation contains such symbols as
`NNP` for proper nouns, `,` for commas, and `CD` for cardinal numbers.
The whole set is provided for `CL-NLP` in the file `src/syntax/word-tags.txt`.
Here's a snippet from it:

    X Unknown, uncertain, or unbracketable. X is often used for bracketing typos and in bracketing the...the-constructions.
    CC Coordinating conjunction
    ...

It's, obviously, possible to extend it with other tags if necessary.
All of them are, finally, available as symbols of the `tag` package in `CL-NLP`.


## Available data and tools to process it

What we're interested in, is obtaining a structured representation of this data.
The `ncorp` package implements interfaces to various raw representations,
such as this one.

Different NLP corpora exist in various formats:

- slightly annotated text, like the above
- XML-based formats (a well-known example is the Reuters corpus)
- a treebank format used for representing syntax trees
- and many others

Most of these representations are supported by the `ncorp` adapters at least to some extent.
The interface of this module consists of the following entities:

- a `text` structure for representing an individual text of the corpus
  (most of the corpora are, thankfully, divided into separate files)
- a generic function `read-corpus-file` that should read a raw file
  and return as multiple values its several representations.
  The common representations supported by almost all its methods are:
  `raw` or almost raw text, `clean` text - just sentences stripped of all annotations,
  and `tokens` - a list of token lists extracted from each sentence.
  Additionally, other corpus-specific slots may be present.
  For example, a `treebank-text` structure adds a `trees` slot to hold
  the syntax trees extracted from the treebank
- a generic function `read-corpus` creates a `corpus` structure out of all corpus' texts.
  In practice, it's usually not feasible to do that due to large sizes
  of most of the useful corpora. That's why the next function is more practical
- a generic function `map-corpus` reads and streams each corpus file as a `text` structure
  to the argument function. This is the default way to deal with corpus processing

For our task we'll be able to utilize just the `tokens` slot
of the `ptb-tagged-text` structure, produced with `map-corpus`.
Let's collect the tag distribution for each word from the WSJ section of the PTB:

    NLP> (let ((words-dist #h(equal))
           (map-corpus :ptb-tagged (corpus-file "ptb/TAGGED/POS/WSJ")
                       #`(dolist (sent (text-tokens %))
                           (dolist (tok sent)
                             (unless (in# (token-word tok) words-dist)
                               (:= (get# (token-word tok) words-dist) #h()))
                             (:+ (get# (token-tag tok)
                                       (get# (token-word tok) words-dist)
                                       0))))
                       :ext "POS")
           words-dist)
    #<HASH-TABLE :TEST EQUAL :COUNT 51457 {10467E6543}>
    NLP> (reduce #'+ (mapcan #'ht-vals (ht-vals *)))
    1289201

So, we have around 50k distinct words and 1,3m tokens.

But, actually, the resources in the field has made some progress in the last decades,
and there's a bigger corpus now available that contains not only the whole Penn Tree Bank,
but also some more data from other domains. The annotations of the WSJ section in it
were also improved. It is called [OntoNotes](https://catalog.ldc.upenn.edu/LDC2013T19).
Let's do the same with its data:

    NLP> (let ((words-dist #h(equal)))
           (map-corpus :treebank (corpus-file "ontonotes/")
                       #`(dolist (sent (text-tokens %))
                           (dolist (tok sent)
                             (with-accessors ((tag token-tag) (word token-word)) tok
                               (unless (eql tag 'tag:-NONE-)
                                 (unless (in# word words-dist)
                                   (:= (get# word words-dist) #h()))
                                 (:+ (get# tag (get# word words-dist) 0))))))
                       :ext "parse")
           words-dist)
     #<HASH-TABLE :TEST EQUAL :COUNT 60925 {1039EAE243}>

So, in the OntoNotes 4.0 there are 60925 distinct words.
50925 of them (~84%) are tagged with a single tag.
I.e. we have a 16% of multi-tag words which corresponds well with the theoretical data.
Also, there are 2,1m tokens in the corpus in total.

Calculating the number of words with distinct tags:

    (count-if-not #`(= 1 (ht-count (rt %)))
                  (ht->pairs words-dist))

And what about the total volume?

    NLP> (let ((total1 0)
               (total 0))
          (map-corpus :treebank "ontonotes"
                      #`(dolist (sent (text-tokens %))
                          (dolist (tok sent)
                            (unless (eql (token-tag tok) 'tag:-NONE-)
                              (:+ total)
                              (when (in# (token-word tok) *single-tag-words*)
                                (:+ total1)))))
                      :ext "parse")
          (float (/ total1 total)))
    0.2409884

Only 24% instead of 60%! What's wrong?

OK, here's the trick: let's add words that have more than 1 tag,
but >99% of their occurrences are labeled with a single tag.
For instance, the word `"the"` has 9 distinct tags in OntoNotes,
but 0.9997 of the times it's a `DT`.

If we consider such words to have a single tag, we'll get just a slight
increase in the number of single-tag words (+386: 51302 instead of 50916),
but a dramatic increase in the volume of their occurrence - now it's 63%!
Just as the literature tells us.

(NB. Taking such shortcut will only increase the quality of the POS tagger
as 99% is above the accuracy it will be able to achieve anyhow,
which is at most 97% on the same-domain data and even lower for out-of-domain data.)

Here's how we can determine such set of words:

    (remove-if-not #`(let ((tag-dist (ht-vals (rt %))))
                       (> (/ (reduce #'max tag-dist)
                             (reduce #'+   tag-dist))
                          0.99))
                   (ht->pairs tag-dist))

NB. The above code samples contain some non-standard utilities and idioms
that may look somewhat alien to some Lisp programmers.
All of them are from my [RUTILS](http://github.com/vseloved/rutils) library,
and you'll see more below. Mostly, these include some hash-table-specific operators,
new iteration constructs, a few radical abbreviations for common operations,
and literal syntax for hash-tables (`#h()`) and anonymous functions (#`()).

Some of them are:

- `get#`/`in#`/`set#` specialized hash-table access routines,
   and `dotable` hash-table iteration
- a pair data type with `lt`/`rt` accessors and conversion routines to/from hash-tables
- `?` generic element access operator with support for generic `setf`,
  plus `:=` abbreviation for `setf` (that is using a common assignment symbol)
  and `:+` analogy for `incf`


## Building the POS tagger

We have explored how to access different corpus data that we'll need to train the POS tagger.
To actually do that, we'll re-implement the approach described by Matthew Honnibal in
"[A good POS tagger in about 200 lines of Python](http://honnibal.wordpress.com/2013/09/11/a-good-part-of-speechpos-tagger-in-about-200-lines-of-python/)".
In fact, due to the expressiveness of Lisp and efficiency of SBCL,
we'll need even less than 200 lines, and we'll get the performance
comparable to a much more complex Cyton implementation of the parser
(6s against 4s on 130k tokens), but that's details... ;)

Here's the source code we'll be discussing below on [github](https://github.com/vseloved/cl-nlp/blob/master/src/tagging/greedy-ap.lisp).

Our tagger will use a greedy averaged perceptron model with single-tag words dictionary lookup:

    (defclass greedy-ap-tagger (avg-perceptron tagger)
      ((dict :initform #h(equal) :initarg :dict :accessor tgr-dict)
       (single-tag-words :initform #h(equalp) :initarg :single-tag-words
                         :accessor tgr-single-tag-words))
      (:documentation
       "A greedy averaged perceptron tagger with single-tag words dictionary lookup."))

As you see, it is derived from a generic class `tagger` and an `avg-perceptron` learning model.
It also has a `dict` slot that holds all the words known to the tagger.

Every `tagger` has just one generic function associated with it.
You guessed its name - `tag` :)

    (defmethod tag ((tagger greedy-ap-tagger) (sentence sentence))
      (let ((prev :start)
            (prev2 :start2)
            (ctx (sent-ctx sentence)))
        (doindex (i token (sent-tokens sentence))
          (:= (token-tag token)
              (classify tagger
                        (extract-fs tagger i (token-word token) ctx prev prev2))
              prev2 prev
              prev (token-tag token)))
        sentence))

It accepts an already tokenized `sentence` and (destructively) assigns tags
to each of its tokens.

The main job is performed by the call to `classify` method that is defined for every
statistical learning model in `CL-NLP`. Another model-associated method here is `extract-fs`
which produces a list of features that describe the current sample.

Now, let's take a look at the implementation of these learning model-related methods.

    (defmethod classify ((model greedy-ap-tagger) fs)
      (or (get# (first fs) (tgr-single-tag-words model))
          (call-next-method model (rest fs))))

For the tagger, we first check the current word against the dictionary of
`single-tag-words` that we've built in the previous part
and then call the `classify` method of the `avg-perceptron` model itself.
That one is a matter of simply returning a class that is an argmax
of a dot product between model weights and sample features `fs`
that in this case can only have values of 1 or 0.

    (defmethod classify ((model greedy-ap) fs)
      (let ((scores #h()))
        (dotable (class weights (m-weights model))
          (dolist (f fs)
            (:+ (get# class scores 0) (get# f weights 0))))
        (keymax scores)))  ; keymax is argmax for hash-tables

As you see, averaged perceptron is very straightforward - a simple linear model
that has a `weights` slot which is a mapping of feature weights for every class.
In the future this method will probably be assigned to a `linear-model` class,
but it hasn't been added to `CL-NLP` so far.


## Training

Let's take a look at the training part. It consists of 2 steps.
`extract-fs` performs feature extraction.
What it, basically, does in our case of a simple perceptron model
is returning a list of features preceded by the word we're currently tagging.

    (cons word (make-fs model
                        "bias"
                        ("i pref1" (char word 0))
                        ("i word" word)
                        ("i-1 tag" prev-tag)
                        ...

The `make-fs` macro is responsible for interning the features as symbols
in package `f` by concatenating the provided prefixes and calculated variables.
This is a standard Lisp practice to use symbols instead of raw strings for such things.
So, in the above example for the word `"the"` preceded by a word tagged as `VBZ`
will get the following list of features:

    '("the" f:|bias| f:|i pref1 t| f:|word the| f:|i-1 tag VBZ| ...)

The second part of learning is training. It is the most involved procedure here,
yet still very simple conceptually. Just like with the `tag` method,
we're iterating over all tokens preceded by a dummy `:-start2-` and `:-start-` ones,
and guessing the current tag using `classify`.
Afterwards we're updating the model's weights in `train1`.
The only difference is that we need to explicitly first consider the case
of `single-tag-words` not to run the model update step for it.

This is how it all looks modulo debugging instrumentation.
Note the use of `psetf` to update `prev` and `prev2` simultaneously.

    (defmethod train ((model greedy-ap-tagger) sents &key (epochs 5))
      (with-slots (single-tag-words dict) model
        ;; expand dict
        (dolist (sent sents)
          (dolist (tok (sent-tokens sent))
            (set# (token-word tok) dict nil)))
        ;; expand single-tag-words
        (dotable (word tag (build-single-tag-words-dict (mapcar #'sent-tokens sents)
                                                        :ignore-case? t))
          (unless (in# word single-tag-words)
            (set# word single-tag-words tag)))
        ;; train
        (dotimes (epoch epochs)
          (dolist (sent (mapcar #'sent-tokens sents))
            (let* ((prev :-start-)
                   (prev2 :-start2-)
                   (ctx (sent-ctx sent)))
              (doindex (i token sent)
                (let ((word (token-word token)))
                  (psetf prev
                         (or (get# word single-tag-words)
                             (let* ((fs (extract-fs model i word ctx prev prev2))
                                    (guess (classify model fs)))
                               (train1 model (rest fs) (token-tag token) guess)
                               guess))
                         prev2 prev)))))
          (:= sents (shuffle sents))))
      model)

Note the additional expansion of the `single-tag-words` dict of the model
(as well as of the normal `dict`).

An interesting feature of the problem's object-oriented decomposition in this case is that
we have a generic perceptron machinery we'd like to capture and reuse for different
concrete models, and a model-specific implementation details.

This dichotomy is manifested in the training phase:

- the `train` method is specific to the `greedy-ap-tagger`.
  The generic `perceptron` training is much simpler, because it doesn't operate
  in a sequence labeling scenario (see the [source code here](https://github.com/vseloved/cl-nlp/blob/master/src/learning/perceptron.lisp#L22))
- however, there's also an `:after` method defined for `train` on the `avg-perceptron` model
  which averages all the weights in the end and prunes the model by removing zero weights
- there are also 2 more methods that are not specialized for `greedy-ap-tagger`:
  `train1` and `update1`. They perform 1 step of the normal perceptron training
  and model update

    (defmethod train1 ((model perceptron) fs gold guess)
      (:+ (ap-step model))
      (dolist (f fs)
        (ensure-f-init model f gold guess)
        (loop
           :for class :in (list gold guess)
           :for val :in '(1 -1) :do
           (update1 model f class val))))

    (defmethod update1 ((model avg-perceptron) f class val)
      (with-slots (step timestamps weights totals) model
        (:+ (? totals class f) (* (- step (? timestamps class f))
                                  (? weights class f)))
        (:+ (? weights class f) val)
        (:= (? timestamps class f) step)))


## Evaluation & persisting the model

We have reached the last part of every machine learning exercise - evaluation.
Usually it's about measuring precision/recall/f-measure, but in the tagger case
both precision and recall are the same, because the sets of relevant and retrieved items
are the same, so we can calculate just the accuracy:

    NLP> (accuracy *tagger* *gold-test*)
    ....................................................................................................
    96.39183

A "gold" corpus is used for evaluation. This one was performed
on the standard evaluation set which is the Wall Street Journal corpus (parts 22-24),
OntoNotes 4.0 version. The model was also trained on the standard training set (0-18).
Its results are consistent with the performance of the reference model from the blog post.
The "gold" features where obtained by calling the `extract-gold` method
of our model on the data from the treebank.

But wait, we can do more.

First, on the evaluation part. It's not being a secret already for a long time
in the NLP community that WSJ corpus is far from representative to the real-world use cases.
And I'm not even talking of twitter here, but just various genres of writing have
different vocabularies and distributions of sentence structures.
So, the high baselines shown by many results on the WSJ corpus may not be that robust.
To help with such kind of evaluation Google and Yahoo have recently released another treebank
called WebText that collect 5 different types of texts seen on the web:
from dialogues to blog posts. It's smaller than Penn Treebank: 273k tokens isntead of 1,3m
with 23k distinct word types. If we evaluate on it the accuracy drops substantially:
only 89.74406!

What we can do is train on more data with better variability.
Let's retrain our model on the whole OntoNotes (minus the evaluation set of WSJ 22-24).
Here are the results:

- on WSJ evaluation set: 96.76323 - a modest gain of 0.4%: we're already at max here
- on Webtext: 92.9431 - a huge gain of more than 4%!

So, broader data helps. What else can we do?

Another aspect we haven't touched is normalization.
There are some variants of generating arbitrary tokens in English which lend themselves
well to normalization to some root form.
These include numbers, emails, urls, and hyphenated words.
The normalization variant proposed by Honnibal is rather primitive and can be improved.

Here's an original variant:

    (defmethod normalize ((model greedy-ap-tagger) (word string))
      (cond
        ((and (find #\- word) (not (char= #\- (char word 0))))
         "!HYPHEN")
        ((every #'digit-char-p word)
         (if (= 4 (length word)) "!YEAR" "!DIGITS"))
        (t (string-downcase word))))


And here's a modified one:

    (defmethod normalize ((model greedy-ap-tagger) (word string))
      (cond-it
        ((re:scan *number-regex* word) (make-string (length word) :initial-element #\0))
        ((re:scan *email-regex* word) "!EMAIL")
        ((re:scan *url-regex* word) "!URL")
        ((in# word (tgr-dict model)) (string-downcase word))
        ((position #\- word :start 1 :from-end t)
         (let ((suffix (slice word (1+ it))))
           (if (in# suffix (tgr-dict model))
               (string-downcase suffix)
               "!HYPH")))
        (t (string-downcase word))))

Such change allows to gain another 0.06% accuracy on the Webtext corpus.

Now, as we finally have the best model we need a way to persist and restore it.
The corresponding `save-model`/`load-model` methods exist for any categorical model.
They use the handy [ZIP](http://common-lisp.net/project/zip/) and
[USERIAL](http://nklein.com/software/unet/userial/)
libraries to save models into a single zip file,
serializing textual (categories and feature names) and binary data (floating point weights)
into separate files. Here's how our serialized POS tagger model looks like:

      Length  File
    --------  --------------------
         552  classes.txt
     4032099  fs.txt
     2916012  fs.bin
     2916012  weights.bin
       35308  single-tag-words.txt
      484712  dict.txt
    --------  --------------------
    10384695  6 files

Finally, I believe, it's an essential practice to make all results we post online reproducible,
but, unfortunately, there are restrictions on the use of the Pen Treebank corpus data,
so we can't just add an automated test that will reproduce the contents of this post.
Still, a small portion of OntoNotes WSJ corpus can be used under the fair use policy,
and it is provided with `CL-NLP` for evaluation purposes.

Let's add such a test to give the users confidence in the performance of our model.
For testing `CL-NLP` I'm using yet another my own library
which is called [SHOULD-TEST](http://github.com/vseloved/should-test) -
I'll have another blog devoted to it some time in the future.

Here's a test we need:

    (defun extract-sents (text)
      (mapcar #`(make 'ncore:sentence :tokens (ncorp:remove-dummy-tokens %))
              (ncore:text-tokens text)))

    (defvar *tagger* (load-model (make 'greedy-ap-tagger)
                                 (models-file "pos-tagging/onf.zip")
                                 :classes-package :tag))
    (defvar *gold*
      (let (test)
        (ncorp:map-corpus :treebank (corpus-file "onf-wsj/")
                          #`(appendf test (extract-sents %)))
        (extract-gold *tagger* test)))

    (deftest greedy-ap-tagger-quality ()
      (should be = 96.31641
              (accuracy *tagger* *gold*)))


## Summing up

In this article I've tried to describe the whole process of creating
a new statistics-based model using `CL-NLP`.
As long as you have the necessary data, it is quite straightforward and commonplace.

If you want to use one of the existing models (namely, greedy averaged perceptron, as of now)
you can reuse almost all of the machinery and just add a couple of functions
to reflect the specifics of your task. I think, it's a great demonstration of the power
of the generic programming capabilities of the CLOS.

Obviously, feature engineering is on you,
but training/evaluation/saving/restoring the model can be handled transparently
by `CL-NLP` tools. There's also support for common data processing and calculation tasks.
We have looked at some of the popular corpora in this domain
(which all, unfortunately, have some usage restrictions and are not readily available,
but can be obtained for research purposes). And we've observed some of factors that impact
the performance and robustness of machine learning models.
I'd say that our final model is of the production-ready state-of-the-art level,
so you can safely use it for your real-world tasks
(under the licensing restrictions of the OnotoNotes dataset used for training it).
Surely, if you have your own data, it should be straightforward to retrain the model with it.

You can also add your own learning algorithms,
and I'm going to be continue doing the same likewise.

Stay tuned and have fun!
