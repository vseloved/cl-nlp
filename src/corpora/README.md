# CL-NLP / Corpora

## Purpose

Provide fundamental utilities for corpus processing (reading,
iterating over documents).

## Main concepts

The struct `corpus` defines a corpus as a set of `texts` that may
also be grouped by some keywords (`groups`).
Corpora types are represented by keywords (`:treebank`, `:brown`, etc.)

Each corpus consists of `text`s that can hold `raw`, `clean`, and `tokenized`
representations. Subclasses of `text` can also hold other stuff in them.

`tokenized` representation abides to a a paragraphs/sentences/tokens
3-level list format. If corpus doesn't delimit paragraphs, we consider
that there's 1 paragraph per text.

There are 3 generic corpus operations:

- `read-corpus-file` reads a single file from the corpus and returns
  the text (or texts - for some XML corpora, notably, Wikipedia) found
  in that file and additional attributes (if present) as secondary
  values.
  NB. For different corpora it may operate on different stuff: usually, it will
  have methods to handle streams and pathnames (both objects and strings).

- `read-corpus` reads the whole corpus and returns a `corpus` object.
  For some corpora, which are always distributed as archives, it may also take care
  of handling the decompression stuff.

- `map-corpus` maps some function over a sequence of `text` objects produced from
  individual corpus texts (read from file by `read-corpus-file` or extracted
  directly from the corpus if it's one huge file)

## Basic corpus readers

- `:brown` corpus interface

- generic `:treebank` interface creates `treebank-text` objects that have an
  additional `trees` slot. It was tested with the Penn Treebank and OntoNotes.

- for dealing with XML-based corpora the special SAX parser
  `xml-corpus-sax` is provided.  It allows to build a quick and dirty
  reader for arbitrary XML corpora, but may not capture all the meta
  information present.  Here's how it's used for parsing the Semcor
  corpus in the [nlp.contrib.corpora](../contrib/corpora/README.md):

    (defmethod read-corpus-file ((type (eql :semcor)) file &key)
      "Read individual file from the Semcor Corpus."
      (cxml:parse file (make 'xml-corpus-sax
                             :token-init 'make-semcor-token
                             :struct-map #h(:token '(:wf :punc)
                                            :sentence :s
                                            :paragraph :p)
                             :attr-map #h(:tag "pos"
                                          :cmd "cmd"
                                          :lemma "lemma"
                                          :wsn "wsn"
                                          :lexsn "lexsn"
                                          :ot "ot"
                                          :sep "sep"))))

  To parse the corpus, the parser is initialized with a map of tag(s)
  relevant for paragprah-sentence-token levels, XML attributes that
  can be extracted for tokens, and a token constructor.
