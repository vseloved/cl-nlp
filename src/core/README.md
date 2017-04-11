# CL-NLP CORE

## Purpose

Define basic operations and building blocks for other CL-NLP packages.

## Main concepts

### Tokens

Struct `token` defines a `word` which can have a position in text (`beg`, `end`),
`id` number in a sentence, POS `tag`, and `lemma` (base form).

Class `sentence` is a list of `token`s.

Operation `tokenize` splits a plain text chunk into smaller ones.
The hierarchy is the following:

    text > paragraph > sentence > token

`tokenize` returns a list of chunks and a list of spans corresponding to them.

The following tokenizers are provided:

-
-
-
-

### Language modeling

Language models provide ways to get a frequency/probability of a certain word or phrase.
