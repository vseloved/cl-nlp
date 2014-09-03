# CL-NLP Style Guide

This is a document for contributors to `CL-NLP` that contains a few simple
principles that will allow to write code cohesive with the libraries style and design.

- In general, follow the [Google Common Lisp Style Guide](https://google-styleguide.googlecode.com/svn/trunk/lispguide.xml).
- `CL-NLP` uses heavily utilities and syntactic extensions from
  [RUTILS](http://github.com/vseloved/rutils).
  Using the "normal" CL things for the same stuff in new code is OK,
  although expect it to be eventually re-written in rutils' style.
  Using other utilities packages is not allowed.
- The library is built around a generic OO-approach where each algorithm
  is specialized with a certain class or classes of entities it is bound to.
  For instance a `tokenize` function will have methods for `sentence-splitter`
  and `word-tokenizer` classes. For more details on it see
  [Overview of CL-NLP architecture](http://lisp-univ-etc.blogspot.com/2013/02/natural-language-meta-processing-with.html).
- Tests for `CL-NLP` are written using [SHOULD-TEST](http://github.com/vseloved/should-test).
  The test module is only compiled when `:dev` is present in `*features*`
