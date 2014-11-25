# Getting Started

The steps to install and run cl-nlp with quicklisp. You will also find
information about the test, docs infrastructure with guidelines to contribute.

---

## Recommended Reading

If you are new to NLP, here are a few documents to help you get started.

- [NLP](http://en.wikipedia.org/wiki/Natural_language_processing)
- [Writing a POS tagger with CL-NLP](../user-guide/samples.md)
- [NLTK series](http://lisp-univ-etc.blogspot.com/search/label/nltk)

## Dependencies

As of now cl-nlp has been tested only on SBCL. 

- [RUTILS](https://github.com/vseloved/rutils.git)
- [Closure XML](http://common-lisp.net/project/cxml/)
- [DRAKMA](http://weitz.de/drakma/)
- [ZIP](http://common-lisp.net/project/zip/)
- [USERIAL](http://nklein.com/software/unet/userial/)

## Test Dependencies

- [SHOULD-TEST](http://github.com/vseloved/should-test)

## Installation

Currently cl-nlp is not available from the quicklisp repository. You need to
clone it your quicklisp local projects directory. You will also need to clone
rutils and should-test to local projects.

```
cd path/to/quicklisp/local-projects

git clone git@github.com:vseloved/cl-nlp.git 

git clone git@github.com:vseloved/rutils.git 

git clone git@github.com:vseloved/should-test.git 
```

## Test System

The tests are guarded by a ```#+dev``` feature. As such you need to push this
symbol to ```*features*``` in order to run the tests. The tests can be run with
a single command using the asdf test system, as is shown in the next section.

If you do not wish to run tests, you can skip this step. We hope a developer
who wants to contribute does not do this!

### Travis CI

cl-nlp is also enabled by [Travis CI](https://travis-ci.org/vseloved/cl-nlp)
which runs the test suite using SBCL. 

## Using Quicklisp with Test System

Now you can load cl-nlp and run the tests. 

```
* (pushnew :dev *features*) ;;; use only if you want to run tests

* (ql:quickload :cl-nlp)

* (asdf:test-system :cl-nlp)

```

## Project Layout

1. src - contains source code divided into modules by NLP tasks
2. test - contains test code guarded by ```#+dev```
3. docs - contains documentation using MKDocs

## Contribution Guidelines

You are welcome to submit bug fixes and new features. The following is the
process you need to follow:

### Process Guidelines

---

- Create your own fork.
- Create a bug fix or feature branch.
- Make your code changes on that branch.
- Add tests for your bug fix/feature.
- Run the test suite locally.
- Add and test documentation as described [next](getting-started.md#Documentation).
- Submit PR.
- Hopefully Travis will be happy.
- Incorporate any review changes and rinse, lather, repeat till integrated :-).

### Coding Style Guidelines

---

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

## Documentation

The project is documented using [MkDocs](http://www.mkdocs.org) and hosted on
[readthedocs.org](http://cl-nlp.readthedocs.org). 

If you wish to contribute to the documentation, the following steps are
recommended

- Install MkDocs as described [here](http://www.mkdocs.org/#installation).
- Create a new branch and make the changes.
- Test locally as described [here](http://www.mkdocs.org/#getting-started)
  using the serve command.
- Verify and submit a Github PR.
