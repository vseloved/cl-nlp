# CL-NLP -- a Lisp NLP toolkit

## Brief description

Eventually, CL-NLP will provide a comprehensive and extensible set of tools
to solve natural language processing problems in Common Lisp.

The goals of the project include the following:

- support for constructing arbitrary NLP pipelines on top of it
- support for easy and fast experimentation and development of new models and approaches
- serve as a good framework for teaching NLP concepts

It comprises of a number of utility/horizontal and end-user/vertical modules
that implement the basic functions and provide a way to add own extensions and models.

The utility layer includes:

- tools for transforming raw natural language text, as well as various corpora
  into a form suitable for further processing
- basic support for language modelling
- support for a number of linguistic concepts
- support for working with machine learning models and a number of training algorithms

The end-user layer will provide:

- POS taggers
- constituency parsers
- dependency parsers
- other stuff (will be added step-by-step, suggestions are welcome)


## How to start working with CL-NLP

The project has already reached a stage of usefulness for the primary author:
for instance, it supports my current language modelling experiments
by providing easy access to treebanks and other utilities.

Yet, it is far from being production-ready.
To reach v.0.1 that may be considered suitable for limited use by non-contributors,
the following things should be finished:

- implement a comprehensive test-suite and fix all bugs encountered in the process
- write an architecture overview
- describe available models and their quality metrics

If you want to play with CL-NLP or become a contributor, here are a few texts
I have already written in connection with it:

- [Writing a POS tagger with CL-NLP](doc/tagger-example.md)
- [NLTK series](http://lisp-univ-etc.blogspot.com/search/label/nltk) - to be continued, by the way...
- [CL-NLP Style Guide](doc/style-guide.md)


## Technical notes

### Current limitations:

- targeted at English language only

### Dependencies

- [RUTILS](http://github.com/vseloved/rutils)
- [CXML]()
- [DRAKMA]()
- [ZIP](http://common-lisp.net/project/zip/)
- [USERIAL](http://nklein.com/software/unet/userial/)

For development:

- [SHOULD-TEST](http://github.com/vseloved/should-test)

### License

The license of `CL-NLP` is Apache 2.0 license.

Specific models may have different license due to the limitations of the dataset
they are built with. Please see a `<model>.license` file accompanying each model for details.

(c) 2013-2014, Vsevolod Dyomkin <vseloved@gmail.com>
