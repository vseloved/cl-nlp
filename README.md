[![Build Status](https://travis-ci.org/vseloved/cl-nlp.png?branch=master)](https://travis-ci.org/vseloved/cl-nlp)
[![Documentation Status](https://readthedocs.org/projects/cl-nlp/badge/?version=latest)](https://readthedocs.org/projects/cl-nlp/?badge=latest)

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

Yet, it is far from being production-ready. So, if you want to use it for production tasks,
expect to bleed on the bleeding edge.

Otherwise, if you want to contribute to developing the toolkit, you're very welcome.
Here are a few write-ups to give you the sense of the project and to help get started:

- [Writing a POS tagger with CL-NLP](docs/user-guide/examples/eng-pos-tagger.md)
- [NLTK series](http://lisp-univ-etc.blogspot.com/search/label/nltk) -
  to be continued, by the way...
- [CL-NLP Style Guide](docs/user-guide/getting-started.md#coding-style-guidelines)

You'll also, probably, need to track the latest version of [RUTILS][RUTILS] from git.

For CL-NLP to reach v.0.1 that may be considered suitable for limited use by non-contributors,
the following things should be finished (work-in-progress):

- implement a comprehensive test-suite and fix all bugs encountered in the process
- describe available models and their quality metrics


## Technical notes

### Dependencies

- [RUTILS][RUTILS]
- [Closure XML](http://common-lisp.net/project/cxml/)
- [DRAKMA](https://github.com/edicl/drakma)
- [ZIP](http://common-lisp.net/project/zip/)
- [USERIAL](http://nklein.com/software/unet/userial/)

For development:

- [SHOULD-TEST](http://github.com/vseloved/should-test)


## License

The license of CL-NLP is Apache 2.0.

Specific models may have different license due to the limitations of the dataset
they are built with. Please see a `<model>.license` file accompanying each model for details.

(c) 2013-2014, Vsevolod Dyomkin <vseloved@gmail.com>


   [RUTILS]:http://github.com/vseloved/rutils
