# cl-nlp

CL-NLP is a Modern NLP toolkit built using Common Lisp.

CL-NLP aims to provide a comprehensive and extensible set of tools to solve
natural language processing problems in Common Lisp.

The goals of the project include the following:

- support for constructing arbitrary NLP pipelines on top of it
- support for easy and fast experimentation and development of new models and
  approaches
- serve as a good framework for teaching NLP concepts

It comprises of a number of utility/horizontal and end-user/vertical modules
that implement the basic functions and provide a way to add own extensions and
models.

The utility layer includes:

- tools for transforming raw natural language text, as well as various corpora
  into a form suitable for further processing
- basic support for language modelling
- support for a number of linguistic concepts
- support for working with machine learning models and a number of training
  algorithms

The end-user layer will provide:

- POS taggers
- constituency parsers
- dependency parsers
- other stuff (will be added step-by-step, suggestions are welcome)
