# CL-NLP — Lisp NLP toolset

Caution: this is vaporware! Come back in 2014, unless you want to contribute...

...but if you want to contribute you're very welcome.
Feel free to write to <vseloved@gmail.com>

## Getting started on OS X

* Install [sbcl](http://www.sbcl.org/) from [Homebrew](http://brew.sh/).
  Since 1.1.13 is [broken](http://sourceforge.net/mailarchive/message.php?msg_id=31604410)
  for some of the dependencies (like `cgn`), use version 1.1.12.
  If your brew copy is too new, do this:

```sh
cd /usr/local/Library/Formula
git checkout d30c2e984c46eac8598043eba8a2b93ec7f1d7d9 -- sbcl.rb # back out to pre-1.1.13
brew install sbcl
```

* Install [quicklisp](http://www.quicklisp.org/) which is a package manager:

```sh
wget http://beta.quicklisp.org/quicklisp.lisp
sbcl --load quicklisp.lisp --eval '(quicklisp-quickstart:install)' --eval '(ql:add-to-init-file)' --quit
```

* Install `cl-nlp` to `~/quicklisp/local-projects/`:

```sh
ln -s $(pwd) ~/quicklisp/local-projects/
```

* Install dependencies and load `cl-nlp`:

```sh
sbcl --eval '(ql:quickload "cl-nlp")'
```

You may `ACCEPT` all `SIMPLE-ERROR`s for now.

Happy hacking!
