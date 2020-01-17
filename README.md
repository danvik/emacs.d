# Emacs configuration using [straight.el](https://github.com/raxod502/straight.el)

## Setup for macOS with [Homebrew](http://brew.sh/)

### Install Emacs with homebrew

Install GUI version of emacs with

`brew cask install emacs`

### Initialize

```sh
$ git clone https://github.com/danvik/emacs.d.git ~/.emacs.d
$ cd ~/.emacs.d
$ cask
```

### Run Emacs as service

The `$ emacs --daemon` starts a daemon that can be connected to by using `$ emacsclient`.

Using [homebrew-servies](https://github.com/Homebrew/homebrew-services) that integrates with `launchctl` this can be a bit simplified.

Do `$ brew tap homebrew/services` and then Emacs daemon can be started with `$ brew services start emacs`

Some handy aliases for this

```sh
alias ec="emacsclient -a '' -c -n"
alias e="emacsclient -a '' -n"
alias et="emacsclient -a '' -t"
```

## Description of some of the packages used

### General

[Ivy](https://github.com/abo-abo/swiper) is used for input completion.
Search in buffers with `C-s` using [Swiper](https://github.com/abo-abo/swiper) and `swiper-isearch`.

[counsel](https://github.com/abo-abo/swiper) uses [Ivy](https://github.com/abo-abo/swiper) and provides commands like `counsel-git-grep` for fast grep in a git repository and `counsel-M-x` that replaces emacs `execute-extended-command`.

[Org mode](http://orgmode.org/) is enhanced with vi-like bindings with [worf](https://github.com/abo-abo/worf) and prettified with [org-bullets](https://github.com/sabof/org-bullets).

Package [ox-twbs](https://github.com/marsmining/ox-twbs) exports org-mode files as HTML compatible with Twitter Bootstrap.

### Text editing

Move text around with [drag-stuff](https://github.com/rejeep/drag-stuff.el). Use `M-<arrows>` on line or region.

Mark region with [expand-region](https://github.com/magnars/expand-region.el) using `C-=`.

Kill text using [easy-kill](https://github.com/leoliu/easy-kill) with `M-w` followed by additional keys.

Select and edit symbol or region using [iedit](https://github.com/victorhge/iedit). Bound to `C-;`.

Multiple cursors using [multiple-cursors](https://github.com/magnars/multiple-cursors.el).

Text pairing with [smartparens](https://github.com/Fuco1/smartparens).

### Programming

Source code completion is done with [company](http://company-mode.github.io/), use `M-2` for candidates.

Comment out code with `M-;` using [comment-dwim-2](https://github.com/remyferre/comment-dwim-2).

Syntax check with [flycheck](http://www.flycheck.org/en/latest/) `C-c u f` to turn it on.

[lispy](https://github.com/abo-abo/lispy) provides Vi-like lisp editing.

#### Projects

Package management with [Projectile](https://github.com/bbatsov/projectile) and extended using [counsel-projectile](https://github.com/ericdanan/counsel-projectile). Prefix `C-c p`.

[magit](https://magit.vc/) for interfacing with git. Use `C-c v` for `magit-status`. `C-c M-g` will open `magit-file-pop`.

Apply project specific rules for code with [editorconfig](https://github.com/editorconfig/editorconfig-emacs).

#### Ruby

Besides the built in `ruby-mode` some other packages makes editing Ruby code easier.

[yard-mode](https://github.com/pd/yard-mode.el) syntax highlighting and editing of [YARD](http://yardoc.org/) documentation.

[inf-ruby](http://github.com/nonsequitur/inf-ruby) Ruby repl.

Use the correct Ruby version for a buffer by calling `rvm-activate-corresponding-ruby` provided by  [rvm](https://github.com/senny/rvm.el).

#### Go

[go-mode](https://github.com/dominikh/go-mode.el) as major mode.

Above requires Go packages to be installed

Install [gocode](https://github.com/nsf/gocode)

`go get -u github.com/nsf/gocode`

Install [godef](github.com/rogpeppe/godef)
`go get -u github.com/rogpeppe/godef`

Install [goimports](https://godoc.org/golang.org/x/tools/cmd/goimports)
`go get golang.org/x/tools/cmd/goimports`

go get -v github.com/ramya-rao-a/go-outline
go get -v github.com/uudashr/gopkgs/cmd/gopkgs
go get -v github.com/sqs/goreturns
go get golang.org/x/tools/cmd/gorename

Extend [company](http://company-mode.github.io/) with [company-go](https://github.com/nsf/gocode/blob/master/emacs-company/company-go.el) for completion.

Help on functions with [go-eldoc](https://github.com/syohex/emacs-go-eldoc).

Setup `GOPATH` with [go-gopath](http://github.com/iced/go-gopath/).

##### Install Go and setup GOPATH

Install go

```sh
brew install go
```

Setup working directory and `GOPATH`

```sh
mkdir ~/golang
```

```sh
echo "export GOPATH=\$HOME/golang" >> ~/.bash_profile
echo "export PATH=\$PATH:\$GOPATH/bin" >> ~/.bash_profile
```

#### Elixir

Major mode [elixir-mode](https://github.com/elixir-lang/emacs-elixir) and extended tools with [alchemist](http://www.github.com/tonini/alchemist.el).

### Appearance

Make comments like `TODO`, `NOTE` and others stand out with [hl-todo](http://github.com/tarsius/hl-todo).

Make numbers standout with [highlight-numbers](https://github.com/Fanael/highlight-numbers).

### Windows / buffers / navigation

Jump quickly to lines, characters, words with [avy](https://github.com/abo-abo/avy).

Better window switching with [ace-window](https://github.com/abo-abo/ace-window).

Window config handling using [eyebrowse](https://github.com/wasamasa/eyebrowse).

Rotate window layout using [rotate](https://github.com/daichirata/emacs-rotate).

### Utils

Kill ring history with [browse-kill-ring](https://github.com/browse-kill-ring/browse-kill-ring) bound to `M-y`.

Copy environment variables to Emacs with [exec-path-from-shell](https://github.com/purcell/exec-path-from-shell).

Easy note taking using [deft](https://github.com/jrblevin/deft).

Help on key bindings with [which-key](https://github.com/justbur/emacs-which-key).

With [hydra](https://github.com/abo-abo/hydra) it is easy to bind shortcuts to just one prefix key.

[undo-tree](http://www.dr-qubit.org/tags/computing-code-emacs.html) makes it easy to undo and redo.

### Built in's

Use `ibuffer` to list buffers and [ibuffer-vc](http://github.com/purcell/ibuffer-vc) to group buffers by project.

### Other packages used

`rainbow-mode`, `clojure-mode`, `css-mode`, `erlang`, `feature-mode`, `json-mode`, `markdown-mode`, `yaml-mode`, `orglink`

## Non-Emacs things used in this config

- [aspell](http://aspell.net/) `$ brew install aspell`
- [Inconsolata font](http://www.levien.com/type/myfonts/inconsolata.html)
- [rubocop](https://github.com/bbatsov/rubocop) Ruby style checker
- [markdown](https://daringfireball.net/projects/markdown/) `$ brew install markdown`
- [gnupg](https://www.gnupg.org) `brew install gnupg`

## Handy stuff

- [markdown-preview](https://www.npmjs.com/package/markdown-preview) `$ npm install --global github-markdown-preview`

## Some handy aliases

```sh
export EDITOR="emacsclient -a ''"
export GIT_EDITOR="emacsclient -a ''"
export SVN_EDITOR="emacsclient -a ''"
```
