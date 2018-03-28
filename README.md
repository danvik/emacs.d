# Emacs configuration using [Cask](https://github.com/cask/cask) and [Pallet](https://github.com/rdallasgray/pallet)

## Setup for OS X with [Homebrew](http://brew.sh/)

### Install Emacs

Get the latest `$ brew install emacs --with-cocoa --with-gnutls --HEAD`

### Install [Cask](https://github.com/cask/cask)

`$ brew install cask`

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
Search in buffers with `C-s` using [Swiper](https://github.com/abo-abo/swiper).

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

Edit grep buffers and apply changes to files using [wgrep](https://github.com/mhayashi1120/Emacs-wgrep).

### Programming

Source code completion is done with [company](http://company-mode.github.io/), use `M-2` for candidates.

Comment out code with `M-;` using [comment-dwim-2](https://github.com/remyferre/comment-dwim-2).

Syntax check with [flycheck](http://www.flycheck.org/en/latest/) `C-c u f` to turn it on.

[lispy](https://github.com/abo-abo/lispy) provides Vi-like lisp editing.

#### Projects

Powerful package management with [Projectile](https://github.com/bbatsov/projectile). Prefix `C-c p`. Also `ivy-switch-project` is bound to `C-c u p` that provides more options with `M-o`.

[magit](https://magit.vc/) for interfacing with git.

Apply project specific rules for code with [editorconfig](https://github.com/editorconfig/editorconfig-emacs).

#### Ruby

Besides the built in `ruby-mode` some other packages makes editing Ruby code easier.

[bundler](https://github.com/tobiassvn/bundler.el) interacts with Bundler for Ruby. Use `bundle-open` to open a dired buffer for specific gem.

[yard-mode](https://github.com/pd/yard-mode.el) syntax highlighting and editing of [YARD](http://yardoc.org/) documentation.

[inf-ruby](http://github.com/nonsequitur/inf-ruby) Ruby repl.

Use the correct Ruby version for a buffer by calling `rvm-activate-corresponding-ruby` provided by  [rvm](https://github.com/senny/rvm.el).

[rubocop](https://github.com/bbatsov/rubocop-emacs) for style checking.

#### Go

[go-mode](https://github.com/dominikh/go-mode.el) as major mode.

Above requires Go packages to be installed

Install [gocode](https://github.com/nsf/gocode)

`go get -u github.com/nsf/gocode`

Install [godef](github.com/rogpeppe/godef)
`go get -u github.com/rogpeppe/godef`

Install [goimports](https://godoc.org/golang.org/x/tools/cmd/goimports)
`go get golang.org/x/tools/cmd/goimports`

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

[smart-mode-line](http://github.com/Malabarba/smart-mode-line) makes the modeline pretty. No nead for setting the format for `projectile-mode-line`, `smart-mode-line` supports [Projectile](https://github.com/bbatsov/projectile).

### Windows / buffers / navigation

Jump quickly to lines, characters, words with [avy](https://github.com/abo-abo/avy).

Better window switching with [ace-window](https://github.com/abo-abo/ace-window).

Manage popup windows and show them in a smaller window than normal with [popwin-el](https://github.com/m2ym/popwin-el).
Make them go away with `C-g` and re-open them in their normal view with `C-c w C-o`.

Window config handling using [eyebrowse](https://github.com/wasamasa/eyebrowse).

Rotate window layout using [rotate](https://github.com/daichirata/emacs-rotate).

Distraction free writing with [writeroom-mode](https://github.com/joostkremers/writeroom-mode) bound to `C-c u w`.

### Utils

Kill ring history with [browse-kill-ring](https://github.com/browse-kill-ring/browse-kill-ring) bound to `M-y`.

Copy environment variables to Emacs with [exec-path-from-shell](https://github.com/purcell/exec-path-from-shell).

Easy note taking using [deft](https://github.com/jrblevin/deft).

Help on key bindings with [which-key](https://github.com/justbur/emacs-which-key).

Stats on commands used [keyfreq](https://github.com/dacap/keyfreq).

With [hydra](https://github.com/abo-abo/hydra) it is easy to bind shortcuts to just one prefix key.

[undo-tree](http://www.dr-qubit.org/tags/computing-code-emacs.html) makes it easy to undo and redo.

Sidebar tree view with [neotree](https://github.com/jaypei/emacs-neotree).

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
