# Emacs configuration using [Cask](https://github.com/cask/cask) and [Pallet](https://github.com/rdallasgray/pallet)

## Setup for OS X with [Homebrew](http://brew.sh/)

### Install emacs

`$ brew install emacs --HEAD --use-git-head --cocoa --srgb`

### Install [Cask](https://github.com/cask/cask)

`$ brew install cask`

### Initialize

```sh
$ git clone https://github.com/danvik/emacs.d.git ~/.emacs.d
$ cd ~/.emacs.d
$ cask
```

## Things used in this config

- [aspell](http://aspell.net/) `$ brew install aspell`
- [editorconfig](http://editorconfig.org/)
- [ag, the silver searcher](https://github.com/ggreer/the_silver_searcher) `$ brew install silver_searcher`
- [Inconsolata font](http://www.levien.com/type/myfonts/inconsolata.html)
- [rubocop](https://github.com/bbatsov/rubocop) Ruby style checker
- [markdown](https://daringfireball.net/projects/markdown/) `$ brew install markdown`

## Handy stuff

- [markdown-preview](https://www.npmjs.com/package/markdown-preview) `$ npm install --global github-markdown-preview`

## Some handy aliases

```sh
export EDITOR="emacsclient -a ''"
export GIT_EDITOR="emacsclient -a ''"
export SVN_EDITOR="emacsclient -a ''"
alias ec="emacsclient -a '' -c -n"
alias e="emacsclient -a '' -n"
alias et="emacsclient -a '' -t"
```

## Running as daemon

`emacs --daemon`
