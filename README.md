# Emacs configuration using [Cask](https://github.com/cask/cask) and [Pallet](https://github.com/rdallasgray/pallet)

## Setup with [Homebrew](http://brew.sh/)

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

## Other handy stuff

- aspell
- editorconfig
- ag
- Inconsolata font
- https://www.npmjs.com/package/markdown-preview
- markdown

## List some handy aliases here

```sh
export EDITOR="emacsclient -a ''"
export GIT_EDITOR="emacsclient -a ''"
export SVN_EDITOR="emacsclient -a ''"
alias ec="emacsclient -a '' -c -n"
alias e="emacsclient -a '' -n"
alias et="emacsclient -a '' -t"
```

`emacs --daemon`


