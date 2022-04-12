# Emacs configuration using [straight.el](https://github.com/raxod502/straight.el)

## Setup for macOS with [Homebrew](http://brew.sh/)

### Install Emacs with homebrew

Install GUI version of emacs with

`brew install emacs-plus@28 --with-native-comp`

### Run Emacs as service

The `$ emacs --daemon` starts a daemon that can be connected to by using `$ emacsclient`.

Using [homebrew-servies](https://github.com/Homebrew/homebrew-services) that integrates with `launchctl` this can be a bit simplified.

Do `$ brew tap homebrew/services` and then Emacs daemon can be started with `$ brew services start emacs`

#### Some handy aliases

```sh
export EDITOR="emacsclient -a ''"
export GIT_EDITOR="emacsclient -a ''"
export SVN_EDITOR="emacsclient -a ''"
```
