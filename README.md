DOTFS
=====

DotFS is first and foremost a work in progress. It is, however, usable, and the authors use it on
their personal machines. The idea is that you
point DotFS to a folder containing your dotfiles containing special
DotFS annotations, which will then be made available as a virtual filesystem at a
user-specified mountpoint. The files will be scanned for special keywords and
presented on the virtual filesystem, possibly configured per-host or per-OS, however you like.
The use case is a user wanting automatic configuration adjustment in larger
config files with shared sections. See the wiki for a quick guide, a users' guide,
a specification of the grammar, and some examples.

Note that if a file doesn't contain DotFS markup, it is passed through "vanilla" -- even binaries. This means that
you don't have to convert all your config files before starting with DotFS, but that you can annotate those which 
currently pose an inconvenience, and leave all the others as they are.


Requirements
------------
* POSIX system
* [FUSE](http://fuse.sourceforge.net/) (or OSXFUSE)
* [GHC](http://hackage.haskell.org/platform/) (Haskell platform recommended)
* Tested on Linux, hfuse needs to be un-broken on OS X

Installation
------------
First install some dependencies (example is for Debian-like systems):

NOTE to OS X users: the HFuse library, on which we depend, doesn't build using the latest OSXFUSE library. Please use the [patched version](https://github.com/toothbrush/hfuse/tree/osxfuse-update) fork for now, until the pull request is hopefully accepted.

```
sudo apt-get install libfuse-dev fuse-utils
```

Install DotFS, or run from the local directory using `make` and then `./dotfs`.

```
cabal configure
cabal install
```

Usage
-----
See the [wiki entry on usage](https://github.com/toothbrush/dotfs/wiki/DotFS-Usage). Soon a short presentation will
also be put online which will hopefully serve as both an elevator pitch and a short introduction to the features that
are available.
