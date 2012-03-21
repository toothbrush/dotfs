DOTFS
=====

DotFS is first and foremost a work in progress. The idea is that you
point DotFS to a folder containing your dotfiles containing special
DotFS annotations, which will then be made available as a virtual filesystem at a
user-specified mountpoint. The files will be scanned for special keywords and
presented on the virtual filesystem, possibly configured per-host or per-OS, however you like.
The use case is a user wanting automatic configuration adjustment in larger
config files with shared sections. See the wiki for a quick guide, a users' guide,
a specification of the grammar, and some examples.


Requirements
------------
* POSIX system
* [FUSE](http://fuse.sourceforge.net/)
* [GHC](http://hackage.haskell.org/platform/) (Haskell platform recommended)
* Tested on Linux, hfuse needs to be un-broken on OS X

Installation
------------
First install some dependencies:


```
sudo apt-get install libfuse-dev fuse-utils
```

Install DotFS, or run from the local directory using `make`.

```
cabal configure
cabal install
```

Usage
-----
There are currently very few options:

```
dotfs [OPTIONS] MOUNTPOINT CONFDIR
```

where MOUNTPOINT and CONFDIR are paths.

OPTIONS:

*  -h
*  -?    print help
*  -V    print version

