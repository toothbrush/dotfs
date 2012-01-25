DOTFS
=====

DotFS is first and foremost a work in progress. The idea is to eventually be able to point DotFS to a folder containing
your dotfiles containing special DotFS syntax, which will then be made available as a virtual filesystem at a user-specified
mountpoint. The files will be scanned for special keywords and presented on the virtual filesystem, possibly configured
per-host or per-OS. The use case is a user wanting automatic configuration adjustment in larger config files with shared sections.


Requirements
------------
* POSIX system
* [FUSE](http://fuse.sourceforge.net/)
* [GHC](http://hackage.haskell.org/platform/) (Haskell platform recommended)

Installation
------------
Install some dependencies:


```
sudo apt-get install libfuse-dev fuse-utils
```

Install DotFS:


```
cabal configure
cabal build
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

