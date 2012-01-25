DOTFS
=====


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

