# odpic-raw

[![Hackage](https://img.shields.io/hackage/v/protolude.svg)](https://hackage.haskell.org/package/odpic-raw)


Haskell raw bindings to [Oracle ODPI-C Library](https://github.com/oracle/odpi)


Requirements:

  * [libodpic 2.x](https://github.com/oracle/odpi/releases) (dpi.h should installed in /usr/local/include)
  * [Oracle Instant Client](http://www.oracle.com/technetwork/database/features/instant-client/index-097480.html)
  * GHC 8.0.1 or greater
  
Install:

```
stack install odpic-raw
```


Test:

edit HSpec.hs, set `username`, `password`, `constr` for test oracle db
```
stack test
```

Stack dependent :

this project is not in stackage yet, please add `extra-deps` in `stack.yaml` configuration.

```
extra-deps:
  - odpic-raw-0.1.2
```

More Documents:

 * [Hackage](https://hackage.haskell.org/package/odpic-raw)
 * [ODPI-C](https://oracle.github.io/odpi/doc/)
