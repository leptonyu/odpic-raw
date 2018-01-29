# odpic-raw

[![Hackage](https://img.shields.io/badge/hackage-v0.1.7-orange.svg)](https://hackage.haskell.org/package/odpic-raw)


Haskell raw bindings to [Oracle ODPI-C Library](https://github.com/oracle/odpi)


Requirements:

  * [libodpic 2.x](https://github.com/oracle/odpi/releases) (dpi.h should installed in /usr/local/include)
  * [Oracle Instant Client](http://www.oracle.com/technetwork/database/features/instant-client/index-097480.html)
  * GHC 8.0.1 or greater
  
Install:

```
stack install odpic-raw
```

Use environment [`DPI_DEBUG_LEVEL`](https://oracle.github.io/odpi/doc/user_guide/debugging.html) to set DPI debug level, for`createStatement` cannot show detail reason for what is wrong.

For non-english user, please set environment  `NLS_LANG` to specify the oracle db encodings. use following sql to get the value.
```SQL
SELECT USERENV ('language') FROM DUAL
```

Test:

edit [Spec.hs](https://github.com/leptonyu/odpic-raw/blob/master/test/Spec.hs), set `username`, `password`, `constr` for test oracle db
```
stack test
```

Stack dependent :

this project is not in stackage yet, please add `extra-deps` in `stack.yaml` configuration.

```
extra-deps:
  - odpic-raw-0.1.7
```

More Documents:

 * [Hackage](https://hackage.haskell.org/package/odpic-raw)
 * [ODPI-C](https://oracle.github.io/odpi/doc/)
