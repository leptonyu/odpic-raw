# odpic-raw

[![Hackage](https://img.shields.io/badge/hackage/v/odpic-raw.svg)](https://hackage.haskell.org/package/odpic-raw)

Haskell raw bindings to [Oracle ODPI-C Library](https://github.com/oracle/odpi).

Requirements:

  * It contains the ODPI-C source, so you don't need to install it manually.
  * [Oracle Instant Client Basic or Basic Light package](http://www.oracle.com/technetwork/database/features/instant-client/index-097480.html)
  * GHC 8.2.2 or greater


Installation:

```
stack build odpic-raw
```

Use environment [`DPI_DEBUG_LEVEL`](https://oracle.github.io/odpi/doc/user_guide/debugging.html) to set DPI debug level.

For non-english user, please set environment  `NLS_LANG` to specify the oracle db encodings. use following sql to get the value.
```SQL
SELECT USERENV ('language') FROM DUAL
```
Or use `setupLanguage` to set  `NLS_LANG` automatically.

Test:

```
export DB_USER=username
export DB_PASS=password
export DB_URL=localhost/dbname
stack test
```

Stack dependencies :

this project is not in stackage yet, please add `extra-deps` in `stack.yaml` configuration.

```
# for lts-11.x, lts-12.x, lts-13.x
extra-deps:
  - odpic-raw-0.4.0
# for lts-10.x
extra-deps:
  - odpic-raw-0.1.11
```

More Documents:

 * [Hackage](https://hackage.haskell.org/package/odpic-raw)
 * [ODPI-C](https://oracle.github.io/odpi/doc/)
