# odpic-raw

[![Hackage](https://img.shields.io/hackage/v/odpic-raw.svg)](https://hackage.haskell.org/package/odpic-raw)
[![Build Status](https://travis-ci.org/leptonyu/odpic-raw.svg?branch=master)](https://travis-ci.org/leptonyu/odpic-raw)

Haskell raw bindings to [Oracle ODPI-C Library](https://github.com/oracle/odpi).

Requirements:

  * odpic-raw 5.* contains the ODPI-C source, so you don't need to install it manually.
  * [Oracle Instant Client Basic or Basic Light package](http://www.oracle.com/technetwork/database/features/instant-client/index-097480.html)
  * GHC 8.* or greater

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

More Documents:

 * [Hackage](https://hackage.haskell.org/package/odpic-raw)
 * [ODPI-C](https://oracle.github.io/odpi/doc/)
