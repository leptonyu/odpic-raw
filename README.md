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

# Changes on this fork

Changes on this forked version of `odpic-raw` include:
- replacing `malloc` with `calloc` to workaround a deficiency in the `Storable` instance for `Data` (where `isNull` was not appropriately being set to `0`): commit [here](https://github.com/liminalisht/odpic-raw/commit/32e8e739292f303aac127e64681b3d0e01cb45c3)
- moving calls to `malloc`/`calloc` to sites where they could be accompanied by appropriate calls to `free` to avoid space leaks: PR [here](https://github.com/liminalisht/odpic-raw/pull/1)
- amending `queryByPage` to return records in the appropriate order: PR [here](https://github.com/liminalisht/odpic-raw/pull/2)
- amending `Data`'s `Storable` instance to use `ODPI-C`'s `lib*` helper functions (that inter alia correctly set `isNull` to `0` when appropriate): PR [here](https://github.com/liminalisht/odpic-raw/pull/3)
- adding a 'withConnectionFromPool' function that calls `closeConnection` rather than `releaseConnection` after performing the desired db action: PR [here](https://github.com/liminalisht/odpic-raw/pull/4)

Several of the changes made on our fork have been submitted as PRs upstream:
- [bug: replace malloc with calloc and fix potential space leaks](https://github.com/leptonyu/odpic-raw/pull/20)
- [bug: fix bug where queryByPage would return rows in the opposite of the intended order](https://github.com/leptonyu/odpic-raw/pull/19)
