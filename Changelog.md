# Changelog

Recent changes include:
- replacing `malloc` with `calloc` to workaround a deficiency in the `Storable` instance for `Data` (where `isNull` was not appropriately being set to `0`)
- moving calls to `malloc`/`calloc` to sites where they could be accompanied by appropriate calls to `free` to avoid space leaks
- amending `queryByPage` to return records in the appropriate order
- amending `Data`'s `Storable` instance to use `ODPI-C`'s `lib*` helper functions (that inter alia correctly set `isNull` to `0` when appropriate)
- adding a 'withConnectionFromPool' function that calls `closeConnection` rather than `releaseConnection` after performing the desired db action
