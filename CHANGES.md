**0.5.2**

- Feature: Windows build. It requires 64bit erlang version to be installed from http://www.erlang.org/download.html
- Change: Simplified and optimized shard rebalancing when adding new nodes.
- Change: Switched to MurmurHash3 for sharding instead of erlang:phash2. It has a better spread. This and the above change means 0.5.2 is backwards incompatible. We promise to make it the last such change.
- Bugfix: Java mysql driver compatibiliy.
- Bugfix: Using an underscore (_) in a key did not work for KV datatypes (you can use ascii characters, numbers, dots and underscore).
- Bugfix: Will no longer create folders in bkdcore/priv


**0.5.1**

- Feature: actordb:types/0, actordb:tables/1, actordb:columns/2 (useful calls when embedded).
- Feature: Return number of rows changed and last insert rowid in non select statements.
- Bugfix: Check schema is valid before saving (did not check in all cases before), check if kv table has right name and types.
- Bugfix: Significantly improved actor migration reliability during cluster rebalancing.
- Bugfix: For "actor type(*);..." queries in mysql console.


**0.5**

- Initial public release