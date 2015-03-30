**0.9pre5 - 30 Mar 2015**
- Bugfix: Raft replication bugfix


**0.9pre4 - 24 Mar 2015**

- Bugfix: New driver had improper Raft handling on conflicts
- Bugfix: Driver/sqlite settings were improper, resulting in slowdowns when creating new actors
- Bugfix: Cluster latency detection was not working
- Feature: Basic thrift interface


**0.9pre3 - 3 Mar 2015**

- Bugfix: Sometimes initialized database did not start completely when restarted.

**0.9pre2 - 27 Feb 2015**

- Bugfix: Sometimes driver crashed when closing program. This did not affect any data.

**0.9pre1 - 18 Feb 2015**

- Feature: We have rewritten the replication core of ActorDB. It now uses the Raft algorithm for replication. For global state and individual actor state.
- Feature: New sqlite driver (actordb_driver). It uses a combined wal file for all actors. This is a giant performance improvement.
- Bugfix: Use variable as actor name.
- Bugfix: Reads accross actors add the right actor name to actor column.
- Bugfix: Mysql protocol do not close connections for invalid ActorDB sql statements.


**0.5.2 - 26 Feb 2014**

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
