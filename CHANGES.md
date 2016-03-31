**0.10.20 - 31 mar 2016**
- Bugfix: Running an empty write that increases evnum but does not actually execute one could result in checkpoints not completing because of the gap in the eventlog.
- Bugfix: Errors for update/insert statements did not clean up the transaction properly.
- Feature: actordb_tool print now has an optional actorindex parameter.


**0.10.19 - 27 Mar 2016**
- Bugfix: In rare cases an endless loop could develop while waiting for a write to complete on an actor.
- Feature: Select only certain actors by name: actor mytype(* when id like 'actorname%')
- Feature: safe flag on reads. Requires a read to be confirmed by a majority of nodes.


**0.10.18 - 23 Mar 2016**
- Bugfix: Better fix for yesterdays bug.
- Bugfix: Self-heal mechanism when an actor is left in an intermediate state due to abnormal shutdown. 


**0.10.17 - 22 Mar 2016**
- Bugfix: Bug in new query interrupt code for 0.10.16 caused queries to get terminated too early.


**0.10.16 - 22 Mar 2016**
- Bugfix: Thrift API update. Some non SQL query calls were not checking user.
- Bugfix: Increase sqlite per actor cache_size. It seems having it too low makes some queries fail.
- Bugfix: Election RPCs are now limited to 200ms.
- Bugfix: query_timeout only worked for read queries. Default timeout has been lowered to 10s.
- Feature: show queries to see currently running queries.
- Feature: Use a lock free task queue for the storage engine.
- Feature: RPC optimizations by using batching.
- Feature: Use C based atomic integers to count queries for backpressure mechanism. Important because we need constant global reads and writes, which is hard to achieve efficiently in Erlang.


**0.10.15 - 29 Feb 2016**
- Bugfix: Driver work queue thread safety issue.
- Bugfix: Windows backup fix. Windows backups are now always compact.
- Bugfix: Executing a backup could produce a (harmless) error once backup has already been completed.
- Bugfix: Bugfix for :memory: sqlite instances. On close sqlite may call undo in wal.c.
- Bugfix: Bugfixes in catchup code. When a node comes back online, so it knows what actors to update.
- Bugfix: Build of actordb_console required erlang-wx to be installed, even though it was only used on windows.
- Bugfix: Replication bug caused unnecessary node chatter until next write.
- Bugfix: When calling non-existing actor without create flag, we might create a slow moving endless loop.
- Feature: Update sqlite to 3.11.0
- Feature: Compact backup which produces a smaller file, but may take longer and use more CPU time.


**0.10.14 - 9 Feb 2016**
- Bugfix: PRAGMA copy response. Copy succeeded, but error was returned.
- Bugfix: When initializing with localnode() rpcport was ignored so only default worked.
- Feature: Remove some LMDB workarounds that are no longer necessary in 0.9.17. Performance and reliability is improved as a result.
- Feature: Added client_inactivity_timeout setting to app.config.
- Feature: Update sqlite to 3.10.2.


**0.10.13 - 16 Dec 2015**
- Bugfix: Parameterized queries over thrift with create flag were failing. Bug was created in 0.10.12.
- Bugfix: actordb_client can now be started without parameters and pool connections added later.
- Feature: Upgrade LMDB to 0.9.17


**0.10.12 - 27 Nov 2015**
- Bugfix: Mysql protocol bugfix for passing strings using parameterized queries.
- Bugfix: Import from backup. It now imports users, schema and idmax. Only nodes/groups need to be recreated.
- Bugfix: Bugfix with {{uniqid}} tags in sql. Previously they only worked in multi-actor transactions.
- Change: Server will now always return integers in bigint from thrift. This simplifies clients.


**0.10.11 - 26 Nov 2015**
- Bugfix: Thrift bugfix for blobs, data may have been returned in text instead of blob.
- Bugfix: Thrift bugfix for returning floats, bug was created in 0.10.8.
- Bugfix: PRAGMA exists failed with parameterized queries.


**0.10.10 - 24 Nov 2015**
- Feature: "show status" command to display status of ActorDB node
- Feature: "show shards" command to display shards running on an ActorDB node
- Bugfix: Go mysql driver compatibility bugfix.
- Bugfix: Bugfix for boolean bind parameters.
- Bugfix: On initialize through actordb_console do not print help headers.
- Bugfix: On import from backup, carry over idmax.
- Bugfix: Race condition on saving global state.


**0.10.9 - 21 Nov 2015**
- Bugfix: If actor was inactive and schema was updated during, it might not update it on next start.


**0.10.8 - 20 Nov 2015**
- Bugfix: Schema was updated right after a read to an actor instead of before.
- Bugfix: When actor moves to a new shard, replace original actor database with redirect marker. This way we do not waste space.
- Feature: Added blob to thrift value type. Somehow we managed to forget about it.


**0.10.7 - 10 Nov 2015**
- Bugfix: Cleaner handling of nocreate error (trying to execute on an actor that does not exist without create flag).
- Bugfix: Updating schema for an actor failed. Bug in schema check before updating.
- Bugfix: For certain types of rare sql execute errors, sqlite statement may not have been cleaned up.


**0.10.6 - 2 Nov 2015**
- Bugfix: Initialization with actordb_console -f "path/to/file.sql" fixed. Bug was created in 0.10.5.


**0.10.5 - 28 Oct 2015**
- Feature: Add state table to config. This is to store various config items like schema version.
- Feature: actordb_console flag: -q "select ...." to run single queries and exit.
- Feature: actordb_console flag: -print < default|min|csv|csvh > to actordb_console.
- Feature: actordb_console flag: -pw to set password directly (instead of being prompted).
- Feature: actordb_console flag: -use < actordb|config|schema > which db to use by default.
- Feature: actordb_console flag: -noshell for use with -q, recommended but not required.
- Feature: new thrift calls: uniqid, actor_types, actor_tables, actor_columns
- Bugfix: If {{RESULT}} was present in a multiactor write, query was wrongly interpreted as a read and failed when executing. 
- Bugfix: Invalid return result on pragma delete statements. Causing thrift to return error when in fact everything was ok.


**0.10.4 - 5 Oct 2015**
- Bugfix: Batching writes to an actor is much improved.
- Bugfix: sql parsing bug on insanely written sEleCT statements.
- Bugfix: Thrift interface returned error when doing multiple inserts using a single parameterized query.
- Bugfix: On SQL error thrift interface was not returning the sql error code.


**0.10.3 - 24 Sept 2015**
- Bugfix: When deleting actor, some data was left behind.
- Bugfix: Backup fix for lmdb. 
- Bugfix: Some vm.args changes that result in better erlang efficiency.
- Bugfix: actordb_console ignore invalid flags. 


**0.10.2 - 16 Sept 2015**
- Bugfix: Actor statement parsing could read wrong actor type.
- Bugfix: PRAGMA list on kv types did not work.
- Bugfix: Integer overflow in driver. 
- Bugfix: Some write results could be ignored if write delays happen.


**0.10.1 - 11 Sept 2015**
- Bugfix: If api_network_interface was not set, thrift was not listening on all interfaces.
- Bugfix: Cleaned up error results in console.
- Bugfix: Improper error handling of certain kind of invalid read queries.


**0.10 - 9 Sept 2015**
- Feature: ActorDB now uses LMDB as a storage engine. SQLite is still the SQL engine. Performace is vastly improved. Old versions are automatically imported. Old files will be left intact. A new file named lmdb will appear in every storage folder and it will contain all data.
- Feature: Seperate Read/Write threads for improved concurrency.
- Feature: Write batching. If an actor receives many writes at once, they will be a part of a single replication event. 
- Feature: Added LIMIT and OFFSET to pragma list: "actor type1(*);PRAGMA list LIMIT 1000 OFFSET 10;"
- Feature: actordb_console to manage the database. It replaces actordbctrl.
- Feature: actordb_tool for backups and diagnostics.
- Feature: Expanded thrift interface. ActorDB can now be completely controled through thrift.
- Feature: Finally added user management. Thrift interface supports safer mysql style login that does not send the actual password over the wire.
- Feature: Expanded configuration options in app.config. 
- Feature: Added fsync flag to queries. Writes with fsync will be synced to disk before response is sent to client. You can set fsync to safe in app.config, which will have the same result for all writes.
- Feature: MySQL protocol support for prepared statements.
- Feature: Thrift protocol support for parameterized queries.
- Bugfix: MySQL console bugfix for pragma statements. 
- Bugfix: ActorDB was listening on all interfaces instead of just the one it actually is configured to use.
- Bugfix: Removing nodes was broken. 


**0.9 - 7 May 2015**
- Bugfix: Pragma exists was still creating actor files when actor did not actually exist.
- Bugfix: Add timeout to recovery process so that it can get started again if leader changes.


**0.9pre9 - 30 Apr 2015**
- Bugfix: Bugfix replication with noop append entries could cause a follower to never catchup.
- Improvement: Added {error,consensus_timeout} when queries are unable to be processed because not enough nodes are available.
- Improvement: Multiactor updates now return how many actors have been changed.

**0.9pre8 - 13 Apr 2015**
- Bugfix: Transaction size bugfix, 0.9pre7 turns out just made it more rare to appear.
- Bugfix: If actor was deleted, pragma exists still returned true.
- Bugfix: Actor delete did not complete if schema had an autoincrement.


**0.9pre7 - 2 Apr 2015**
- Bugfix: Manually setting cache size was a limit to transaction size.


**0.9pre6 - 31 Mar 2015**
- Bugfix: Rename two thrift values as they had language conflicts.


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
