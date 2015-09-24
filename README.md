### ActorDB is a distributed SQL database...

with the scalability of a KV store, while keeping the query capabilities of a relational database.

ActorDB is based on the actor model of computation. Unlike traditional monolithic databases, ActorDB is made out of any number of small independent and concurrent SQL databases called actors.

You can think of ActorDB is a maximally sharded SQL database. Instead of splitting a database into N shards of M users, every user has his own shard in ActorDB.

You can run queries or transactions on a single actor or across any number of actors. ActorDB can run on a single server or many servers. Writing to one actor is completely independent of writes to another actor, unless they are participating in the same transaction.

Homepage: http://www.actordb.com/

ActorDB is:

*   A distributed relational SQL database.
*   Consistent (not eventually consistent).
*   Distributed.
*   Redundant.
*   Massively concurrent.
*   No single point of failure.
*   ACID.
*   Connectable over MySQL protocol and [Thrift](https://github.com/biokoda/actordb/blob/master/adbt.thrift).
*   Replicated safely using the Raft distributed consensus algorithm.

Advantages

*   Complete horizontal scalability. All nodes are equivalent and you can have as many nodes as you need.
*   Full featured ACID database.
*   Suitable for very large datasets over many actors and servers.
*   No special drivers needed. Use the mysql driver of your language of choice.
*   Easy to configure and administer.
*   No global locks. Only the actors (one or many) involved in a transaction are locked during a write. All other actors are unaffected.
*   Uses stable reliable SQL and storage engines: SQLite on top of LMDB.

Documentation: http://www.actordb.com/docs-about.html

Story: http://blog.biokoda.com/post/112206754025/why-we-built-actordb

How to configure and run: http://www.actordb.com/docs-configuration.html

Change log: https://github.com/biokoda/actordb/blob/master/CHANGES.md

**ubuntu/debian package (64bit)**

https://s3-eu-west-1.amazonaws.com/biokoda/actordb_0.10.3-1_amd64.deb

**osx package (64bit):**

https://s3-eu-west-1.amazonaws.com/biokoda/actordb-0.10.3-OSX-x86_64.tar.gz

**red hat/centos package (64bit):**

Centos 6: https://s3-eu-west-1.amazonaws.com/biokoda/actordb-0.10.3-1.el6.x86_64.rpm

Centos 7: https://s3-eu-west-1.amazonaws.com/biokoda/actordb-0.10.3-1.el7.centos.x86_64.rpm

**windows package (64bit):**

https://s3-eu-west-1.amazonaws.com/biokoda/actordb-0.10.3-win-x86_64.zip
