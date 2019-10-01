### ActorDB is a distributed SQL database...

with the scalability of a KV store, while keeping the query capabilities of a relational database.

ActorDB is ideal as a server side database for [apps](http://www.actordb.com/docs-examples.html#example_filesync). Think of running a large mail service, dropbox, evernote, etc. They all require server side storage for user data, but the vast majority of queries is within a specific user. With many users, the server side database can get very large. Using ActorDB you can keep a full relational database for every user and not be forced into painful scaling strategies that require you to throw away everything that makes relational databases good.

ActorDB is a database that does not hide sharding from you. It makes it explicit, so you can keep fully relational chunks (i.e. actors) for the 99% of your database queries.

Even if your data model is not easily partitioned, ActorDB has a powerful KV data type that you can use instead. An [ActorDB KV](http://www.actordb.com/docs-kvstore.html#about_kv_store) type is an sql table that is partitioned across all servers. That table can have sub tables linked to it using foreign keys.

You can run queries or transactions on a single actor or across any number of actors. ActorDB can run on a single server or many servers. Writing to one actor is completely independent of writes to another actor, unless they are participating in the same transaction.

Servers can be added and schema can be updated at any time while the database is running.

Homepage: http://www.actordb.com/

For any questions you can use: https://gitter.im/actordb/

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
*   Inherits SQLite features like JSON support and common table expressions.

### Would you like to contribute?

What we would most like to see is more client libraries on top of Thrift. Thrift generated code can be a bit verbose. Generally it is much nicer to implement an interface to it that hides some boilerplate code and uses nicer types.

Also if you have any ideas, thoughts on possible improvements or bugs to report, contact us using github issues.

So if you're interested in contributing. Use your language of choice. Generate a thrift interface using our [adbt.thrift](https://github.com/biokoda/actordb/blob/master/adbt.thrift), then write a clean interface to it.

We will list any outside contributions here.

### Learn more

Documentation: http://www.actordb.com/docs-about.html

Story: http://blog.biokoda.com/post/112206754025/why-we-built-actordb

How SQLite runs on top of LMDB: http://blog.biokoda.com/post/133121776825/actordb-how-and-why-we-run-sqlite-on-top-of-lmdb

How to configure and run: http://www.actordb.com/docs-configuration.html

Change log: https://github.com/biokoda/actordb/blob/master/CHANGES.md

### Client libs

Erlang: https://github.com/biokoda/actordb_client

.NET 2.0: https://github.com/hq-io/actordb-net

### Builds

**ubuntu/debian package (64bit)**

https://dzbscw1ubdtyw.cloudfront.net/actordb_0.10.29-1_amd64.deb

**osx package (64bit):**

https://dzbscw1ubdtyw.cloudfront.net/actordb-0.10.29-OSX-x86_64.tar.gz

**red hat/centos package (64bit):**

Centos 7: https://dzbscw1ubdtyw.cloudfront.net/actordb-0.10.29-1.el7.x86_64.rpm

**general linux build (64bit)**

https://dzbscw1ubdtyw.cloudfront.net/actordb-0.10.29-linux.tar.gz

**windows package (64bit):**

https://dzbscw1ubdtyw.cloudfront.net/actordb-0.10.25-win-x86_64.zip
