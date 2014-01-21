### ActorDB is a distributed SQL database...

with the scalability of a KV store, while keeping the query capabilities of a relational database.

ActorDB is based on the actor model of computation. Unlike traditional monolithic databases, ActorDB is made out of any number of small independent and concurrent SQL databases called actors. 

You can think of ActorDB is a maximally sharded SQL database. Instead of splitting a database into N shards of M users, every user has his own shard in ActorDB.

You can run queries or transactions on a single actor or across any number of actors. ActorDB can run on a single server or many servers. Writing to one actor is completely independent of writes to another actor, unless they are participating in the same transaction. 

ActorDB is:

*   Consistent (not eventually consistent).
*   Distributed.
*   Redundant.
*   Massively concurrent.
*   No single point of failure.
*   Based on sqlite (every actor is an sqlite database).
*   ACID.
*   Runs over MySQL protocol.

Advantages

*   Complete horizontal scalability. All nodes are equivalent and you can have as many nodes as you need.
*   Full featured ACID database.
*   Suitable for very large datasets over many actors and servers.
*   Does not reinvent the storage engine and it does not avoid the issue by being RAM only. Sqlite is one of the most well tested, stable and reliable pieces of software in use. Not only that, you can query the actual database files directly with your language of choice. There is an sqlite interface implemented for practically every language.
*   No special drivers needed. Use the mysql driver of your language of choice. 
*   Easy to configure and administer. 
*   No global locks. Only the actors (one or many) involved in a transaction are locked during a write. All other actors are unaffected.

Sacrifices

*   Decoupled data. You can't run arbitrary queries over your data. But you can still have a full relational database within actors. So you need to organize and split your data model into actors. Read the query model and examples sections to understand how.
*   Single actor performance is limited to the speed of an sqlite instance (which is still quite fast). If you require a lot of data (many GB) within a single actor, ActorDB may not be the best tool for the job. You can however create copies of actors and implement archiving of old data. If an actor grows to some size, you can: 
	1. Create a copy of an actor
	2. Delete old data from original actor
	3. Keep copy as an archive 

How to configure and run: https://github.com/biokoda/actordb/blob/master/CONFIGURE.md
deb package: https://s3-eu-west-1.amazonaws.com/biokoda/actordb_0.5-1_amd64.deb
osx package: https://s3-eu-west-1.amazonaws.com/biokoda/actordb-0.5-OSX-x86_64.tar.gz
More packages to come...	

### 1. Query model

####1.1 ACTOR statement

**1.1.1 Single actor**

Queries to ActorDB always need to be directed to an actor. For this reason ActorDB uses the "ACTOR" command. 

Basic query, sent as a single string to the database:

    ACTOR actortype(actorid) create;
    SELECT * FROM sometable;


**actorid** - id that you have set for actor. 

**actortype** - type of actor. You can have multiple types of actors with completely different schemas.

**create** (flag) - if actorid does not exist, create and actor with this id. If you don't wish to create it, ommit this flag.

**sometable** - a table defined in **actortype** schema.


You can update multiple types of actors in a single query.

    ACTOR actortype1(actorid1) create;
    INSERT INTO table1 VALUES (1,'some text');
    ACTOR actortype2(actorid2) create;
    INSERT INTO tableX VALUES (3,'something else');

This is a transaction over two actors. They will both be updated or none of them will.

**1.1.2 Multiple actors**
    
    ACTOR actortype(actorid1,actorid2,actorid3) create;
    INSERT INTO sometable VALUES (1,'txt');

Insert will execute on all three actors of type "actortype" inside a transaction. If an actor does not exist, it will be created. 

**1.1.3 All actors of a type**

    ACTOR type1(*);
    INSERT INTO sometable VALUES (1,'txt');

This query will run on all actors of type1. If there are many actors of type1 calling this is not recommended.

**1.1.4 Loop over actors**

When your list of actors over which you wish to run a query is stored in another actor.  

    ACTOR type1(actor);
    {{ACTORS}}SELECT * FROM table2;
    ACTOR type1(for X.txt in ACTORS) create;
    insert into sometable values (1,'{{X.val}}');

What is going on line by line:

1. Select a single actor of type1. It must exist because there is no create flag.
2. Run a select over table2 and store the result in a variable ACTORS
3. For every row in ACTORS, read the .txt column which contains the name of an actor. These actors are of type "type1". If they do not exist, they will be created.
4. For every actor in ACTORS, insert a row into "sometable". Use another column from current row of ACTORS. 


####1.2 Variables

As mentioned in 1.1.4 ActorDB uses variables to pass data around when doing queries over multiple actors. 

There are also special variables available:

**{{uniqid}}** ActorDB will replace this with a unique integer id. {{uniqid}} is always increasing globally across all servers every time it is used. Calls to one server are guaranteed to always have a bigger id, calls to different servers are not, but are guaranteed to be unique.

    ACTOR type1(actor);
    INSERT INTO table VALUES ({{uniqid}}, 'sometext');
 
**{{uniqid.X}}** will also insert a unique integer value, but can be referenced in multiple places:
    
    ACTOR type1({{uniqid.myid}}) create;
    INSERT INTO table VALUES (1,'actor created with id {{uniqid.myid}}');

**{{curactor}}** when executing statements over multiple actors, {{curactor}} will always reference the current executing actor. It will only work in queries over multiple actors.

    ACTOR type1(actor1,actor2,actor3);
    INSERT INTO table VALUES (1,'my id is {{curactor}}')

**{{RESULT}}** when executing a read query across multiple actors, you have to add the result to this variable:
    
    ACTOR type1(actor1,actor2,actor3);
    {{RESULT}}SELECT * FROM tablex;

Result set will contain an additional column with every row named "actor", which will contain the name of actor to which result row belongs to. Do not use {{RESULT}} in queries to a single actor.

**1.2.1 Appending columns to result**

Say you have a forum thread actor. Every post in the thread contains the user id of the user that wrote the message. But when reading the thread you need more than just the user id, you need his username:

    ACTOR thread(1);
    {{RESULT}}SELECT * FROM thread;
    ACTOR user(for X.userid in RESULT);
    {{INFO}}SELECT * FROM userinfo WHERE id=1;
    {{X.username=INFO.name}}

Explanation line by line:

1. Switch to thread actor with id 1
2. Read all posts in thread table and store in "RESULTS" variable
3. Loop over all posts by userid column and switch to user with that id.
4. Read from his userinfo table
5. Take name from "INFO" and store into username column of result. This column does not need to exist in thread table. It will be added.

#### 1.3 PRAGMA statement

PRAGMA commands do specific operations or queries over one or many actors.

**PRAGMA list**

Will list all actors of a certain type

    ACTOR thread(*);
    PRAGMA list;
    
**PRAGMA delete**

Will delete all actors listed in "ACTOR" statement.

    ACTOR thread(thread1,thread2);
	PRAGMA delete;

**PRAGMA exists**

Will return true or false if actor exists. Works only on a single actor.

    ACTOR thread(thread1);
    PRAGMA exists;
    
**PRAGMA count**

Will count number of existing actors of a type.

    ACTOR thread(*);
    PRAGMA count;

**PRAGMA copy**

Will create a copy of an actor with a different name. Actor listed in "ACTOR" statement is new actor, actor listed in PRAGMA statement is actor that will be copied over. The destination name of actor should not exist. If it does it will either be overwritten or operation will fail, because that actor is actively executing requests.

    ACTOR thread(threadCopyTo);
    PRAGMA copy=threadCopyFrom;
 

### 2. Operational characteristics

An ActorDB setup is a network of small replication clusters of recommended size 3 but can be any size. Actors are asigned a replication cluster and live within that replication cluster. There can be as many replication clusters as you need and you can add new clusters at any time. ActorDB is tightly coupled on the replication cluster level, but loosely coupled between replication clusters. You can grow an ActorDB setup from a single 3 node replication cluster to hundreds of clusters if you need.

Every write to an actor is executed on all nodes of a replication cluster. SQL commands will succeed only if a majority of the cluster is online.

* 3 node cluster means any one of three nodes can go offline and actordb will continue to work.
* 2 node cluster means both nodes have to be online. This means availability is lower than a 1 node replication cluster. 
* 1 node cluster means if that node goes offline, the actors assigned to this single node cluster will be unavailable.

If you are using a cloud hosting provider, it is recommended to have each node of a replication cluster in a different availability zone. 

Expanding should be done by adding a replication cluster, not adding a single node to a replication cluster. You can add a node to an existing cluster, but it is not recommended. You can have clusters of more than 3 servers, but it is not optimal.

### 3. Performance characteristics

ActorDB can work across multiple hard drives in a single node. It can work across as many nodes as you need. Single actor performance is limited to the speed of an sqlite instance (which should be quite fast) and your configured replication level. 

ActorDB is not concurrent within a transaction. Writes to a single actor are fast and independent of other actors. Writes to multiple actors at the same time are O(2+2N). Use them only when you require them for consistency reasons. For optimal performance, structure your database to minimize multi-actor updates. 

Read performance for entire ActorDB network will grow linearly with every added node. Write performance will grow linearly with every added replication cluster. Writes are executed on all nodes of a replication cluster.

### 4. Key/Value store

ActorDB can also act as a key/value store by maintaining a sharded table across all clusters. The syntax is a bit verbose and not idiot proof, but it is quite powerful.

In schema.yaml file you can define a table like this:

    mykvtype: 
     type: kv
     schema: 
      - CREATE TABLE actors (id TEXT UNIQUE, hash INTEGER, val INTEGER)

- mykvtype is the key/value namespace. You can have multiple namespaces with different table schemas.
- kv specifies this type as a sharded key/value table.
- Table must be named "actors" and it must have id and hash columns. id is key, hash is hash of id, everything after that is up to you.
- Note: Only a single table is supported at the moment.

Inserting a value:

    ACTOR mykvtype(mykey);
    insert into actors values ('mykey',{{hash(mykey)}},1);

Very similar to how actors are queried. First line sets namespace and id. Second line is what gets executed on shard. First two columns must be written like above. If string for key (mykey) is not written the same in all three places, the KV store will not work correctly.

You can return an entire KV namespace with a simple query:

    ACTOR mykvtype(*);
    {{RESULT}}SELECT * FROM actors;
    

#### 4.1 Use case: reliable distributed counters

Say you have an analytics application that needs to be able count pageviews. Single points of failure or data loss are out of the question. You basically have two operations: increment and read counter.

If you have 2 clusters of 3 servers. Create 12 counter IDs (24 would probably be better).

Pick key for increment: N = RandomNumber % 12.

**Increment**

    ACTOR mykvtype(N);
    UPDATE actors SET val = val+1 WHERE id='N';

**Read counters**
    
    ACTOR mykvtype(1,2,3,4,5,6,7,8,9,10,11,12);
    {{RESULT}}SELECT * FROM actors where id='{{curactor}}';

This will return row for every key. You have sum them manually however.

### 5. Examples 

Web forums, news sites, blogs and even websites like reddit can all fit within the same basic hierarchical structure. 

    Landing page -> subsection1 -> article1
                                   article2
                                   ...
                 -> subsection2 -> ...
                 -> ...

Each level is an actor type. Article actors are what gets changed most frequently and where the scalability and concurrency of ActorDB is most important.

Landing page contains the current emphasized articles and links to subsections. This is a single actor, but is easily cachable with memcached or varnish. 

Subsection contains titles, ids and other basic information about articles that belong to it.

Articles cointain the article data (text,links) and comments.

####5.1 Web Forum

Actor types: subforums, threads, users. 

schema.yaml would look like this:

    user:
    - CREATE TABLE user (id INTEGER PRIMARY KEY, email TEXT, username TEXT, registered_at DATETIME);
    - CREATE TABLE mymessages (id INTEGER PRIMARY KEY, threadid INTEGER);

    thread:
    - CREATE TABLE messages (id INTEGER PRIMARY KEY, txt TEXT, user INTEGER, time DATETIME);

    subforum:
    - CREATE TABLE threads (id INTEGER PRIMARY KEY, title TEXT, created DATETIME);

**create new thread** query:

    ACTOR thread({{uniqid.threadid}}) create;
    INSERT INTO messages VALUES ({{uniqid.msgid}}, 'txt', _userid_, datetime('now'));
    ACTOR user(_userid_);
    INSERT INTO mymessages VALUES ({{uniqid.msgid}},{{uniqid.threadid}});
    ACTOR subforum(_subforumid_);
    INSERT INTO threads VALUES ({{uniqid.threadid}},'Thread title',datetime('now'))

This query is sent to ActorDB in a single call. Activities line by line:

1. Create an actor of type thread with a generated unique integer id named threadid. ACTOR command will either create an actor if it does not exist or switch to that actor
and cause all sql statements to be sent to that actor until next ACTOR command.
2. Insert first message into thread.
3. Switch to actor with a known id _userid_ of type user.
4. Insert msg id into his own table of messages. 
5. Switch to subforum actor.
6. Insert new thread.

**read thread**

    ACTOR thread(_threadid_);
    SELECT * FROM messages LIMIT 30;

**post message to existing thread**

    ACTOR thread(_threadid_);
    INSERT INTO messages VALUES ({{uniqid.msgid}},'I disagree because of reasons.',_userid_,datetime('now'));
    ACTOR user(_userid_)
    INSERT INTO mymessages VALUES ({{uniqid.msgid}},_threadid_);


### 6. Planned features

* Incremental backups.
* Key/Value additional tables. Right now KV store only has a single sharded table. Add additional tables for a hierarchical data structure (which still is tied to a key).
* Downscaling.
* Streaming large responses to client.
* SSL communication
