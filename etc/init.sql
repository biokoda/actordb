--WARNING: Every sql statement must be in its own line.

-- First initialize node. Create group, create node and create root user. Only this created
-- user is able to change schema or change configuration. Once initialization is done
-- console will be connected as this user. Every user created in this stage will have all privileges.
-- Which means you should not create more than one. Add additional users later.
use config
insert into groups values ('grp1','cluster')
-- localnode() is whatever is in vm.args (-name ....) for node we are connected to.
insert into nodes values (localnode(),'grp1')
CREATE USER 'root' IDENTIFIED BY 'rootpass'
commit

-- Still in config db, now add second user to run queries with
CREATE USER 'myuser' IDENTIFIED BY 'mypass'
-- * means user has access to all actor types
GRANT read,write ON * to 'myuser'
-- We could also set a user that only has access to type1 actors with
-- CREATE USER 'type1user' IDENTIFIED BY 'type1pass'
-- GRANT read,write ON type1 to 'type1user';
commit

-- Set schema
use schema
actor type1
CREATE TABLE tab (id INTEGER PRIMARY KEY, txt TEXT)
CREATE TABLE tab1 (id INTEGER PRIMARY KEY, txt TEXT)
ALTER TABLE tab ADD i INTEGER
CREATE TABLE tabx (id INTEGER PRIMARY KEY CHECK (typeof(id) == 'integer'), txt TEXT CHECK (typeof(id) == 'text'))
actor type2
CREATE TABLE asdf (id INTEGER PRIMARY KEY AUTOINCREMENT, txt BLOB)
-- KV type of actor. This means counters is a sharded table across all nodes.
actor counters kv
CREATE TABLE actors (id TEXT PRIMARY KEY, hash INTEGER, val INTEGER) WITHOUT ROWID
-- Another KV type. This one with a nother sub table.
-- Any sub table must use foreign key on actors.id and have "on delete cascade"
actor filesystem kv
CREATE TABLE actors (id TEXT PRIMARY KEY, hash INTEGER, size INTEGER)  WITHOUT ROWID
CREATE TABLE users (id INTEGER PRIMARY KEY AUTOINCREMENT, fileid TEXT, uid INTEGER, FOREIGN KEY (fileid) REFERENCES actors(id) ON DELETE CASCADE)
commit
