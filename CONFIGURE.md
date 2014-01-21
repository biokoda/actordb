## 1. ActorDB configuration files:

- app.config: where to store data, mysql protocol port.
- vm.args: set name of node (also erlang vm tweak parameters, which you should probably leave default).
- nodes.yaml: list of all nodes
- schema.yaml: SQL database schema for every actor type.


app.config and vm.args need to be set on every node. 

nodes.yaml and schema.yaml only need to be set on one node, then sent to ActorDB with actordbctrl command. ActorDB will then store that data and send it to all nodes internally.

On install, default configuration files are placed in /etc/actordb or whichever is equivalent for your platform.


**nodes.yaml**    

Nodes file is split into two sections. First section lists all servers, second section organizes those servers into groups. This way you can manually organize nodes across different availability zones per cluster.

    # List all servers.
    nodes:
    - node1@192.168.1.2 
    - node2@192.168.1.3
    - node3@192.168.1.4

    # Organize servers.
    # Groups of type cluster are actordb data nodes.
    # Groups of other types will be able to communicate with cluster nodes, 
    #  but will not hold actordb data. They can be used as incremental backup nodes (coming soon), 
    #  client connection gateways and transaction managers. 
    groups:
    - name: grp2
      nodes: [node1,node2,node3]
      type: cluster


**schema.yaml**

Schema file contains the SQL create or alter table commands. The schema can be changed at any time, but only by adding SQL statements to the list. Every time a new actor is created, the entire list of SQL commands will be executed to create the schema. If schema changes while ActorDB is running, actors will be updated by applying the new statements in the list.

    # type1, type2 are types of actors. Every line is an update to the schema.
    # Lines SHOULD NEVER BE REMOVED. If lines are removed new actors will 
    #  have an incomplete schema and existing actors will not detect the schema change either.
    # To update the schema add lines for type or add new types.

    type1: 
    - CREATE TABLE tab (id INTEGER PRIMARY KEY, txt TEXT)   
    - CREATE TABLE tab1 (id INTEGER PRIMARY KEY, txt TEXT)  
    - ALTER TABLE tab ADD i INTEGER    		             
    - CREATE TABLE tabx (id INTEGER PRIMARY KEY, txt TEXT) 

    type2:
    - CREATE TABLE asdf (id INTEGER PRIMARY KEY AUTOINCREMENT, txt BLOB)

    # typekv is a key-value type. It will not be an actor but a fragmented table across all nodes
    typekv: 
     type: kv
     schema: 
      - CREATE TABLE actors (id TEXT UNIQUE, hash INTEGER, val INTEGER)



## 2. Setup guide

**FOR EVERY NODE**

1. Install package

2. Open /etc/actordb/vm.args and set name for node in format: nodename@ipordomain.
   Changing the cookie is good practice but not mandatory. Cookie has to be the same on every server.

3. OPTIONAL: Open /etc/actordb/app.config and set "main_db_folder" to something other than default. You can also specify additional DB folders on different hard drives. 

4. **Run:** sudo actordb start

**ON ONE NODE**

5. Open /etc/actordb/nodes.yaml and list all servers and organize them into groups

6. Open /etc/actordb/schema.yaml set your DB schema

7. **Run:** actordbctrl init

8. Backup your nodes and schema file. 

## 3. Changing schema

1. Open schema.yaml file. Add statements or types.
2. **Run:** actordbctrl updateschema

## 4. Adding a cluster

1. Do steps 2.1-2.4 for every node in cluster.
2. Open nodes.yaml file. Add nodes and create a new group. 
3. **Run:** actordbctrl updatenodes

## 5. Building ActorDB

1. Clone from github
2. make dist
3. make package
4. Release should be generated in: ./package/packages 

## 6. Running ActorDB dev mode

1. Clone from github
2. Move to actordb folder
3. ./startdev.sh
4. ./actordbctrl init

