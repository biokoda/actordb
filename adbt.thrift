namespace java com.actordb.thrift
namespace cpp com.actordb
namespace csharp Actordb
namespace py actordb
namespace php actordb
namespace perl Actordb
namespace rb ActordbThrift
namespace erl adbt

const string VERSION = "1.1.0"

union Val
{
  1: i64 bigint,
  2: i32 integer,
  3: i16 smallint,
  4: double real,
  5: bool bval,
  6: string text,
  7: bool isnull
}

struct ReadResult
{
  1: required bool hasMore, // not used yet
  2: required list<string> columns,
  3: required list<map<string,Val>> rows
}

struct WriteResult
{
  1: required i64 lastChangeRowid,
  2: required i64 rowsChanged
}

struct LoginResult
{
  1: required bool success,
  2: optional string error
  3: optional list<string> readaccess;
  4: optional list<string> writeaccess;
}

union Result
{
  1: ReadResult rdRes,
  2: WriteResult wrRes
}

enum ErrorCode {
  NotLoggedIn = 1,
  EmptyActorName = 2,
  InvalidActorName = 3, // invalid characters
  InvalidType = 4,      // invalid actor type
  NotPermitted = 5,     // accessing actor that user does not have permission for
  SqlError = 6,
  ConsensusTimeout = 7, // cluster is unable to reach consensus, query was not executed
  LocalNodeMissing = 8, // when creating a cluster, node where init was attempted was missing
                        // from the node list.
  MissingGroupInsert = 9,  // when creating a cluster, if no groups are specified
  MissingNodesInsert = 10, // when creating a cluster, if no nodes are specified
  MissingRootUser = 11, // when creating a cluster, if no root user was specified
  LoginFailed     = 12, // username and/or password was incorrect
  NotInitialized  = 13, // query before actordb initialized
  Error = 100           // unknown error
}

exception InvalidRequestException {
  1: required ErrorCode code,
  2: required string info
}

service Actordb {

  string protocolVersion(),

  LoginResult login(1: required string username, 2: required string password) throws (1:InvalidRequestException ire), 

  // Initialize instance/cluster(s), create users
  Result exec_config(1: required string sql) throws (1:InvalidRequestException ire),

  // Change schema
  Result exec_schema(1: required string sql) throws (1:InvalidRequestException ire),

  // query for 1 actor of type
  Result exec_single(1: required string actorname, 2: required string actortype, 3: required string sql, 4: list<string> flags = []) throws (1:InvalidRequestException ire),

  // query for 1 actor of type with prepare argument
  Result exec_single_prepare(1: required string actorname, 2: required string actortype, 3: required string sql, 4: list<string> flags = [], 5: list<list<Val>> bindingvals = []) throws (1:InvalidRequestException ire),

  // query over some actors of type
  Result exec_multi(1: required list<string> actors, 2: required string actortype, 3: required string sql, 4: list<string> flags = []) throws (1:InvalidRequestException ire),

  // query over some actors of type prepare argument
  Result exec_multi_prepare(1: required list<string> actors, 2: required string actortype, 3: required string sql, 4: list<string> flags = [], 5: list<list<Val>> bindingvals = []) throws (1:InvalidRequestException ire),

  // query over all actors for type
  Result exec_all(1: required string actortype, 2: required string sql, 3: list<string> flags = []) throws (1:InvalidRequestException ire),

  // query over all actors for type with prepare argument
  Result exec_all_prepare(1: required string actortype, 2: required string sql, 3: list<string> flags = [], 4: list<list<Val>> bindingvals = []) throws (1:InvalidRequestException ire),

  // all in sql: actor sometype(actorname) create; select * from mytab;
  Result exec_sql(1: required string sql) throws (1:InvalidRequestException ire),

  // all in sql: actor sometype(actorname) create; select * from mytab; with prepare argument
  Result exec_sql_prepare(1: required string sql, 2: list<list<Val>> bindingvals = []) throws (1:InvalidRequestException ire)

}
