namespace java com.actordb.thrift
namespace cpp com.actordb
namespace csharp Actordb
namespace py actordb
namespace php actordb
namespace perl Actordb
namespace rb ActordbThrift
namespace erl adbt
namespace go actordb

const string VERSION = "1.3.0"

union Val
{
  1: i64 bigint,
  2: i32 integer,
  3: i16 smallint,
  4: double real,
  5: bool bval,
  6: string text,
  7: bool isnull,
  8: binary blob
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
  InvalidActorName = 3,       // Invalid characters
  InvalidType = 4,            // Invalid actor type
  NotPermitted = 5,           // Accessing actor that user does not have permission for
  SqlError = 6,
  ConsensusTimeout = 7,       // After writing not enough nodes responded to confirm. 
                              // Write may later be successfuly replicated, or it may be
                              // abandoned. 
  ConsensusImpossibleAtm = 8, // Query was not executed because not enough nodes are online.
  LocalNodeMissing = 9,       // When creating a cluster, node where init was attempted was missing
                              // from the node list.
  MissingGroupInsert = 10,    // When creating a cluster, if no groups are specified
  MissingNodesInsert = 11,    // When creating a cluster, if no nodes are specified
  MissingRootUser = 12,       // When creating a cluster, if no root user was specified
  LoginFailed     = 13,       // Username and/or password was incorrect
  NotInitialized  = 14,       // Query before actordb initialized
  NoCreate        = 15,       // Query without create flag was attempted on an actor which does not exist.
  Error = 100                 // Unknown error
}

exception InvalidRequestException {
  1: required ErrorCode code,
  2: required string info
}

service Actordb {

  string protocolVersion(),

  LoginResult login(1: required string username, 2: required string password) throws (1:InvalidRequestException ire), 

  // For safer login, get 20 bytes of cryptographically random data, use it to hash password for login call.
  // It uses the same hashing algorithm as mysql:
  // SHA1( password ) XOR SHA1( "20-bytes random data from server" <concat> SHA1( SHA1( password ) ) )
  binary salt(), 

  // Initialize instance/cluster(s), create users
  Result exec_config(1: required string sql) throws (1:InvalidRequestException ire),

  // Change schema
  Result exec_schema(1: required string sql) throws (1:InvalidRequestException ire),

  // query for a single actor of type
  Result exec_single(1: required string actorname, 2: required string actortype, 3: required string sql, 4: list<string> flags = []) throws (1:InvalidRequestException ire),

  // query for a single actor of type with parameterized query (ex.: "insert into tab values (?1,?2,?3)")
  // This is faster and safer.
  Result exec_single_param(1: required string actorname, 2: required string actortype, 3: required string sql, 4: list<string> flags = [], 5: list<list<Val>> bindingvals = []) throws (1:InvalidRequestException ire),

  // query over multiple actors of type
  Result exec_multi(1: required list<string> actors, 2: required string actortype, 3: required string sql, 4: list<string> flags = []) throws (1:InvalidRequestException ire),

  // query over all actors for type
  Result exec_all(1: required string actortype, 2: required string sql, 3: list<string> flags = []) throws (1:InvalidRequestException ire),

  // all in sql: actor sometype(actorname) create; select * from mytab;
  Result exec_sql(1: required string sql) throws (1:InvalidRequestException ire),

  // all in sql but with parameterized query
  Result exec_sql_param(1: required string sql, 2: list<list<Val>> bindingvals = []) throws (1:InvalidRequestException ire),

  // Which actor types in schema.
  list<string> actor_types(),

  // Which tables are in an actor type.
  list<string> actor_tables(1: required string actor_type),

  // Which columns for actor type and table.
  map<string,string> actor_columns(1: required string actor_type, 2: required string actor_table),

  // Returns a unique integer
  i64 uniqid()

}
