#!/usr/bin/env escript


main(BinFiles1) ->
  BinFiles = ["deps/bkdcore/ebin/butil.beam",
    "deps/actordb_core/ebin/actordb_sql.beam",
    "ebin/actordb_console.beam"] ++ BinFiles1,
  Apps = [thrift,lager,poolboy, actordb_client,adbt],

  file:make_dir("ebin"),

  {ok, _Mod,Bin} = compile:file("src/actordb_console.erl", [binary,return_errors]),
  filelib:ensure_dir("ebin/"),
  file:write_file("ebin/actordb_console.beam",Bin),

  case os:type() of
    {win32, _} ->
      EndName = "actordb_wxconsole",
      {ok, _,WxBin} = compile:file("src/actordb_wxconsole.erl", [binary,return_errors]),
      filelib:ensure_dir("ebin/"),
      file:write_file("ebin/actordb_wxconsole.beam",WxBin);
    _ ->
      EndName = "actordb_console"
  end,

  %% Add ebin paths to our path
  % true = code:add_path("ebin"),
  ok = code:add_paths(filelib:wildcard("deps/*/ebin")),

  %% Read the contents of the files in ebin(s)
  Files1 = [begin
    FileList = filelib:wildcard("deps/"++atom_to_list(Dir)++"/ebin/*.*") ++ filelib:wildcard("ebin/*.*"),
    [{filename:basename(Nm),element(2,file:read_file(Nm))} || Nm <- FileList]
  end || Dir <- Apps],

  Files = [{filename:basename(Fn),element(2,file:read_file(Fn))} || Fn <- BinFiles]++lists:flatten(Files1),

  case zip:create("mem", Files, [memory]) of
    {ok, {"mem", ZipBin}} ->
      Script = <<"#!/usr/bin/env escript\n%%! +Bc \n", ZipBin/binary>>,
      case file:write_file("priv/"++EndName, Script) of
        ok -> ok;
        {error, WriteError} ->
          io:format("Failed to write ~p: ~p\n", [EndName,WriteError]),
          halt(1)
      end;
    {error, ZipError} ->
      io:format("Failed to construct ~p archive: ~p\n", [EndName,ZipError]),
      halt(1)
  end,

  %% Finally, update executable perms for our script
  case os:type() of
    {unix,_} ->
      [] = os:cmd("chmod a+x actordb_console"),
      ok;
    _ ->
      ok
  end.
