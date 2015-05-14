-module(hugin).

-export([start/0, start/1, pool/1, pool/2]).
-type url() :: binary().
-type server_ref() :: atom() | pid().

start() ->
  hackney:start(),
  application:ensure_started(hugin).

start(Module) ->
  Id = supervisor_id(Module),
  start(),
  supervisor:start_child(
    hugin_sup,
    {Id,
     {supervisor, start_link, [{local, Id}, hugin_server_sup, [Module, Id]]},
      transient, 5000, supervisor, [ hugin_server_sup ]}).


-spec pool(ServerRef :: server_ref()) -> [ url() ].
pool(ServerRef) ->
  hugin_server:pool(ServerRef).

-spec pool(ServerRef :: server_ref(), url()) -> ok | {error, term()}.
pool(ServerRef, Urls) ->
  hugin_server:pool(ServerRef, Urls).

%% private functions

supervisor_id(Module) ->
  list_to_atom( lists:concat([Module, "_sup"])).
