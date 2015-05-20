-module(hugin).

-export([start/0, start/1, pool/1, pool/2]).
-type url() :: binary().
-type server_ref() :: atom() | pid().
-type callback_type() :: init | request | timeout.
-type callback_fun() :: fun((callback_type(), Args :: [any()]) -> any()).

%% start application with no active crawlers
start() ->
  hackney:start(),
  application:ensure_started(hugin).

-spec start(Callback :: module() | callback_fun()) -> {ok, pid()}.

%% start application with one crawler and callback module M
%% M should implement the behavior "raven".
start(M) when is_atom(M) ->
  start1(M);

%% Start application with one crawler and callback function F.
start(F) when is_function(F) ->
  start1(F).

start1(Cback) ->
  start(),
  Id = supervisor_id(Cback),
  supervisor:start_child(
    hugin_sup,
    {Id,
     {supervisor, start_link, [{local, Id}, hugin_server_sup, [Cback, Id]]},
     transient, 5000, supervisor, [ hugin_server_sup ]}).


-spec pool(ServerRef :: server_ref()) -> [ url() ].
pool(ServerRef) ->
  hugin_server:pool(ServerRef).

-spec pool(ServerRef :: server_ref(), url()) -> ok | {error, term()}.
pool(ServerRef, Urls) ->
  hugin_server:pool(ServerRef, Urls).

%% private functions

supervisor_id(F) when is_function(F) ->
  supervisor_id(erlang:ref_to_list(make_ref()));

supervisor_id(Module) ->
  list_to_atom( lists:concat([Module, "_sup"])).
