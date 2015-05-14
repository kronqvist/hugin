-module(hugin).

-export([start/0, start/1]).

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

supervisor_id(Module) ->
  list_to_atom( lists:concat([Module, "_sup"])).
