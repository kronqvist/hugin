-module(hugin_server_sup).

-behaviour(supervisor).

%% Api
-export([start_worker/3]).

%% Supervisor callbacks
-export([init/1]).

-define(WORKER(Pid, Url),
        {make_ref(),
         {hugin_worker, start_link, [Pid, Url]},
         transient, 5000, worker, [hugin_worker]}).

-define(SERVER(Id, SupId, Cback),
        {Id,
         {gen_server, start_link,
          [{local, Id}, hugin_server, [Cback, SupId], [ ]]},
         permanent, 5000, worker, [hugin_server]}).

%% Supervisor callbacks

start_worker(Pid, Id, Url) ->
  supervisor:start_child(Id, ?WORKER(Pid, Url)).

init([Callback, SupId]) ->
  Id = server_id(Callback),
  {ok, {{one_for_one, 10, 1}, [?SERVER(Id, SupId, Callback)]}}.

server_id(F) when is_function(F) ->
  server_id(erlang:ref_to_list(make_ref()));

server_id(Module) ->
  list_to_atom( lists:concat([Module])).
