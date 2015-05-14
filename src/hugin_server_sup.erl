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

-define(SERVER(M, SupId),
        {M,
         {gen_server, start_link,
          [{local, M}, hugin_server, [M, SupId], [ ]]},
         permanent, 5000, worker, [hugin_server]}).

%% Supervisor callbacks

start_worker(Pid, Id, Url) ->
  supervisor:start_child(Id, ?WORKER(Pid, Url)).

init([Module, SupId]) ->
  {ok, {{one_for_one, 10, 1}, [?SERVER(Module, SupId)]}}.
