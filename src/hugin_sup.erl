-module(hugin_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(SERVER(I, T), {I, {I, start_link, []}, permanent, 5000, T, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================
start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
  {ok, {{one_for_one, 10, 1}, []}}.
