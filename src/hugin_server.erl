-module(hugin_server).

-behaviour(gen_server).

%% api
-export([worker_completed/3]).

%% private gen_server api
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% definitions
-record(state, {module,        %% Callback module
                sup,           %% Name of supervisor
                state,         %% State of implemented behaviour module
                urls    = [ ], %% Pool of unqueried urls
                pending = [ ], %% Urls currently being queried
                restraint,     %% Restraint option
                restraint_times = [ ] %%
               }).


%% API

worker_completed(Pid, Url, Response) ->
  gen_server:cast(Pid, {worker_completed, Url, Response}).

%% gen_server callbacks
init([Module, SupId]) ->
  case Module:init() of
    {ok, State, Urls, Opts} when is_list(Urls), Urls /= [ ] ->
      Restraint = lists:keyfind(restraint, 1, Opts),
      {ok, #state{ module    = Module,
                   sup       = SupId,
                   state     = State,
                   urls      = Urls,
                   restraint = Restraint}, 0};
    {stop, Reason} ->
      {stop, Reason};
    ignore ->
      ignore;
    _ ->
      {stop, {init, badargs}}
  end.

handle_call(_Request, _From, S) ->
  {reply, ok, S}.

handle_cast({worker_completed, Url, Response}, S) ->
  Module = S#state.module,
  {ok, Urls, S1} = Module:request(Url, Response, S#state.state),

  handle_cast_return(
    S#state{ pending   = S#state.pending -- [ Url ],
             urls      = S#state.urls ++ Urls,
             state     = S1 }).

handle_info(timeout, S) ->
  Now = now_(),
  [ Url | Urls ] = S#state.urls,
  hugin_server_sup:start_worker(self(), S#state.sup, Url),
  S1 = S#state{
         urls            = Urls,
         pending         = [ Url | S#state.pending ],
         restraint_times =
           [ Now ] ++ [ RT || RT <- S#state.restraint_times,
                         restraint(RT, Now, S#state.restraint) ]},
  handle_info_return(S1);

handle_info(_Info, S) ->
  handle_info_return(S).

code_change(_OldVsn, S, _Extra) ->
  {ok, S}.

terminate(_Reason, _S) ->
  ok.

handle_info_return(S) ->
  Now = now_(),
  case S#state.urls of
    [ _ | _ ] ->
      {noreply, S, calc_timeout(Now, S)};
    _ ->
      {noreply, S}
  end.

handle_cast_return(S) ->
  Now = now_(),
  case S#state.urls of
    [ ] -> {noreply, S};
    _   -> {noreply, S, calc_timeout(Now, S)}
  end.

now_() ->
  {MgS, S, Us} = erlang:now(),
  1000000000 * MgS + 1000 * S + Us div 1000.


restraint(_, _, false) -> false;
restraint(RT, Now, {restraint, _, Every}) ->
  RT >= Now - Every.

calc_timeout(Now, S) ->
  case S#state.restraint of
    false -> 0;
    {restraint, Amount, _} when length(S#state.restraint_times) < Amount -> 0;
    {restraint, _, Every} ->
      Earliest = lists:last(S#state.restraint_times),

      case Earliest + Every - Now of
        T when T < 0 -> 0;
        T -> T
      end
  end.
