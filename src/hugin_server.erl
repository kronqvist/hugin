-module(hugin_server).

-behaviour(gen_server).

%% api
-export([pool/1,
         pool/2,
         worker_completed/3]).

%% private gen_server api
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%% definitions
-record(state, {module,               %% Callback module
                sup,                  %% Name of supervisor
                state,                %% State of implemented behaviour module
                urls     = [ ],       %% Pool of unqueried urls
                pending  = [ ],       %% Urls currently being queried
                timeouts = [ ],       %% Callback requested timeouts
                restraint = false,    %% Restraint option
                restraint_times = [ ],
                rcond                 %% Restraint condition
               }).

-type url() :: binary().
-type server_ref() :: atom() | pid().

%% API

-spec pool(ServerRef :: server_ref()) -> any().
pool(Server) ->
  gen_server:call(Server, pool).

-spec pool(ServerRef :: server_ref(), Urls :: [ url() ]) -> term().
pool(Server, Urls) ->
  gen_server:cast(Server, {pool, Urls}).

-spec worker_completed(Pid :: pid(), Url :: url(), Response :: term())
                      -> term().
worker_completed(Pid, Url, Response) ->
  gen_server:cast(Pid, {worker_completed, Url, Response}).

%% gen_server callbacks
init([ Module, SupId ]) ->
  case Module:init() of
    {ok, State, Urls, Opts} when is_list(Urls), Urls /= [ ] ->
      Restraint = lists:keyfind(restraint, 1, Opts),
      {ok, #state{
              module    = Module,
              sup       = SupId,
              state     = State,
              urls      = Urls,
              restraint = Restraint /= false,
              rcond     = Restraint}, 0};

    {stop, Reason} ->
      {stop, Reason};

    ignore ->
      ignore;

    _ ->
      {stop, {init, badargs}}
  end.

handle_call(pool, _From, S) ->
  handle_call_return(S#state.urls, S);

handle_call(_Request, _From, S) ->
  handle_call_return(ok, S).


handle_cast({pool, Urls}, S) ->
  handle_cast_return( S#state{ urls = S#state.urls ++ Urls });

handle_cast({worker_completed, Url, Response}, S) ->
  Module = S#state.module,

  case Module:request(Url, Response, S#state.state) of
    {ok, Urls, NewState} -> S1 = S;
    {ok, Urls, NewState, Tmt} ->
      Now = now_(),
      S1 = S#state{ timeouts = lists:sort([ Now + Tmt | S#state.timeouts ]) }
  end,

  handle_cast_return(
    S1#state{
      pending   = S1#state.pending -- [ Url ],
      urls      = S1#state.urls ++ Urls,
      state     = NewState }).

handle_info(timeout, S) ->
  Now = now_(),

  case not empty(S#state.timeouts) andalso Now >= hd(S#state.timeouts) of
    true ->
      Module = S#state.module,
      case Module:timeout(S#state.state) of
        {ok, Urls, NewState} ->
          handle_info_return(
            S#state{ urls = S#state.urls ++ Urls,
                     state = NewState,
                     timeouts = tl(S#state.timeouts)});
        {ok, Urls, NewState, Tmt} ->
          handle_info_return(
            S#state{
              urls = S#state.urls ++ Urls,
              state = NewState,
              timeouts = lists:sort([ Now + Tmt | tl(S#state.timeouts) ]) })
      end;
    false ->
      [ Url | Urls ] = S#state.urls,
      hugin_server_sup:start_worker(self(), S#state.sup, Url),

      handle_info_return(
        S#state{
          urls            = Urls,
          pending         = [ Url | S#state.pending ],
          restraint_times =
            [ Now ] ++ [ RT || RT <- S#state.restraint_times,
                               restraint(RT, Now, S) ]})
  end;

handle_info(_Info, S) ->
  handle_info_return(S).

code_change(_OldVsn, S, _Extra) ->
  {ok, S}.

terminate(_Reason, _S) ->
  ok.


%% internal functions

handle_info_return(S) ->
  Now = now_(),
  case empty(S#state.urls) andalso empty(S#state.timeouts) of
    true  -> {noreply, S};
    false -> {noreply, S, timeout(Now, S)}
  end.

handle_call_return(Reply, S) ->
  Now = now_(),
  case empty(S#state.urls) andalso empty(S#state.timeouts) of
    true  -> {reply, Reply, S};
    false -> {reply, Reply, S, timeout(Now, S)}
  end.

handle_cast_return(S) ->
  Now = now_(),
  case empty(S#state.urls) andalso empty(S#state.timeouts) of
    true  -> {noreply, S};
    false -> {noreply, S, timeout(Now, S)}
  end.

now_() ->
  {MgS, S, Us} = erlang:now(),
  1000000000 * MgS + 1000 * S + Us div 1000.

restraint(_, _, #state{ restraint = false }) -> false;
restraint(RT, Now, #state { rcond = {restraint, _, Every} }) ->
  RT >= Now - Every.

empty([ ]) -> true;
empty([_|_]) -> false.

timeout(Now, S) ->
  Rstr  = S#state.restraint andalso not empty(S#state.urls),
  TRstr = timeout_r(Now, S),
  Tmt   = not empty(S#state.timeouts),
  TTmt  = timeout_t(Now, S),

  if Rstr, Tmt -> lists:min([ TRstr, TTmt ]);
     Rstr      -> TRstr;
     Tmt       -> TTmt;
     true      -> 0
  end.

timeout_r(_, #state{ restraint = false }) -> 0;
timeout_r(Now, S) ->
  case S#state.rcond of
    {restraint, Amount, _} when length(S#state.restraint_times) < Amount -> 0;
    {restraint, _, Every} ->
      Earliest = lists:last(S#state.restraint_times),
      min0(Earliest + Every - Now)
  end.

timeout_t(_, #state{ timeouts = [ ] }) -> undefined;
timeout_t(Now, S) ->
  min0( hd(S#state.timeouts) - Now).

min0(Val) when is_integer(Val), Val < 0 -> 0;
min0(Val) when is_integer(Val)          -> Val.
