-module(hugin_server).

-behaviour(gen_server).

%% api
-export([set_options/2,
         url/1,
         url/2,
         worker_completed/3]).

%% private gen_server api
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%% private
-export([max_freq/2, max_par/2]).

%% definitions
-record(state, {callback,            %% Callback module
                sup,                 %% Name of supervisor
                state,               %% State of implemented behaviour callback
                urls     = [ ],      %% Pool of unqueried urls
                pending  = [ ],      %% Urls currently being queried
                timeouts = [ ],      %% Callback requested timeouts
                max_par  = 5,        %% Max simultaneous outgoing connections
                restraint = false,   %% Restraint option
                restraint_times = [ ],
                rcond                %% Restraint condition
               }).

-type url() :: binary().
-type server_ref() :: atom() | pid().

%% API

-spec set_options(server_ref(), [hugin_opts:opt()]) -> ok | {error, any()}.

set_options(ServerRef, Options) ->
  gen_server:call(ServerRef, {set_options, Options}).


-spec url(ServerRef :: server_ref()) -> any().
url(ServerRef) ->
  gen_server:call(ServerRef, url).

-spec url(ServerRef :: server_ref(), Urls :: [ url() ]) -> term().
url(ServerRef, Urls) ->
  gen_server:cast(ServerRef, {url, Urls}).

-spec worker_completed(Pid :: pid(), Url :: url(), Response :: term())
                      -> term().
worker_completed(Pid, Url, Response) ->
  gen_server:cast(Pid, {worker_completed, Url, Response}).

%% gen_server callbacks
init([Callback, SupId]) ->
  case callback(Callback, init, [ ]) of
    {ok, CallbackState, Urls, Opts} when is_list(Urls) ->
      case catch set_options1(Opts, #state{ }) of
        {'EXIT', _} ->
          {stop, badarg};
        State ->
          handle_init_return(
            State#state{
               callback    = Callback,
               sup       = SupId,
               state     = CallbackState,
               urls      = Urls
             })
      end;

    {stop, Reason} ->
      {stop, Reason};

    ignore ->
      ignore;

    _ ->
      {stop, badarg}
  end.

handle_call(url, _From, S) ->
  handle_call_return(S#state.urls, S);

handle_call({set_options, Options}, _From, S) ->
  case catch set_options1(Options, S) of
    {'EXIT', _} ->   handle_call_return({error, bad_arg}, S);
    #state{} = S1 -> handle_call_return(ok, S1)
  end;

handle_call(_Request, _From, S) ->
  handle_call_return(ok, S).


handle_cast({url, Urls}, S) ->
  handle_cast_return( S#state{ urls = S#state.urls ++ Urls });

handle_cast({worker_completed, Url, Response}, S) ->
  case callback(S#state.callback, request, [Url, Response, S#state.state]) of
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
      case callback(S#state.callback, timeout, [S#state.state]) of
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

handle_init_return(S) ->
  case empty(S#state.urls) of
    true -> {ok, S};
    false -> {ok, S, 0}
  end.

handle_info_return(S) ->
  Now = now_(),
  case should_use_timeouts(S) of
    true  -> {noreply, S, timeout(Now, S)};
    false -> {noreply, S}
  end.

handle_call_return(Reply, S) ->
  Now = now_(),
  case should_use_timeouts(S) of
    true  -> {reply, Reply, S, timeout(Now, S)};
    false -> {reply, Reply, S}
  end.

handle_cast_return(S) ->
  Now = now_(),
  case should_use_timeouts(S) of
    true  -> {noreply, S, timeout(Now, S)};
    false -> {noreply, S}
  end.

should_use_timeouts(S) ->
  max_par_ok(S) andalso max_freq_ok(S).

max_par_ok(#state{ max_par = infinity }) -> true;
max_par_ok(#state{ pending = Pending, max_par = N }) ->
  length(Pending) < N.

max_freq_ok(S) ->
  not (empty(S#state.urls) andalso empty(S#state.timeouts)).





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

callback(Callback, CallbackType, Args) ->
  if
    is_atom(    Callback) -> apply(Callback,  CallbackType, Args);
    is_function(Callback) -> apply(Callback, [CallbackType, Args])
  end.

set_options1(Options, S) ->
  lists:foldl(
    fun({Option, Data}, State) -> ?MODULE:Option(Data, State) end, S, Options).

%% @private
max_par(N, S) when is_integer(N) ->
  S#state{ max_par = N }.

%% @private
max_freq({A, Ms}, S) when is_integer(A), is_integer(Ms) ->
  S#state{ restraint = true, rcond = {restraint, A, Ms} }.
