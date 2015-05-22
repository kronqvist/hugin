-module(hugin_server).

-behaviour(gen_server).

%% api
-export([set_options/2, url/1, url/2, worker_completed/3]).

%% private gen_server api
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
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

%%%========================================================================
%%% API
%%%========================================================================

set_options(ServerRef, Options) ->
  gen_server:call(ServerRef, {set_options, Options}).

url(ServerRef) ->
  gen_server:call(ServerRef, url).

url(ServerRef, Urls) ->
  gen_server:cast(ServerRef, {url, Urls}).

worker_completed(Pid, Url, Response) ->
  gen_server:cast(Pid, {worker_completed, Url, Response}).

%%%========================================================================
%%% Gen Server Callback
%%%========================================================================

%% Init will call the callback's init function which
%% allows the callback to:
%%   * perform any side effect
%%   * provide a starting state for its own callback
%%   * provide a list of URLs to start fetching
%%   * provide a list of options to modify the standard behavior of Hugin
init([Callback, SupId]) ->
  case callback(Callback, init, [ ]) of

    %% Normal conditions
    {ok, CallbackState, Urls, Opts} when is_list(Urls) ->

      case catch set_options1(Opts, #state{ }) of
        %% At least one option was not valid
        {'EXIT', _} ->
          {stop, badarg};

        %% All options are valid, server will start
        State ->
          handle_init_return(
            State#state{
               callback  = Callback,
               sup       = SupId,
               state     = CallbackState,
               urls      = Urls
             })
      end;

    %% The following two is to allow the callback to be compliant
    %% with the normal gen_server behavior.
    %%
    %% One
    {stop, Reason} ->
      {stop, Reason};

    %% Two
    ignore ->
      ignore;

    %% Anything else will crash the server with a badarg exception
    _ ->
      {stop, badarg}

  end.

%% Return the list of URLs that has not yet been fetched
handle_call(url, _From, S) ->
  handle_call_return(S#state.urls, S);

%% Set new options. This will not be done half-way. Either all options will
%% be set, or none. If any option is not valid the function will fail with a badarg.
handle_call({set_options, Options}, _From, S) ->
  case catch set_options1(Options, S) of
    {'EXIT', _} ->   handle_call_return({error, badarg}, S);
    #state{} = S1 -> handle_call_return(ok, S1)
  end;

%% Please don't crash if someone is abusing us
handle_call(_Request, _From, S) ->
  handle_call_return(ok, S).

%% Add more URLs to the list to be fetched
handle_cast({url, Urls}, S) ->
  handle_cast_return( S#state{ urls = S#state.urls ++ Urls });

%% Worker has completed. We need to call our callback's request/3 function.
%% This allows the callback to:
%%   * perform any side effect
%%   * provide a starting state for its own callback
%%   * provide a list of URLs to start fetching
%%   * TODO: provide a list of options to modify the standard behavior of Hugin
%%   * provide a "heart beat" (timeout)
handle_cast({worker_completed, Url, Response}, S) ->
  case callback(S#state.callback, request, [Url, Response, S#state.state]) of
    %% normal case
    {ok, Urls, NewState} -> S1 = S;

    %% callback provided us with a heart beat request
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

%% Please don't crash if someone is abusing us
handle_info(_Info, S) ->
  handle_info_return(S).

%% Please don't crash if someone is abusing us
code_change(_OldVsn, S, _Extra) ->
  {ok, S}.

%% Please don't crash if someone is abusing us
terminate(_Reason, _S) ->
  ok.


%%%========================================================================
%%% Internal Functions
%%%========================================================================


%% Functions to give a proper gen_server return value.
%% These functions will also decide to return a timeout if one of the
%% following conditions are met:
%%   * we can fetch a new URL right away in which case 0 is returned
%%   * we can fetch a new soon in which case that time is returned
%%   * callback has requested a "heart beat" in which case that heart
%%     beat timeout is returned
%%
%% If no timeout is returned it means that we must wait until one of the
%% current requests have finished
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

%% If a timeout should be returned or not
should_use_timeouts(S) ->
  max_par_ok(S) andalso max_freq_ok(S).

%% If a timeout should be returned due to max_par option
max_par_ok(#state{ max_par = infinity }) -> true;
max_par_ok(#state{ pending = Pending, max_par = N }) ->
  length(Pending) < N.

%% If a timeout should be returned due to max_freq option
max_freq_ok(S) ->
  not (empty(S#state.urls) andalso empty(S#state.timeouts)).

%% Gives current time in milliseconds
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

%% Calculate timeout due to max_freq
timeout_r(_, #state{ restraint = false }) -> 0;
timeout_r(Now, S) ->
  case S#state.rcond of
    {restraint, Amount, _} when length(S#state.restraint_times) < Amount -> 0;
    {restraint, _, Every} ->
      Earliest = lists:last(S#state.restraint_times),
      min0(Earliest + Every - Now)
  end.

%% Calculate timeout due to callback's heart beat
timeout_t(_, #state{ timeouts = [ ] }) -> undefined;
timeout_t(Now, S) ->
  min0( hd(S#state.timeouts) - Now).

%% Returns 0 if value is negative and value otherwise
min0(Val) when is_integer(Val), Val < 0 -> 0;
min0(Val) when is_integer(Val)          -> Val.

%% Call the callback.
%% The callback can be one of the following:
%%   * A module in which case we will call Module:init|request|timeout
%%   * A function in which case we will call Fun(init|request|timeout, Args)
callback(Callback, CallbackType, Args) ->
  if
    is_atom(    Callback) -> apply(Callback,  CallbackType, Args);
    is_function(Callback) -> apply(Callback, [CallbackType, Args])
  end.

%% Return a new state with the updated options
set_options1(Options, S) ->
  lists:foldl(
    fun({Option, Data}, State) -> ?MODULE:Option(Data, State) end, S, Options).

%% Return a new state with the max_par option updated
max_par(N, S) when is_integer(N) orelse N == infinity ->
  S#state{ max_par = N }.

%% Return a new state with the max_freq option updated
max_freq(infinity, S) ->
  S#state{ restraint = false };
max_freq({A, Ms}, S) when is_integer(A), is_integer(Ms) ->
  S#state{ restraint = true, rcond = {restraint, A, Ms} }.
