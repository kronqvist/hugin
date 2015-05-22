%% =====================================================================
%% @doc Hugin is a framework to simplify the process of defining your
%% own web crawlers. It tries to do so while maintaining maximum
%% flexibility and efficiency.
%%
%% The name Hugin comes from one the two ravens owned by Odin in
%% Norse mythology, Hugin and Munin. Odin sent out the two ravens every
%% morning to scout the world and bring the entire world's accumulated
%% information back to him.
%%
%% Similar to the case for Odin, the Hugin framework helps you to crawl
%% the internet in the pursuit of gathering the World Wide Web's
%% collective information and bringing it back to you.
%% @copyright 2015 Magnus Kronqvist
%% @author Magnus Kronqvist <magnus.kronqvist@gmail.com>
%% @version {@version}
%% @end
%% =====================================================================

-module(hugin).

-export([start/0, start/1]).
-export([url/1, url/2]).
-export([set_option/2, set_options/2, max_freq/3, max_freq/4, max_par/2]).

-type url() :: binary().
-type server_ref() :: atom() | pid().

%%%=========================================================================
%%%  API
%%%=========================================================================

-spec start() -> ok | {error, any()}.

%% @doc Start the application with no active crawlers.
start() ->
  hackney:start(),
  application:ensure_started(hugin).

-spec start(Callback :: module() | fun()) -> {ok, pid()}.

%% @doc Start the application together with one crawler.
%% The callback can be either a atom with the module or a function.
%% See {@link raven} for more information about the callback.
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


%% @doc Return the pool of URLs waiting to be fetched.

-spec url(ServerRef :: server_ref()) -> [ url() ].

url(ServerRef) ->
  hugin_server:url(ServerRef).

-spec url(ServerRef :: server_ref(), [url()]) -> ok | {error, any()}.

%% @doc Add more URLs to be fetched.

url(ServerRef, Urls) ->
  hugin_server:url(ServerRef, Urls).


%%%========================================================================
%%% Functions to edit the behavior of the server
%%%========================================================================

-spec max_freq(Ref :: server_ref(), Amount :: integer(),
               U :: hugin_opts:time_unit())
              -> ok | {error, Reason :: any()}.

%% @doc An option to limit the frequency of calls that Hugin makes.
%% The default option is to have no frequency limits.
%% However, Hugin still limits the amount of simultaneous outgoing connections
%% to parallel sessions by default.
%% See {@link max_par/2} for more information.
%% @equiv max_freq(ServerRef, Amount, 1, U)
%% @see hugin_opts:max_freq/3
max_freq(ServerRef, Amount, Unit) ->
  max_freq(ServerRef, Amount, 1, Unit).


%% @equiv set_option(ServerRef, hugin_opts:max_freq(ServerRef, Amount, 1, U))
%% @see hugin_opts:max_freq/4
max_freq(ServerRef, Amount, N, Unit) ->
  set_option(ServerRef, hugin_opts:max_freq(Amount, N, Unit)).

%% @doc An option to limit the amount of simultaneous parallel outgoing
%% connections that Hugin will keep. The default is 5.
max_par(ServerRef, N) ->
  set_option(ServerRef, hugin_opts:max_par(N)).

-spec set_option(ServerRef :: server_ref(), Option :: hugin_opts:opt())
                -> ok | {error, Reason :: any()}.

%% @doc Immediately update the behavior of the Hugin server.
%% @equiv set_options(ServerRef, [Option])
set_option(ServerRef, O) ->
  set_options(ServerRef, [O]).

-spec set_options(ServerRef :: server_ref(), Options :: [ hugin_opts:opt() ])
                 -> ok | {error, Reason :: any()}.

%% @doc Immediately update the behavior of the Hugin server.
set_options(ServerRef, Options) ->
  hugin_server:set_options(ServerRef, Options).


%%%=========================================================================
%%%  Private Functions
%%%=========================================================================

supervisor_id(F) when is_function(F) ->
  supervisor_id(erlang:ref_to_list(make_ref()));

supervisor_id(Module) ->
  list_to_atom( lists:concat([Module, "_sup"])).
