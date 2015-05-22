%% =====================================================================
%% @doc An abstraction library providing an interface to the possible
%% options supported by hugin. The values returned from the functions
%% in this library can be returned in the hugin init/0 callback
%% function, or be used in the hugin API function set_option/1 and
%% set_options/1. Calling the functions in this library DOES NOTHING
%% MORE THAN RETURNING VALUES, so don't try to use them to directly
%% influence the behavior of the hugin server.
%%
%% If you want to directly influence the behavior of the server you can
%% use the corresponding functions in the hugin module. See
%% {@link hugin}.
%% @copyright 2015 Magnus Kronqvist
%% @author Magnus Kronqvist <magnus.kronqvist@gmail.com>
%% @version {@version}
%% @end
%% =====================================================================

-module(hugin_opts).

-export([max_freq/2, max_freq/3,
         max_par/1]).


-opaque opt() :: {atom(), any()}.
-type time_unit() ::
        ms | millisecond   | milliseconds
      | s  | sec  | second | seconds
      | m  | min  | minute | minutes
      | h  | hour | hours
      | d  | day  | days
      | w  | week | weeks.


-export_type([opt/0, time_unit/0]).

%% API

%% @doc An option to limit the amount of calls that hugin makes per
%% time unit. The default option is to have no limits. However, notice
%% that hugin still limits the amount of parallel connections to five
%% by default. See {@link max_par/1}.
%%
%% @equiv max_freq(Amount, 1, Unit)

-spec max_freq(Amount :: integer(), Unit :: time_unit()) -> opt().

max_freq(A, U) ->
  max_freq(A, 1, U).

%% @doc Same as max_freq/2 but allows one more argument to specify how many
%%      calls per N time units.

-spec max_freq(Amount :: integer(), N :: integer(), Unit :: time_unit())
              -> opt().

max_freq(A, N, U) when is_integer(A), is_integer(N), is_atom(U) ->
  Ms = milliseconds(U) * N,
  {max_freq, {A, Ms}}.

%% @doc An option to limit the amount of parallel connections allowed
%%      by hugin.

-spec max_par(N :: integer()) -> opt().

max_par(N) when is_integer(N) ->
  {max_par, N}.


%% internal functions

milliseconds(M) ->
  case M of
    ms           -> 1;
    millisecond  -> milliseconds(ms);
    milliseconds -> milliseconds(ms);
    s            -> 1000 * milliseconds(ms);
    sec          -> milliseconds(s);
    second       -> milliseconds(s);
    seconds      -> milliseconds(s);
    m            -> 60 * milliseconds(s);
    min          -> milliseconds(m);
    minute       -> milliseconds(m);
    minutes      -> milliseconds(m);
    h            -> 60 * milliseconds(m);
    hour         -> milliseconds(h);
    hours        -> milliseconds(h);
    d            -> 24 * milliseconds(h);
    day          -> milliseconds(d);
    days         -> milliseconds(d);
    w            -> 7 * milliseconds(d);
    week         -> milliseconds(w);
    weeks        -> milliseconds(w);
    _            -> erlang:error(badarg)
  end.
