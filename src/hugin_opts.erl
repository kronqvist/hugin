-module(hugin_opts).

-export([restraint_connections/2, restraint_connections/3]).

-type option() :: {term(), term()}.
-type time_unit() :: ms | millisecond   | milliseconds
                   | s  | sec  | second | seconds
                   | m  | min  | minute | minutes
                   | h  | hour | hours
                   | d  | day  | days
                   | w  | week | weeks.


%% API

-spec restraint_connections(Amount :: integer(), TimeUnit :: time_unit())
                           -> option().

restraint_connections(Amount, TimeUnit) ->
  restraint_connections(Amount, 1, TimeUnit).

-spec restraint_connections(Amount :: integer(), Every :: integer(),
                            TimeUnit :: time_unit()) -> option().

restraint_connections(Amount, Every, TimeUnit) when is_integer(Amount),
                                                    is_integer(Every),
                                                    is_atom(TimeUnit) ->
  Milliseconds = milliseconds(TimeUnit) * Every,
  {restraint, Amount, Milliseconds}.


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
