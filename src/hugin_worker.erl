-module(hugin_worker).

-export([start_link/2]).

start_link(Pid, Url) ->
  Pid = spawn_link( fun() -> start_worker(Pid, Url) end),
  {ok, Pid}.

start_worker(Pid, Url) ->
  try
    Method = get,
    Headers = [ ],
    Payload = <<>>,
    Options = [ ],
    Response = hackney:request(Method, Url, Headers, Payload, Options),
    [ hackney:controlling_process(Ref, Pid)
      || {ok, 200, _, Ref} <- [ Response ] ],

    hugin_server:worker_completed(Pid, Url, Response)
  catch E:R ->
      hugin_server:worker_completed(
        Url, {failed, {E, R, erlang:get_stacktrace()}})
  end.
