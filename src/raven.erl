-module(raven).

-export([behaviour_info/1]).

%% behaviour
behaviour_info(callbacks) ->
  [{init,0}, {request,3}];

behaviour_info(_) ->
  undefined.
