%% @doc
%%
-module(hercule_q).

-export([
   allowed_methods/1,
   content_provided/1, 
   content_accepted/1,
   'PUT'/3
]).

%%
allowed_methods(_Req) ->
   ['PUT'].

%%
content_provided(_Req) ->
   [{application, json}].

%%
content_accepted(_Req) ->
   [{text, datalog}].


'PUT'({application, json}, Datalog, {Url, _Head, Env}) ->
   Snap = hercule:q(lens:get(lens:pair(<<"ns">>), Env), Datalog),
   case scalar:i( uri:q(<<"n">>, 0, Url) ) of
      0 ->
         {ok, jsx:encode( stream:list(Snap) )};
      N ->
         {ok, jsx:encode( stream:list(N, Snap) )}
   end.
