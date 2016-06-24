%% @doc
%%   rest api - entity key / val
-module(hercule_entity).

-export([
   allowed_methods/1,
   content_provided/1, 
   'GET'/3
]).

%%
allowed_methods(_Req) ->
   ['GET'].

%%
content_provided(_Req) ->
   [{application, json}].

%%
'GET'({application, json}, _, {_Url, _Head, Env}) ->
   [{_, Fn, _}] = ets:lookup(hercule, lens:get(lens:pair(<<"id">>), Env)),
   Urn  = lens:get(lens:pair(<<"urn">>), Env),
   Snap = hercule:q(lens:get(lens:pair(<<"ns">>), Env), #{urn => Urn}, Fn),
   {ok, jsx:encode( stream:list(Snap) )}.
