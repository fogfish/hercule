%% @doc
%%   resource snapshot resource
-module(hercule_snap).

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
   [{application, json}].


'PUT'({application, json}, Json, {_Url, _Head, Env}) ->
   [{_, Fn, _}] = ets:lookup(hercule, lens:get(lens:pair(<<"id">>), Env)),
   Heap = jsx:decode(Json, [return_maps, {labels, atom}]), %% @todo: use existing_atom | attempt_atom
   Snap = hercule:q(lens:get(lens:pair(<<"ns">>), Env), Heap, Fn),
   {ok, jsx:encode( stream:list(Snap) )}.


