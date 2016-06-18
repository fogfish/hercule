%% @doc
%%   web-socket live stream resource
-module(hercule_live).

-export([
   allowed_methods/1,
   content_provided/1, 
   recv/3,
   send/3
]).

%%
allowed_methods(_Req) ->
   ['GET'].

%%
content_provided(_Req) ->
   [{application, json}].

%%
%%
recv(_Type, Json, {_Url, _Head, Env}) ->
   [{_, Fn, _}] = ets:lookup(hercule, lens:get(lens:pair(<<"id">>), Env)),
   Heap = jsx:decode(Json, [return_maps, {labels, atom}]), %% @todo: use existing_atom | attempt_atom
   Pid  = self(),
   spawn(fun() ->
      stream:foreach(
         fun(X) ->
            Pid ! jsx:encode(X)
         end,
         hercule:q(lens:get(lens:pair(<<"ns">>), Env), Heap, Fn)
      ),
      Pid ! eof
   end),
   ok.

%%
send(_Type, eof, {_Url, _Head, _Env}) ->
   eof;

send(_Type, Msg, {_Url, _Head, _Env}) ->
   {ok, Msg}.
