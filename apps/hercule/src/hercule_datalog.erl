%% @doc
%%   rest api: datalog management
-module(hercule_datalog).

-export([
   allowed_methods/1,
   content_provided/1, 
   content_accepted/1,
   'GET'/3,
   'PUT'/3
]).

%%
allowed_methods(_Req) ->
   ['PUT', 'GET'].

%%
content_provided(_Req) ->
   [{text, datalog}].

%%
content_accepted(_Req) ->
   [{text, datalog}].


'PUT'({text, datalog}, Datalog, {_Url, _Head, Env}) ->
   Id = lens:get(lens:pair(<<"id">>), Env),
   Fn = elasticlog:c( scalar:c(Datalog) ),
   ets:insert(hercule, {Id, Fn, Datalog}),
   ok.

'GET'({text, datalog}, _, {_Url, _Head, Env}) ->
   Id = lens:get(lens:pair(<<"id">>), Env),
   [{_, _, Datalog}] = ets:lookup(hercule, Id),   
   {ok, Datalog}.


% %%
% %%
% 'POST'(_Type, Datalog, {_Url, _Head, Env}) ->
%    Ns = lens:get(lens:pair(<<"ns">>), Env),
%    {200, jsx:encode( stream:list( hercule:q(Ns, Datalog) ) )}.
   