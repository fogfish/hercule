-module(hercule_q).

-export([
   allowed_methods/1,
   content_provided/1, 
   content_accepted/1,
   'POST'/3
]).

%%
allowed_methods(_Req) ->
   ['POST'].

%%
content_provided(_Req) ->
   [{application, json}].

%%
content_accepted(_Req) ->
   [{text, datalog}].

%%
%%
'POST'(_, {_Url, _Head, Env}, Msg) ->
   Id = lens:get(lens:pair(<<"id">>), Env),
   %% @todo: create a pool of connection
   {ok, Sock} = esio:socket(uri:segments([Id], uri:new("http://docker:9200/"))),
   Datalog = elasticlog:c(scalar:c(Msg)),
   {200, jsx:encode( stream:list(Datalog(Sock)) )}.

