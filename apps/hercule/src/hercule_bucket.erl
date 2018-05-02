-module(hercule_bucket).
-compile({parse_transform, category}).

-export([
   create/2,
   schema/1
]).

%%
%%
create(Id, Schema) ->
   [either ||
      Sock <- esio:socket(bucket(Id), []),
      Data <- cats:unit( elasticlog:schema(Sock, encode_schema(Schema))),
      esio:close(Sock),
      cats:flatten(Data)
   ].

%%
%%
schema(Id) ->
   [either ||
      Sock <- esio:socket(bucket(Id), []),
      Data <- cats:unit([either ||
         elasticlog:schema(Sock),
         cats:unit(decode_schema(_))
      ]),
      esio:close(Sock),
      cats:flatten(Data)
   ].

bucket(Id) ->
   [identity ||
      opts:val(storage, hercule),
      uri:new(_),
      uri:segments([Id], _)
   ].

encode_schema(Schema) ->
   [{semantic:compact(Key), semantic:compact(Type)} 
      || {Key, Type} <- maps:to_list(Schema)].


decode_schema(Schema) ->
   maps:from_list([{iri(Key), iri(Type)} 
      || {Key, Type} <- Schema, Type =/= undefined, Key =/= undefined]).

iri({iri, Prefix, Suffix}) ->
   <<Prefix/binary, $:, Suffix/binary>>.

