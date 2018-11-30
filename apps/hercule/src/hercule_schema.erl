-module(hercule_schema).
-compile({parse_transform, category}).

-export([
   create/2,
   lookup/1
]).

%%
%%
bucket(Id) ->
   [identity ||
      hercule_config:storage(),
      uri:segments([Id], _)
   ].

%%
%%
create(Id, Schema) ->
   [either ||
      Sock <- esio:socket(bucket(Id), []),
      elasticlog:schema(Sock, encode_schema(Schema)),
      esio:close(Sock),
      cats:unit(Id)
   ].

encode_schema(Schema) ->
   [{Key, semantic:compact(Type)} 
      || {Key, Type} <- maps:to_list(Schema)].

%%
%%
lookup(Id) ->
   [either ||
      Sock <- esio:socket(bucket(Id), []),
      Data <- cats:unit([either ||
         elasticlog:schema(Sock),
         cats:unit(decode_schema(_))
      ]),
      esio:close(Sock),
      cats:flatten(Data)
   ].

decode_schema(Schema) ->
   maps:from_list([{Key, iri(Type)} 
      || {Key, Type} <- maps:to_list(Schema), Type =/= undefined, Key =/= undefined]).

iri({iri, Prefix, Suffix}) ->
   <<Prefix/binary, $:, Suffix/binary>>.

