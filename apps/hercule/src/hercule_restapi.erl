-module(hercule_restapi).
-compile({parse_transform, category}).

-export([endpoints/0]).

%%
%%
endpoints() ->
   [
      cors(),
      % connect(),
      bucket(),
      schema(),

      stream(),
      commit(),

      deduct()
   ].

cors() ->
   [reader ||
         _ /= restd:method('OPTIONS'),
      Head /= restd:cors([
         {<<"Access-Control-Allow-Methods">>, <<"GET, PUT, POST, DELETE, OPTIONS">>}
        ,{<<"Access-Control-Allow-Headers">>, <<"Content-Type, Authorization, Accept">>}
        ,{<<"Access-Control-Max-Age">>,       600}
      ]),
         _ /= restd:to_text(200, Head, <<" ">>)
   ].


%%
%%
bucket() ->
   [reader ||
      Path   /= restd:path("/buckets/:id"),
      Bucket /= cats:optionT({badkey, bucket}, lens:get(lens:pair(<<"id">>), Path)),
           _ /= restd:method('PUT'),
           _ /= restd:accepted_content({application, json}),   
      Schema /= restd:as_json(),

      cats:unit(hercule_bucket:create(Bucket, Schema)),

      Http   /= restd:to_json(_),
           _ /= restd:accesslog(Http)
   ].

schema() ->
   [reader ||
        Path /= restd:path("/buckets/:id"),
      Bucket /= cats:optionT({badkey, bucket}, lens:get(lens:pair(<<"id">>), Path)),
           _ /= restd:method('GET'),
           _ /= restd:provided_content({application, json}),

      Head /= restd:cors(),

      cats:unit( hercule_bucket:schema(Bucket) ),

      Http /= restd:to_json(Head, _),
      _ /= restd:accesslog(Http)
   ].


%%
%%
stream() ->
   [reader || 
      Path   /= restd:path("/streams/:id"),
      Stream /= cats:optionT({badkey, stream}, lens:get(lens:pair(<<"id">>), Path)),
      Packet /= restd:as_text(),
      cats:unit( infusion:stream(Stream, Packet) )
   ].


%%
%%
commit() ->
   [reader ||
      Path   /= restd:path("/streams/:id/commit/:bucket"),
      Stream /= cats:optionT({badkey, stream}, lens:get(lens:pair(<<"id">>), Path)),
      Bucket /= cats:optionT({badkey, bucket}, lens:get(lens:pair(<<"bucket">>), Path)),
           _ /= restd:method('GET'),

      cats:unit( infusion:commit(Stream, Bucket) ),
      Http /= restd:to_json({ok, Bucket}),
      _ /= restd:accesslog(Http)
   ].







connect() ->
   [reader ||
      _ /= restd:url("/_sys/connect"),
      _ /= restd:method('POST'),
      _ /= restd:accepted_content({application, json}),
      _ /= restd:provided_content({application, json}),
      Server /= restd:as_json(),

      hercule_poirot:schema(),

      Http /= restd:to_json(_),
      _ /= restd:accesslog(Http)
   ].


deduct() ->
   [reader ||
      _ /= restd:url("/deduct"),
      _ /= restd:method('POST'),
      _ /= restd:accepted_content({text, plain}),
      _ /= restd:provided_content({application, json}),
      Datalog /= restd:as_text(),
      Head /= restd:cors(),

      cats:unit( hercule_poirot:deduct(Datalog) ),

      Http /= restd:to_json(Head, _),
      _ /= restd:accesslog(Http)
   ].







