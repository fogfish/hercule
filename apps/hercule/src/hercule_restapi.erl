-module(hercule_restapi).
-compile({parse_transform, category}).

-export([filters/0, endpoints/0]).

%%
%%
filters() ->
   [
      restd:cors(),
      restd:compress(deflate),
      restd:accesslog()
   ].

%%
%%
endpoints() ->
   [
      restd:preflight(),
      deduct(),
      entity(),
      knowledge(),

      fact(),
      % @todo: design auth schema
      % stream(),
      % commit(),

      bucket(),
      schema(),

      restd_static:react("/console", hercule, 'hercule-console')
   ].


%%
%%
auth(<<"Bearer ", Jwt/binary>>, _) ->
   [either ||
      %% @todo: permit:include(Jwt, #{<<"aud">> => _, <<"iss">> => _})
      permit:include(Jwt, #{<<"uid">> => true}),
      cats:unit( lens:get(lens:at(<<"sub">>), _) )
   ].



%%-----------------------------------------------------------------------------
%%
%% lookup interface
%%
%%-----------------------------------------------------------------------------

%%
%%
deduct() ->
   [reader ||
      Path   /= restd:path("/buckets/:id/deduct"),
      Bucket /= cats:optionT({badkey, bucket}, lens:get(lens:pair(<<"id">>), Path)),
           _ /= restd:method('POST'),
        User /= restd:authorize(fun auth/2),
           _ /= restd:accepted_content({text, plain}),
           _ /= restd:provided_content({application, json}),
        Opts /= restd:q(),
      Script /= restd:as_text(),

      cats:unit( 
         hercule:deduct(
            User,
            Bucket,
            scalar:i(lens:get(lens:pair(<<"limit">>, 100), Opts)),
            Script
         )
      ),
      _ /= restd:to_json(_)
   ].

%%
%%
entity() ->
   [reader ||
      Path   /= restd:path("/buckets/:id/keys/:key"),
      Bucket /= cats:optionT({badkey, bucket}, lens:get(lens:pair(<<"id">>), Path)),
         Key /= cats:optionT({badkey, key}, lens:get(lens:pair(<<"key">>), Path)),
           _ /= restd:method('GET'),
        User /= restd:authorize(fun auth/2),           
           _ /= restd:provided_content({application, json}),

      cats:unit( hercule:entity(User, Bucket, Key) ),

      _ /= restd:to_json(_)
   ].

%%
%%
knowledge() ->
   [reader ||
      Path   /= restd:path("/buckets/:id/iris/:key"),
      Bucket /= cats:optionT({badkey, bucket}, lens:get(lens:pair(<<"id">>), Path)),
         IRI /= cats:optionT({badkey, key}, lens:get(lens:pair(<<"key">>), Path)),
           _ /= restd:method('GET'),
        User /= restd:authorize(fun auth/2),           
           _ /= restd:provided_content({application, json}),

      cats:unit( hercule:entity(User, Bucket, elasticlog:identity(IRI)) ),

      _ /= restd:to_json(_)
   ].

%%-----------------------------------------------------------------------------
%%
%% intake / write interface
%%
%%-----------------------------------------------------------------------------

%%
%%
fact() ->
   [reader ||
      Path   /= restd:path("/buckets/:id/iris/:key"),
      Bucket /= cats:optionT({badkey, bucket}, lens:get(lens:pair(<<"id">>), Path)),
         IRI /= cats:optionT({badkey, bucket}, lens:get(lens:pair(<<"key">>), Path)),
           _ /= restd:method('POST'),
        User /= restd:authorize(fun auth/2),
           _ /= restd:accepted_content({application, json}),
           _ /= restd:provided_content({application, json}),
        Fact /= restd:as_json(),

      cats:unit( hercule:fact(User, Bucket, elasticlog:identity(IRI), Fact) ),

      _ /= restd:to_json(_)
   ].


%%
%%
% stream() ->
%    [reader || 
%       Path   /= restd:path("/streams/:id"),
%       Stream /= cats:optionT({badkey, stream}, lens:get(lens:pair(<<"id">>), Path)),
%       Packet /= restd:as_text(),
%       cats:unit( infusion:stream(Stream, Packet) )
%    ].


% %%
% %%
% commit() ->
%    [reader ||
%       Path   /= restd:path("/streams/:id/commit/:bucket"),
%       Stream /= cats:optionT({badkey, stream}, lens:get(lens:pair(<<"id">>), Path)),
%       Bucket /= cats:optionT({badkey, bucket}, lens:get(lens:pair(<<"bucket">>), Path)),
%            _ /= restd:method('GET'),

%       cats:unit( infusion:commit(Stream, Bucket) ),
%       _ /= restd:to_json({ok, Bucket})
%    ].


%%-----------------------------------------------------------------------------
%%
%% bucket interface
%%
%%-----------------------------------------------------------------------------

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

           _ /= restd:to_json(_)
   ].

%%
%%
schema() ->
   [reader ||
        Path /= restd:path("/buckets/:id"),
      Bucket /= cats:optionT({badkey, bucket}, lens:get(lens:pair(<<"id">>), Path)),
           _ /= restd:method('GET'),
           _ /= restd:provided_content({application, json}),

      cats:unit( hercule_bucket:schema(Bucket) ),

           _ /= restd:to_json(_)
   ].



