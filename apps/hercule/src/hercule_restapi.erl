-module(hercule_restapi).
-compile({parse_transform, category}).

-export([filters/0, endpoints/0]).

%%
%%
filters() ->
   [
      restd:cors(),
      % restd:compress(deflate),
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

      restd_static:react_env_js("/hercule", config()),
      restd_static:react("/hercule", hercule, 'hercule-console')
   ].


%%
%%
auth(<<"Bearer ", Jwt/binary>>, _) ->
   [either ||
      %% @todo: permit:include(Jwt, #{<<"aud">> => _, <<"iss">> => _})
      permit:include(Jwt, #{<<"uid">> => true}),
      cats:unit( lens:get(lens:at(<<"sub">>), _) )
   ].

%%
%%
config() ->
   #{
      'OAUTH2_AUTHORIZE' => typecast:s(opts:val(oauth2_authorize, permit))
   ,  'OAUTH2_TOKEN' => typecast:s(opts:val(oauth2_token, permit))
   ,  'OAUTH2_CLIENT_ID' => typecast:s(opts:val(oauth2_client_id, permit))
   ,  'OAUTH2_FLOW_TYPE' => typecast:s(opts:val(oauth2_flow_type, permit))
   }.

%%-----------------------------------------------------------------------------
%%
%% lookup interface
%%
%%-----------------------------------------------------------------------------

%%
%%
deduct() ->
   [reader ||
      Path   /= restd:path("/hercule/deduct"),
           _ /= restd:method('POST'),
       Owner /= restd:authorize(fun auth/2),
           _ /= restd:accepted_content({text, plain}),
           _ /= restd:provided_content({application, json}),
      Script /= restd:as_text(),

      cats:unit( hercule:deduct(Owner, Script) ),
      _ /= restd:to_json(_)
   ].

%%
%%
entity() ->
   [reader ||
      Path   /= restd:path("/hercule/buckets/:id/keys/:key"),
      Bucket /= cats:optionT({badkey, bucket}, lens:get(lens:pair(<<"id">>), Path)),
         Key /= cats:optionT({badkey, key}, lens:get(lens:pair(<<"key">>), Path)),
           _ /= restd:method('GET'),
       Owner /= restd:authorize(fun auth/2),
           _ /= restd:provided_content({application, json}),

      cats:unit( hercule:entity(Owner, Bucket, Key) ),

      _ /= restd:to_json(_)
   ].

%%
%%
knowledge() ->
   [reader ||
      Path   /= restd:path("/hercule/buckets/:id/iris/:key"),
      Bucket /= cats:optionT({badkey, bucket}, lens:get(lens:pair(<<"id">>), Path)),
         IRI /= cats:optionT({badkey, key}, lens:get(lens:pair(<<"key">>), Path)),
           _ /= restd:method('GET'),
       Owner /= restd:authorize(fun auth/2),
           _ /= restd:provided_content({application, json}),

      cats:unit( hercule:entity(Owner, Bucket, elasticlog:identity(IRI)) ),

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
       Owner /= restd:authorize(fun auth/2),
           _ /= restd:accepted_content({application, json}),
           _ /= restd:provided_content({application, json}),
        Fact /= restd:as_json(),

      cats:unit( hercule:fact(Owner, Bucket, elasticlog:identity(IRI), Fact) ),

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
      Path   /= restd:path("/hercule/buckets/:id"),
      Bucket /= cats:optionT({badkey, bucket}, lens:get(lens:pair(<<"id">>), Path)),
           _ /= restd:method('PUT'),
       Owner /= restd:authorize(fun auth/2),
           _ /= restd:accepted_content({application, json}),
      Schema /= restd:as_json(),

      cats:unit(hercule_schema:create(Bucket, Schema)),

           _ /= restd:to_json(_)
   ].

%%
%%
schema() ->
   [reader ||
        Path /= restd:path("/hercule/buckets/:id"),
      Bucket /= cats:optionT({badkey, bucket}, lens:get(lens:pair(<<"id">>), Path)),
           _ /= restd:method('GET'),
       Owner /= restd:authorize(fun auth/2),
           _ /= restd:provided_content({application, json}),

      cats:unit( hercule_schema:lookup(Bucket) ),
           _ /= restd:to_json(_)
   ].



