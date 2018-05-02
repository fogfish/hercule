-module(infusion).

-export([
   stream/2,
   commit/2
]).


stream(Stream, Fact) ->
   clue:inc({hercule, intake}),
   pts:put_(infusion, Stream, Fact, false),
   undefined.

commit(Stream, Bucket) ->
   pts:send(infusion, Stream, {commit, Bucket}).

