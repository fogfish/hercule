FROM fogfish/erlang-alpine-rt:20.3

COPY _build/default/rel /rel

ENTRYPOINT spawn-erlang-node hercule
