-module(msg_dispatcher).

-export([dispatch/1]).

%% dispatch msgid to callback. like message registion
dispatch(1) -> {module1, cb1};
dispatch(2) -> {module2, cb2};
dispatch(3) -> {module3, cb3}.
