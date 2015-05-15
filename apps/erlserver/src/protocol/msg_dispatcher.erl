-module(msg_dispatcher).

-export([dispatch/1]).

%% dispatch msgid to callback. like message registion
dispatch(1) -> {module1, save_name};
dispatch(2) -> {module2, get_name};
dispatch(3) -> {module3, cb3};
dispatch(_) -> {error, not_find_callback}.
