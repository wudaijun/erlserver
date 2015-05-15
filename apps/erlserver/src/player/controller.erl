-module(controller).
-compile([export_all]).

-callback(init_callback(term()) -> ok).
