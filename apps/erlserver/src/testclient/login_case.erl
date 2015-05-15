-module(login_case).

-compile([export_all]).

run() ->
  {ok, ClientA} = client:start(),
  client:send(ClientA, <<0, 198>>),

  timer:sleep(3000),

  {ok, ClientB} = client:start(),
  client:send(ClientB, <<0, 198>>).
