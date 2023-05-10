%%%-------------------------------------------------------------------
%%% @author kwinin
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 27. 4月 2023 18:26
%%%-------------------------------------------------------------------
-module(complex2).
-author("kwinin").

%% API
-export([client/0]).

client() ->
  SomeHostInNet = "localhost",
  Port = 8080,
  %% {packet,4}后面的数字代表数据头的字节
  {ok, Sock} = gen_tcp:connect(SomeHostInNet, Port, [binary, {packet, 4}]),
  ok = gen_tcp:send(Sock, "Some Data"),
  ok = gen_tcp:close(Sock).