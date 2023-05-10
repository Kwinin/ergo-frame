%% complex1.erl
-module(complex1).
-export([start/1, stop/0, init/1]).
-export([foo/1, bar/1]).

start(ExtPrg) ->
  spawn(?MODULE, init, [ExtPrg]).
stop() ->
  complex ! stop.

foo(X) ->
  call_port({foo, X}).
bar(Y) ->
  call_port({bar, Y}).

call_port(Msg) ->
  complex ! {call, self(), Msg},
  receive
    {complex, Result} ->
      Result
  end.

init(ExtPrg) ->
  register(complex, self()),
  process_flag(trap_exit, true),
  %% 注意{packet, 2}代表的是用2个字节表示传输的
  %% Port = open_port({spawn, ExtPrg}, [{packet, 2}]),
  %% 在这里不去处理这个数据头，不需要这个参数
  Port = open_port({spawn, ExtPrg}, []),
  loop(Port).

loop(Port) ->
  receive
    {call, Caller, Msg} ->
      Port ! {self(), {command, encode(Msg)}},
      receive
        {Port, {data, Data}} ->
          Caller ! {complex, decode(Data)}
      end,
      loop(Port);
    stop ->
      Port ! {self(), close},
      receive
        {Port, closed} ->
          exit(normal)
      end;
    {'EXIT', Port, Reason} ->
      exit(port_terminated)
  end.

%% 进行编码，将数据转成2进制
encode({foo, X}) -> <<1:8, X:8>>;
encode({bar, Y}) -> <<2:8, Y:8>>.

%% 这里有点特别，
decode(Data) -> erlang:list_to_binary(Data).