%% -*- mode: nitrogen -*-
-module(cluster_machine).
-export([main/0, machines_num/0, machines/0, event/1]).
-include_lib("wf.hrl").
-include_lib("records.hrl").
-include("master.hrl").

main() -> common:try_template("./templates/cluster_machine.html").

machines_num()->
    integer_to_binary(length(get_machine_list())).

machines() ->
    Machines = get_machine_list(),
    OnlineNodes = nodes:get_nodes(),
    Func =
        fun({Host, InnerIP, ExtIp, NodeList}) ->
            Fun2 =
                fun(Node, {OriNode, MachineNodesDesc}) ->
                    case lists:member(Node, OnlineNodes) of
                        true -> {Node, MachineNodesDesc ++ [io_lib:format("~p~ts", [lib_cluster:get_nodetype(Node), <<"(在线)"/utf8>>])]};
                        false -> {OriNode, MachineNodesDesc ++ [io_lib:format("~p~ts", [lib_cluster:get_nodetype(Node), <<"(离线)"/utf8>>])]}
                    end
                end,
            {OnlineNode, OnlineDesc} = lists:foldl(Fun2, {undefined, []}, NodeList),
            NodeListDesc = lists:join("、", OnlineDesc),
            {CpuDesc, MemDesc} =
                case OnlineNode of
                    undefined -> {"机器离线", "机器离线"};
                    _ ->
                        MapMachine = rpc:call(OnlineNode, lib_base, get_machine_info, []),
                        MapCpu = maps:get(cpu, MapMachine),
                        Cpu1 = maps:get(1, MapCpu),
                        Cpu5 = maps:get(5, MapCpu),
                        Cpu15 = maps:get(15, MapCpu),
                        MapMem = maps:get(mem, MapMachine),
                        MemTotal = maps:get(total, MapMem),
                        MemUsed = maps:get(used, MapMem),
                        {io_lib:format("~p/~p/~p", [Cpu1, Cpu5, Cpu15]), io_lib:format("~p%  (total:~p, used:~p)", [round(MemUsed*100/MemTotal), MemTotal, MemUsed])}
                end,
            [Host, InnerIP, ExtIp, length(NodeList), NodeListDesc, CpuDesc, MemDesc]
        end,
    #eDatatable{
        name = "机器管理",
        headers = ["机器名", "内部IP", "外部IP", "节点数量", "节点列表", "CPU负载", "内存负载"],
        rows = lists:map(Func, Machines)
    }.

event(Event) ->
    wf:wire(#alert{text = term_to_binary(Event)}).

%%%%%
get_machine_list() ->
    Fun =
        fun({Node, _Sign, ExtIp}, Acc) ->
            Host = lib_cluster:get_nodehost(Node),

            case lists:keyfind(Host, 1, Acc) of
                false ->
                    InnerIP =
                        case inet:getaddr(Host, inet) of
                            {ok, {IP1, IP2, IP3, IP4}} -> io_lib:format("~p.~p.~p.~p", [IP1, IP2, IP3, IP4]);
                            {error, _Reason} -> "未知IP"
                        end,
                    Acc ++ [{Host, InnerIP, ExtIp, [Node]}];
                {Host, InnerIP, ExtIp, OriNodeList} ->
                    lists:keyreplace(Host, 1, Acc, {Host, InnerIP, ExtIp, OriNodeList ++ [Node]})
            end
        end,
    Machines = lists:foldl(Fun, [], master_lib:get_cfg_master_nodes()),
    Machines.


