%% -*- mode: nitrogen -*-
-module(cluster_node).
-export([main/0, nodes_num/0, wsgate_online_num/0, player_online_num/0, guild_online_num/0, service_online_num/0, activity_online_num/0, ranker_online_num/0]).
-export([nodes/0, event/1]).
-include_lib("wf.hrl").
-include_lib("records.hrl").
-include("master.hrl").

main() -> common:try_template("./templates/cluster_node.html").

nodes_num()->
    integer_to_binary(length(nodes:get_nodes())).

wsgate_online_num() ->
    Total = lists:sum([getWsgateOnline(Node) || Node <- nodes:get_nodes(wsgate)]),
    integer_to_binary(Total).

player_online_num() ->
    Total = lists:sum([getPlayerOnline(Node) || Node <- nodes:get_nodes(gamer)]),
    integer_to_binary(Total).

guild_online_num() ->
    Total = lists:sum([getGuildOnline(Node) || Node <- nodes:get_nodes(gamer)]),
    integer_to_binary(Total).

service_online_num() ->
    Total = lists:sum([getServiceOnline(Node) || Node <- nodes:get_nodes(gamer)]),
    integer_to_binary(Total).

activity_online_num() ->
    Total = lists:sum([getActivityOnline(Node) || Node <- nodes:get_nodes(gamer)]),
    integer_to_binary(Total).

ranker_online_num() ->
    Total = lists:sum([getRankerOnline(Node) || Node <- nodes:get_nodes(gamer)]),
    integer_to_binary(Total).

nodes() ->
%%    {ok, Nodes} = application:get_env(master, nodes),
    CfgNodes = master_lib:get_cfg_master_nodes(),
    Func1 =
        fun({Node, _Sign, ExtIp}) ->
            IsOnline = lists:member(lib_base:try_to_atom(Node), nodes:get_nodes()),
            {OnlineDesc, OnlinePlayerNum, OnlineGuildNum, OnlineServiceNum, OnlineAcitivityNum, OnlineRankerNum, Switch} =
                case IsOnline of
                    true ->
                        case lib_cluster:get_nodetype(Node) of
                            gamer -> {"在线", integer_to_list(getPlayerOnline(Node)), integer_to_list(getGuildOnline(Node)), integer_to_list(getServiceOnline(Node)), integer_to_list(getActivityOnline(Node)), integer_to_list(getRankerOnline(Node)), "关闭"};
                            wsgate ->  {"在线", integer_to_list(getWsgateOnline(Node)), "---", "---", "---", "---", "关闭"};
                            _ -> {"在线", "---", "---", "---", "---", "---", "关闭"}
                        end;
                    false -> {"离线", "---", "---", "---", "---", "---", "建议启动"}
                end,
            Host = lib_cluster:get_nodehost(Node),
            InnerIP =
                case inet:getaddr(Host, inet) of
                    {ok, {IP1, IP2, IP3, IP4}} ->
                        io_lib:format("~p.~p.~p.~p", [IP1, IP2, IP3, IP4]);
                    {error, _Reason} -> "未知IP"
                end,
            [Node, InnerIP, ExtIp, OnlineDesc, OnlinePlayerNum, OnlineGuildNum, OnlineServiceNum, OnlineAcitivityNum, OnlineRankerNum, #button{text = Switch, postback = {clicked, Node, case IsOnline of true -> stop_node; false -> start_node end}}]
        end,
    Func2 =
        fun(Node) ->
            {OnlineDesc, OnlinePlayerNum, OnlineGuildNum, OnlineServiceNum, OnlineAcitivityNum, OnlineRankerNum} =
                case lib_cluster:get_nodetype(Node) of
                    gamer -> {"在线", integer_to_list(getPlayerOnline(Node)), integer_to_list(getGuildOnline(Node)), integer_to_list(getServiceOnline(Node)), integer_to_list(getActivityOnline(Node)), integer_to_list(getRankerOnline(Node))};
                    wsgate -> {"在线", integer_to_list(getWsgateOnline(Node)), "---", "---", "---", "---"};
                    _ -> {"在线", "---", "---", "---", "---", "---"}
                end,
            Host = lib_cluster:get_nodehost(Node),
            InnerIP =
                case inet:getaddr(Host, inet) of
                    {ok, {IP1, IP2, IP3, IP4}} ->
                        io_lib:format("~p.~p.~p.~p", [IP1, IP2, IP3, IP4]);
                    {error, _Reason} -> "未知IP"
                end,
            [Node, InnerIP, "-- 未配置 --", OnlineDesc, OnlinePlayerNum, OnlineGuildNum, OnlineServiceNum, OnlineAcitivityNum, OnlineRankerNum, #button{text = "建议关闭", postback = {clicked, Node, stop_node}}]
        end,
    DataList = lists:map(Func1, CfgNodes) ++ lists:map(Func2, [Node || Node <- nodes:get_nodes(), lists:keyfind(Node, 1, CfgNodes) =:= false]),
    TotalWsgateOnline = lists:sum([lib_base:to_integer(Num) || [Node, _, _, _, Num, _, _, _, _, _] <- DataList, lib_cluster:get_nodetype(Node) =:= wsgate, is_number(Num)]),
    TotalPlayerOnline = lists:sum([lib_base:to_integer(Num) || [Node, _, _, _, Num, _, _, _, _, _] <- DataList, lib_cluster:get_nodetype(Node) =:= gamer, is_number(Num)]),
    TotalGuildOnline = lists:sum([lib_base:to_integer(Num) || [Node, _, _, _, _, Num, _, _, _, _] <- DataList, lib_cluster:get_nodetype(Node) =:= gamer, is_number(Num)]),
    TotalServiceOnline = lists:sum([lib_base:to_integer(Num) || [Node, _, _, _, _, _, Num, _, _, _] <- DataList, lib_cluster:get_nodetype(Node) =:= gamer, is_number(Num)]),
    TotalAcitivtyOnline = lists:sum([lib_base:to_integer(Num) || [Node, _, _, _, _, _, _, Num, _, _] <- DataList, lib_cluster:get_nodetype(Node) =:= gamer, is_number(Num)]),
    TotalRankerOnline = lists:sum([lib_base:to_integer(Num) || [Node, _, _, _, _, _, _, _, Num, _] <- DataList, lib_cluster:get_nodetype(Node) =:= gamer, is_number(Num)]),
    #eDatatable{
        name = io_lib:format("节点管理 （网关总在线：~p， 玩家总在线：~p， 总数公会：~p， 区服总在线：~p， 活动总在线：~p， 排行榜总在线：~p）", [TotalWsgateOnline, TotalPlayerOnline, TotalGuildOnline, TotalServiceOnline, TotalAcitivtyOnline, TotalRankerOnline]),
        headers = ["节点", "内部IP", "外部IP", "状态", "玩家在线", "公会在线", "区服在线", "活动在线", "排行榜在线", "操作"],
        rows = DataList
    }.

event({clicked, Node, stop_node}) ->
    case net_adm:ping(Node) of
        pong ->
            rpc:cast(Node, lib_app, stop, []);
%%            lib_base:sleep(2000),
%%            case net_adm:ping(Node) of
%%                pong -> wf:wire(#alert{text = <<"关闭节点失败"/utf8>>});
%%                pang -> wf:wire(#alert{text = <<"关闭节点成功"/utf8>>})
%%            end;
        pang -> wf:wire(#alert{text = "节点不在线"})
    end;

event({clicked, Node, start_node}) ->
    case net_adm:ping(Node) of
        pang ->
            Nodes = [X || X <- nodes:get_nodes(), lib_cluster:get_nodehost(X) =:= lib_cluster:get_nodehost(Node)],
            case Nodes of
                [] -> wf:wire(#alert{text = "无法启动远程节点(无中转节点)"});
                [H|_] ->
                    rpc:cast(H, lib_cluster, start_node, [Node])
%%                    lib_base:sleep(2000),
%%                    case net_adm:ping(Node) of
%%                        pong ->
%%                            case lists:member(Node, nodes:get_nodes()) of
%%                                true -> wf:wire(#alert{text = <<"启动节点并加入集群成功"/utf8>>});
%%                                false -> wf:wire(#alert{text = <<"启动节点成功, 但加入集群失败"/utf8>>})
%%                            end;
%%                        pang -> wf:wire(#alert{text = <<"启动节点失败"/utf8>>})
%%                    end
            end;
        pong -> wf:wire(#alert{text = "节点已在线"})
    end;

event(Event) ->
    wf:wire(#alert{text = term_to_binary(Event)}).


%%%%%%%%%%%%%%%%%
getWsgateOnline(Node)->
    try
        {_, Cnt, _} = rpc:call(lib_base:try_to_atom(Node), wsgate_manage_server, get_online, []),
        Cnt
    catch
        _:_ -> 0
    end.

getPlayerOnline(Node)->
    try
        [{specs,_},{active,_},{supervisors,_},{workers,Cnt}] = rpc:call(lib_base:try_to_atom(Node), supervisor, count_children, [player_sup]),
        Cnt
    catch
        _:_ -> 0
    end.

getGuildOnline(Node)->
    try
        [{specs,_},{active,_},{supervisors,_},{workers,Cnt}] = rpc:call(lib_base:try_to_atom(Node), supervisor, count_children, [guild_sup]),
        Cnt
    catch
        _:_ -> 0
    end.

getServiceOnline(Node)->
    try
        [{specs,_},{active,_},{supervisors,_},{workers,Cnt}] = rpc:call(lib_base:try_to_atom(Node), supervisor, count_children, [service_sup]),
        Cnt
    catch
        _:_ -> 0
    end.

getActivityOnline(Node)->
    try
        [{specs,_},{active,_},{supervisors,_},{workers,Cnt}] = rpc:call(lib_base:try_to_atom(Node), supervisor, count_children, [activity_sup]),
        Cnt
    catch
        _:_ -> 0
    end.

getRankerOnline(Node)->
    try
        [{specs,_},{active,_},{supervisors,_},{workers,Cnt}] = rpc:call(lib_base:try_to_atom(Node), supervisor, count_children, [ranker_sup]),
        Cnt
    catch
        _:_ -> 0
    end.


