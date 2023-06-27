%% -*- mode: nitrogen -*-
-module(cluster_cmd).
-export([main/0, panel/0, event/1]).
-include_lib("wf.hrl").
-include("master.hrl").

main() -> common:try_template("./templates/cluster_cmd.html").

panel() ->
    Func = fun(N, Acc) ->
        T = lib_cluster:get_nodetype(N),
        case lists:member(T, Acc) of
            true -> Acc;
            false -> [T | Acc]
        end
    end,
    NodeTypeList = [Type || Type <- lists:foldl(Func, [], nodes:get_nodes())],
    NodeTypeOptions = [#option{text = Type, value = Type} || Type <- NodeTypeList],
    [ChoiceNodeType | _] = NodeTypeList,
    NodeList = nodes:get_nodes(ChoiceNodeType),
    NodeOptions = [#option{text = X, value = X} || X <- NodeList],
    TargetOptions =
        case ChoiceNodeType of
            gamer -> [#option{text = "节点", value = "1"}, #option{text = "玩家", value = "2"}];
            _ -> [#option{text = "节点", value = "1"}]
        end,
    Panel = [
        #table{rows = [
            #tablerow{cells = [
                #tablecell{text = "选择类型：", style = "width:150px"},
                #tablecell{id = ref_nodetype, body = #dropdown{options = NodeTypeOptions, postback = dropdown_changed_nodetype}}
            ]},
            #tablerow{cells = [
                #tablecell{text = "选择节点：", style = "width:150px"},
                #tablecell{id = ref_node, body = #dropdown{options = NodeOptions}}
            ]},
            #tablerow{cells = [
                #tablecell{text = "作用对象：", style = "width:150px"},
                #tablecell{id = ref_target, body = #dropdown{options = TargetOptions, postback = dropdown_changed_target}}
            ]},
            #tablerow{id = ref_rowplayerid, cells = [
                #tablecell{text = "角色ID：", style = "width:150px"},
                #tablecell{body = [
                    #textbox{id = ref_playerid, style = "width:150px", text = ""},
                    #strong{text = "请谨慎使用该功能!"}
                ]}
            ]},
            #tablerow{cells = [
                #tablecell{text = "远程命令：", style = "width:150px"},
                #tablecell{body = #textarea{id = ref_cmd, style = "width:850px; height:200px;", text = "", disabled = false}}
            ]},
            #tablerow{cells = [
                #tablecell{text = "", style = "width:150px"},
                #tablecell{body = #button{text = "执行", postback = btn_cmd_exec}}
            ]},
            #tablerow{id = ref_row_res, cells = [
                #tablecell{text = "执行结果", style = "width:150px"},
                #tablecell{body = #label{id = ref_res, style = "width:850px; height:200px;", text = ""}}
            ]}
        ]}
    ],
    wf:wire(ref_row_res, #hide{}),
    wf:wire(ref_rowplayerid, #hide{}),
    Panel.

event(dropdown_changed_nodetype) ->
    NodeType = lib_base:try_to_atom(wf:q(ref_nodetype)),
    NodeOptions = [#option{text = X, value = X} || X <- nodes:get_nodes(NodeType)],
    wf:replace(ref_node, #tablecell{id = ref_node, body = #dropdown{options = NodeOptions}}),
    TargetOptions =
        case NodeType of
            gamer -> [#option{text = "节点", value = "1"}, #option{text = "玩家", value = "2"}, #option{text = "区服", value = "3"}, #option{text = "公会", value = "4"}, #option{text = "活动", value = "5"}];
            _ -> [#option{text = "节点", value = "1"}]
        end,
    wf:replace(ref_target, #tablecell{id = ref_target, body = #dropdown{options = TargetOptions, postback = dropdown_changed_target}}),
    event(dropdown_changed_target);

event(dropdown_changed_target) ->
    wf:replace(ref_playerid, #textbox{id = ref_playerid, style = "width:150px", text = ""}),
    Target = wf:q(ref_target),
    NodeType = lib_base:try_to_atom(wf:q(ref_nodetype)),
    wf:wire(ref_rowplayerid,
        case {NodeType, Target} of
            {gamer, _} when Target /= "1" -> #show{};
            _ -> #hide{}
        end);

event(btn_cmd_exec) ->
    Node = lib_base:try_to_atom(wf:q(ref_node)),
    Target = wf:q(ref_target),
    RoleIdStr = wf:q(ref_playerid),
    Cmd = wf:q(ref_cmd),
    case Cmd of
        "" ->
            wf:wire(#alert{text = "不可执行空命令"});
        _ ->
            wf:wire(ref_row_res, #show{}),
            case Target of
                "1" ->  %% 节点
                    case rpc:call(Node, lib_base, exec, [Cmd]) of
                        {badrpc, Reason} ->
                            wf:set(ref_res, wf:f("badrpc reason: ~p", [Reason]));
                        Result ->
                            wf:set(ref_res, wf:f("~p", [Result]))
                    end;
                "2" ->  %% 玩家
                    PlayerId =
                        case length(RoleIdStr) =< 9 andalso (lists:all(fun(X) -> X >= $0 andalso X =< $9 end, RoleIdStr)) of
                            true -> lib_base:to_integer(RoleIdStr);
                            false ->
                                case lib_base:is_address(list_to_binary(RoleIdStr)) of
                                    true -> lib_base:to_address(list_to_binary(RoleIdStr));
                                    false -> error
                                end
                        end,
                    case PlayerId of
                        error ->
                            wf:wire(#alert{text = "玩家id填写错误"});
                        _ ->
                            case rpc:call(Node, gamer_lib_role, sync_to_role, [{1, PlayerId}, {gamer_mod_common, {exec, Cmd}}]) of
                                {badrpc, Reason} ->
                                    wf:set(ref_res, wf:f("badrpc reason: ~p", [Reason]));
                                Result ->
                                    wf:set(ref_res, wf:f("~p", [Result]))
                            end
                    end;
                "3" ->  %% 区服
                   ServerId =
                        case length(RoleIdStr) =< 9 andalso (lists:all(fun(X) -> X >= $0 andalso X =< $9 end, RoleIdStr)) of
                            true -> lib_base:to_integer(RoleIdStr);
                            false -> error
                        end,
                    case ServerId of
                        error ->
                            wf:wire(#alert{text = "区服id填写错误"});
                        _ ->
                            case rpc:call(Node, gamer_lib_role, sync_to_role, [{0, ServerId}, {service_mod_attr, {exec, Cmd}}]) of
                                {badrpc, Reason} ->
                                    wf:set(ref_res, wf:f("badrpc reason: ~p", [Reason]));
                                Result ->
                                    wf:set(ref_res, wf:f("~p", [Result]))
                            end
                    end;
                "4" ->  %% 公会
                    GuildId =
                        case length(RoleIdStr) =< 20 andalso (lists:all(fun(X) -> X >= $0 andalso X =< $9 end, RoleIdStr)) of
                            true -> lib_base:to_integer(RoleIdStr);
                            false -> error
                        end,
                    case GuildId of
                        error ->
                            wf:wire(#alert{text = "公会id填写错误"});
                        _ ->
                            case rpc:call(Node, gamer_lib_role, sync_to_role, [{3, GuildId}, {guild_mod_attr, {exec, Cmd}}]) of
                                {badrpc, Reason} ->
                                    wf:set(ref_res, wf:f("badrpc reason: ~p", [Reason]));
                                Result ->
                                    wf:set(ref_res, wf:f("~p", [Result]))
                            end
                    end;
                "5" ->  %% 活动
                    ActivityId =
                        try
                            {Type, Key} = lib_base:string_to_term(RoleIdStr),
                            case is_integer(Type) andalso is_integer(Key) of
                                true -> {Type, Key};
                                false -> error
                            end
                        catch
                            _:_ -> error
                        end,
                    case ActivityId of
                        error ->
                            wf:wire(#alert{text = "活动id填写错误"});
                        _ ->
                            case rpc:call(Node, gamer_lib_role, sync_to_role, [{2, ActivityId}, {activity_mod_attr, {exec, Cmd}}]) of
                                {badrpc, Reason} ->
                                    wf:set(ref_res, wf:f("badrpc reason: ~p", [Reason]));
                                Result ->
                                    wf:set(ref_res, wf:f("~p", [Result]))
                            end
                    end
            end
    end;



event(Event) ->
    ?INFO_LOG("Event:~p", [Event]),
    wf:wire(#alert{text = term_to_binary(Event)}).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


