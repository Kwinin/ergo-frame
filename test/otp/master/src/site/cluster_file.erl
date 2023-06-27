%% -*- mode: nitrogen -*-
-module(cluster_file).
-export([main/0, file/0, event/1, finish_upload_event/4]).
-include_lib("wf.hrl").
-include("master.hrl").

main() -> common:try_template("./templates/cluster_file.html").

file() ->
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
    [Node | _] = NodeList,
    NodeOptions = [#option{text = X, value = X} || X <- NodeList],
    Html = [
        #table{rows = [
            #tablerow{cells = [
                #tablecell{text = "选择类型：", style = "width:150px"},
                #tablecell{id = ref_nodetype, body = #dropdown{options = NodeTypeOptions, postback = dropdown_changed_nodetype}}
            ]},
            #tablerow{cells = [
                #tablecell{text = "选择节点：", style = "width:150px"},
                #tablecell{id = ref_node, body = #dropdown{options = NodeOptions, postback = dropdown_changed_node}}
            ]},
            #tablerow{cells = [
                #tablecell{text = "上传文件：", style = "width:150px"},
                #tablecell{id = ref_upload, body = #upload{tag = tag_upload, droppable = true, show_button = false, multiple = true, file_text = "选择", droppable_text = "拖拽到此处", overall_progress = false}}
            ]},
            #tablerow{cells = [
                #tablecell{text = "选择路径：", style = "width:150px"},
                #tablecell{body = [
                    #button{text = "后退", postback = btn_click_back},
                    #textbox{id = ref_dir, text = "", disabled = true},
                    #button{text = "刷新", postback = btn_click_browse},
                    #button{id = ref_btn_deps, text = "前往依赖库", postback = btn_click_deps},
                    #table{id = ref_dirdetail, class = tiny, rows = []}
                ]}
            ]},
            #tablerow{id = ref_row_filecontent, cells = [
                #tablecell{text = "文件内容：", style = "width:150px"},
                #tablecell{body = #textarea{id = ref_filecontent, style = "width:850px; height:500px;", text = "", disabled = true}}
            ]}
        ]},
        #flash{}
    ],
    wf:wire(ref_row_filecontent, #hide{}),
    browse_dir(Node, ""),
    Html.

event(dropdown_changed_nodetype) ->
    wf:wire(ref_row_filecontent, #hide{}),
    NodeType = list_to_atom(wf:q(ref_nodetype)),
    Nodes = nodes:get_nodes(NodeType),
    [Node | _] = Nodes,
    NodeOptions = [#option{text = X, value = X} || X <- Nodes],
    wf:replace(ref_node, #tablecell{id = ref_node, body = #dropdown{options = NodeOptions, postback = dropdown_changed_node}}),
    browse_dir(Node, ""),
    wf:replace(ref_btn_deps, #button{id = ref_btn_deps, text = "前往依赖库", postback = btn_click_deps});

event(dropdown_changed_node) ->
    wf:wire(ref_row_filecontent, #hide{}),
    Node = list_to_atom(wf:q(ref_node)),
    browse_dir(Node, ""),
    wf:replace(ref_btn_deps, #button{id = ref_btn_deps, text = "前往依赖库", postback = btn_click_deps});

event(btn_click_browse) ->
    wf:wire(ref_row_filecontent, #hide{}),
    Node = list_to_atom(wf:q(ref_node)),
    Dir = wf:q(ref_dir),
    browse_dir(Node, Dir);

event(btn_click_deps) ->
    wf:wire(ref_row_filecontent, #hide{}),
    Node = list_to_atom(wf:q(ref_node)),
    Dir = wf:q(ref_dir),
    {NewText, NewDir} = case Dir of "../deps/" ++ _ -> {"前往依赖库", ""}; _ -> {"返回游戏库", "../deps/"} end,
    wf:replace(ref_btn_deps, #button{id = ref_btn_deps, text = NewText, postback = btn_click_deps}),
    browse_dir(Node, NewDir);

event(btn_click_back) ->
    wf:wire(ref_row_filecontent, #hide{}),
    Node = list_to_atom(wf:q(ref_node)),
    Dir = wf:q(ref_dir),
    case Dir of
        "" ->
            wf:wire(#alert{text = <<"顶级目录不可后退"/utf8>>});
        "../deps/" ->
            wf:wire(#alert{text = <<"顶级目录不可后退"/utf8>>});
        _ ->
            List = string:tokens(Dir, "/"),
            NewDir =
                case string:join(lists:sublist(List, 1, length(List) - 1), "/") of
                    "" -> "";
                    Dir1 -> Dir1 ++ "/"
                end,
            browse_dir(Node, NewDir)
    end;

event({opendir, DirName}) ->
    wf:wire(ref_row_filecontent, #hide{}),
    Node = list_to_atom(wf:q(ref_node)),
    Dir = wf:q(ref_dir),
    NewDir = Dir ++ lib_base:to_list(DirName) ++ "/",
    browse_dir(Node, NewDir);

event({openfile, FileName}) ->
    wf:wire(ref_row_filecontent, #show{}),
    Node = list_to_atom(wf:q(ref_node)),
    Dir = wf:q(ref_dir),
    case lists:reverse(lib_base:to_list(FileName)) of
        "maeb." ++ ModuleName0 ->
            ModuleName = list_to_atom(lists:reverse(ModuleName0)),
            case rpc:call(Node, lib_base, dec_beam, [ModuleName]) of
                {badrpc, nodedown} ->
                    wf:wire(#alert{text = <<"节点不存在"/utf8>>});
                {error, _} ->
                    wf:wire(#alert{text = io_lib:format("模块不存在:~p", [ModuleName])});
                Content ->
                    wf:set(ref_filecontent, Content)
            end;
        _ ->
            DirFileName = Dir ++ lib_base:to_list(FileName),
            case rpc:call(Node, file, read_file, [DirFileName]) of
                {badrpc, nodedown} ->
                    wf:wire(#alert{text = <<"节点不存在"/utf8>>});
                {error, _} ->
                    wf:wire(#alert{text = io_lib:format("文件不存在:~p", [DirFileName])});
                {ok, ContentBin} ->
                    wf:set(ref_filecontent, ContentBin)
            end
    end;

event({del, Type, FileName}) ->
    wf:wire(ref_row_filecontent, #hide{}),
    Node = list_to_atom(wf:q(ref_node)),
    Dir = wf:q(ref_dir),
    DirFileName = Dir ++ lib_base:to_list(FileName),
    case Type of
        file ->
            case rpc:call(Node, file, delete, [DirFileName]) of
                {badrpc, nodedown} ->
                    wf:wire(#alert{text = <<"节点不存在"/utf8>>});
                {error, _} ->
                    wf:wire(#alert{text = io_lib:format("文件不存在:~p", [DirFileName])});
                ok -> ok
            end;
        dir ->
            case rpc:call(Node, file, del_dir, [DirFileName]) of
                {badrpc, nodedown} ->
                    wf:wire(#alert{text = <<"节点不存在"/utf8>>});
                {error, _} ->
                    wf:wire(#alert{text = io_lib:format("文件不存在:~p", [DirFileName])});
                ok -> ok
            end
    end,
    browse_dir(Node, Dir);

event(Event) ->
    ?INFO_LOG("Event:~p", [Event]),
    wf:wire(#alert{text = term_to_binary(Event)}).

finish_upload_event(_Tag, undefined, _, _) ->
    wf:flash("Please select a file."),
    ok;

finish_upload_event(tag_upload, FileName, LocalFileName, _LocalNode) ->
    NodeType = list_to_atom(wf:q(ref_nodetype)),
    Node = list_to_atom(wf:q(ref_node)),
    Dir = wf:q(ref_dir),
    Nodes = nodes:get_nodes(NodeType),
    case Nodes of
        [] ->
            wf:wire(#alert{text = <<"当前没有该类别节点在线"/utf8>>});
        _ ->
            {ok, LocalFileData} = file:read_file(LocalFileName),
            Res = rpc:multicall(Nodes, lib_cluster, update_file, [Dir, FileName, LocalFileData]),
            wf:flash(wf:f("~p", [Res]))
    end,
    browse_dir(Node, Dir),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
browse_dir(Node, Dir) ->
    wf:set(ref_dir, Dir),
    case rpc:call(Node, os, cmd, ["ls -ll " ++ Dir]) of
        {badrpc, nodedown} ->
            wf:wire(#alert{text = <<"节点不存在"/utf8>>});
        Str ->
            List = re:split(Str, "\n"),
            FunDir = fun(Item) ->
                case [I || I <- re:split(Item, " "), I /= <<>>] of
                    [Authority0, _I2, _I3, _I4, _I5, Month0, Date0, Time0, FileName0] ->
                        case Authority0 of
                            <<"d", _/binary>> ->
                                {true, [binary_to_list(FileName0) ++ "/", binary_to_list(<<Month0/binary, "/", Date0/binary, " ", Time0/binary>>), {opendir, FileName0}, {del, dir, FileName0}]};
                            _ ->
                                false
                        end;
                    _ ->
                        false
                end
                     end,
            DatasDir = lists:zf(FunDir, List),
            FunFile = fun(Item) ->
                case [I || I <- re:split(Item, " "), I /= <<>>] of
                    [Authority0, _I2, _I3, _I4, _I5, Month0, Date0, Time0, FileName0] ->
                        case Authority0 of
                            <<"d", _/binary>> ->
                                false;
                            _ ->
                                {true, [binary_to_list(FileName0), binary_to_list(<<Month0/binary, "/", Date0/binary, " ", Time0/binary>>), {openfile, FileName0}, {del, file, FileName0}]}
                        end;
                    _ ->
                        false
                end
                      end,
            DatasFile = lists:zf(FunFile, List),
            CellsDir = [
                #tablecell{id = filename},
                #tablecell{id = edittime},
                #tablecell{body = #button{id = openbtn, text = "打开目录"}},
                #tablecell{body = #button{id = delbtn, text = "删除"}}
            ],
            CellsFile = [
                #tablecell{id = filename},
                #tablecell{id = edittime},
                #tablecell{body = #button{id = openbtn, text = "查看文件"}},
                #tablecell{body = #button{id = delbtn, text = "删除"}}
            ],
            Maps = [filename@text, edittime@text, openbtn@postback, delbtn@postback],
            Table = #table{id = ref_dirdetail, style = "width:60%", class = tiny, rows = [
                #tablerow{cells = [
                    #tableheader{text = "文件名"},
                    #tableheader{text = "更新时间"},
                    #tableheader{text = ""},
                    #tableheader{text = ""}
                ]},
                #bind{data = DatasDir, map = Maps, transform = fun common:alternate_color_2/2, body = #tablerow{id = top, cells = CellsDir}},
                #bind{data = DatasFile, map = Maps, transform = fun common:alternate_color_1/2, body = #tablerow{id = top, cells = CellsFile}}
            ]},
            wf:replace(ref_dirdetail, Table)
    end.

