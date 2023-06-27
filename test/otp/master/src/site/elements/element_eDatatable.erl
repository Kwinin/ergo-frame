%% -*- mode: nitrogen -*-
%% vim: ts=4 sw=4 et
-module(element_eDatatable).
-include("records.hrl").
-export([
    reflect/0,
    render_element/1,
    event/1
]).

reflect() -> record_info(fields, eDatatable).

render_element(Record = #eDatatable{}) ->
    Func =
        fun(Item) ->
            case Item of
                Item when is_record(Item, button) ->
                    #tablecell{body = [Item]};
                _ ->
                    #tablecell{text = Item}
            end
        end,
    #section{
        class = "panel",
        body = [
            %% 表描述
            #html5_header{
                class = "panel-heading head-border",
                body = [
                    Record#eDatatable.name,
                    #span{
                        class = "tools pull-right",
                        body = [
                            #link{
                                class = "t-close fa fa-times"
                            }
                        ]
                    }
                ]
            },
            %% 表内容
            #table{
                class = "table data-table",
                header = #tablerow{cells = [#tableheader{text = X} || X <- Record#eDatatable.headers]},
                rows = [#tablerow{cells = [Func(X) || X <- Y]} || Y <- Record#eDatatable.rows]
            }
        ]
    }.

event(Event) ->
    io:format("Event:~p~n", [Event]),
    PageModule = wf:page_module(),
    PageModule:event(Event).