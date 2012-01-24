%% Author: 77
%% Created: 2011-7-5
%% Description: TODO: Add description to fetch_node_resouce_cmd
-module(get_node_info_cmd).
-author(kanghua ).  
-email(kanghua151@msn.com).  
%%
%% Include files
%%
-include("meta_schema.hrl").
-include("cmd_shema.hrl").
%%
%% Exported Functions
%%
-export([request_process/2,check_parameter/2]).
%%
%% API Functions
%%


request_process(Request,State) ->
	io:format("handle info ~p ~p ~p ~n",[?MODULE,Request,State]), %% debug info
	Node = list_to_atom(Request#get_node_info_2m_req.node_name),
	io:format("node info ~p ~n",[Node]), %% debug info
    case gen_server:call({node_server,Node},#node_req{node_name = Request#get_node_info_2m_req.node_name, content=#get_node_info_2s_req{}},1000) of
		{error,Reason} ->
               {error,Reason};
        {ok,Resource}  ->  
			io:format("node resource ~p ~n",[Resource]), %% debug info
            {ok,Resource}
     end.

%%
%% Local Functions
%%
-spec(check_parameter(term(),term()) -> ok|no_return()).
check_parameter(Request,State)->
	 Node_Name = Request#get_node_info_2m_req.node_name,
	 Res = meta_data_ops:get_node_res(Node_Name),
	 if 
	   length(Res) =:= 0  
			-> throw({false,err_for_nodedown});
	   true ->
            ok
     end.
