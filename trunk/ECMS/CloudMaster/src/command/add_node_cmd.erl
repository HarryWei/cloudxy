%% Author: 77
%% Created: 2011-7-3
%% Description: TODO: Add description to add_node_cmd
-module(add_node_cmd).
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
	io:format(" ~p ~p ~p ~n",[?MODULE,Request,State]), %% debug info
	Node = list_to_atom(Request#add_node_2m_req.node_name),
	io:format(" name name: ~p ~n",[Node]),
	Resp1 = case rpc:call(Node,application,start,[node_controller],2000) of
			{_badrpc,{Reason,_}} when Reason /= already_started ->
				  			io:format("start node controller fail ! reason ~p ~n",[Reason]),
                {error,error_for_nc_start};
	  	_Other ->    		
             	  io:format("start up node controller: ~n"),%% debug info
		            gen_server:call({node_server,Node},#node_req{node_name = Request#add_node_2m_req.node_name, content=#get_node_info_2s_req{}},2000)
		            
    end,
    io:format("node resource Resp1  ~p ~n",[Resp1]), %% debug info
    Resp2 = case Resp1 of 
      {ok,Result}-> 
						io:format("node resource back  ~p ~n",[Result]), %% debug info
            meta_data_ops:add_node_res(Request#add_node_2m_req.node_name,
												 (Result#get_node_info_2s_res_content_t.net_addr)#net_addr_info_t.ip_addr,
												 (Result#get_node_info_2s_res_content_t.node_resource)#machine_resoure_info_t.cpu_count,
 												 (Result#get_node_info_2s_res_content_t.node_resource)#machine_resoure_info_t.disk_size,
												 (Result#get_node_info_2s_res_content_t.node_resource)#machine_resoure_info_t.mem_size,
												 (Result#get_node_info_2s_res_content_t.node_free_resource)#machine_resoure_info_t.disk_size,
												 (Result#get_node_info_2s_res_content_t.node_free_resource)#machine_resoure_info_t.mem_size
												 );
        {error,_Reason} -> 
	        	{error,nc_start_failed}
    end,
    io:format("node resource Resp2  ~p ~n",[Resp2]), %% debug info
    case Resp2 of 
        ok ->
            io:format("node resource back  ~n"),
            ok;
        _Error ->
            _Error
    end.

%%
%% Local Functions
%%
-spec(check_parameter(term(),term()) -> ok|no_return()).
check_parameter(Request,State)->
	io:format("  : ~p check parameter ~n",[?MODULE]),
	Node = list_to_atom(Request#add_node_2m_req.node_name),
	case net_adm:ping(Node) of
		pong ->
			io:format("node up !! ~n"),
			ok;
		pang ->
			io:format("node still down ~n"),
            %{false,err_for_nodedown}
			throw({false,err_for_nodedown})
	 end.
