%% Author: 77
%% Created: 
%% Description: TODO: Add description to fetch_node_resouce_cmd
-module(get_node_info_cmd).
-author(kanghua ).  
-email(kanghua151@msn.com).  

%%
%% Include files
%%
-include("meta_schema.hrl").
-include("cmd_shema.hrl").
-include_lib("kernel/include/inet.hrl").
-define(MOCK_TEST,1).
%%
%% Exported Functions
%%
-export([request_process/2,check_parameter/2]).

%%
%% API Functions
%%

mock_get_node_info()->
	{ok,Iplist} = inet:getiflist(),
  io:format("Iplist ~p ~n", [Iplist]),
  Ip = lists:nth(1,Iplist),
  io:format("ip ~p ~n", [Ip]),
  io:format("hostname ~p ~n", [atom_to_list(node())]),
	Res = lists:concat([atom_to_list(node()) , " ",
	                   Ip  , " ",
	                   "4" , " " ,
	                   "4096" , " ",
	                   "4096" , " ",
	                   "1024" , " ",
	                   "1024"]),
	Res.

get_node_info()->
	 Res = os:cmd("bash ./priv/script/get_node_resource.sh") -- "\r\n",
   Res.



-ifdef(MOCK_TEST).
-define(GET_NODE_INFO(),mock_get_node_info()).
-else.
-define(GET_NODE_INFO(),get_node_info()).
-endif.

request_process(Request,State) ->
	io:format("~p ~p ~p ~n",[?MODULE,Request,State]), %% debug info
  Res = ?GET_NODE_INFO(),
	io:format("os:cmd return ~p ~n", [Res]),
	case io_lib:fread("~s~s~d~d~d~d~d",Res) of 
		  {ok,ParseRes,_} ->
		  	io:format("parse cmd result ok ~p ~n",[ParseRes]),
		  	[Host_Name,Ip_Addr,Cpu_Count,Storage_Size,Memory_Size,Free_StorageSize,Free_MemorySize] = ParseRes,
        Machine_Resource = #machine_resoure_info_t{mem_size=Memory_Size,cpu_count=Cpu_Count},
        Machine_Free_Resource = #machine_resoure_info_t{mem_size=Free_MemorySize,cpu_count=Cpu_Count},
				Machine_Net_Addr =  #net_addr_info_t{ip_addr=Ip_Addr},
		  	Result = #get_node_info_2s_res_content_t{node_resource = Machine_Resource,
 								 																 node_free_resource = Machine_Free_Resource,
                                                 net_addr =Machine_Net_Addr},
			  {ok,Result};
		  _ -> 
		  	io:format("parse cmd result error ~p ~n",[resource_unkown]),
        {error,resource_unkown}
	end.
-spec(check_parameter(term(),term()) -> ok|no_return()).
check_parameter(Request,State)->
    ok.
