%% Author: 77
%% Created: 2011-7-6
%% Description: TODO: Add description to create_vm_cmd
-module(create_vm_cmd).
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
	io:format("handle info ~p  ~p ~p ~n",[?MODULE,Request,State]), %% debug info
	Vm_Id=  sequence:sequence(vm_id),
	Req_Mem_Size  = (Request#vm_create_2m_req.vm_resource_info)#machine_resoure_info_t.mem_size,
  Req_Disk_Size = (Request#vm_create_2m_req.vm_resource_info)#machine_resoure_info_t.disk_size,
  OS_Type = Request#vm_create_2m_req.os_type,
  User_Pass  = (Request#vm_create_2m_req.vm_custom_info)#vm_custom_info_t.user_password,
  User_HostName = (Request#vm_create_2m_req.vm_custom_info)#vm_custom_info_t.user_hostname,
  io:format("handle info ~p ~p ~p ~p ~p ~n",[?MODULE,Vm_Id,OS_Type,Req_Mem_Size,Req_Disk_Size]), %% debug info
	meta_data_ops:add_vm_res_conf(Vm_Id,
	                OS_Type,
								  1,
								  Req_Mem_Size,
								  Req_Disk_Size,
								  "",
								  "",
								  User_Pass,
								  User_HostName
								  ),
	{ok,Vm_Id}.
%%
%% Local Functions
%%
-spec (check_parameter(term(),term()) -> ok|no_return()).
check_parameter(Requst,State)->
	 io:format("  : ~p check parameter ~n",[?MODULE]),
   ok.

	
	
