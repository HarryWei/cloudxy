%% Author: 77
%% Created: 2011-7-6
%% Description: TODO: Add description to start_vm_cmd
-module(start_vm_cmd).
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

build_vm_start_req(NetAddr_Res,Vm_Phy_Res,Node_Name) when (is_record(Vm_Phy_Res,vm_resource_config))-> 
	  {Ip_Addr,Mac_Addr} =  NetAddr_Res,
    Vm_Id           = Vm_Phy_Res#vm_resource_config.vm_id,
    OS_Type         = Vm_Phy_Res#vm_resource_config.os_type,
    Vm_UserPass     = Vm_Phy_Res#vm_resource_config.user_password,
    Vm_HostName     = Vm_Phy_Res#vm_resource_config.user_hostname, %%TODO
    Mem_Size        = Vm_Phy_Res#vm_resource_config.mem_size,
    Disk_Size 	    = Vm_Phy_Res#vm_resource_config.disk_size,
    Vcpu_Count      = Vm_Phy_Res#vm_resource_config.vcpu_count,
    io:format(" ********** ~n"),
    Vnc_Port        = vncport:vnc_port(Node_Name),
    Vnc_Pass        = password:random_password(?PASSWORD_SIZE), 
    io:format("Vnc_Port, Vnc_Pass ~p ~p ~n",[Vnc_Port,Vnc_Pass]),
    Machin_Resource = #machine_resoure_info_t{mem_size = Mem_Size,disk_size = Disk_Size,cpu_count = Vcpu_Count},
    Custom_Info     = #vm_custom_info_t{user_password = Vm_UserPass,user_hostname = Vm_HostName},
    Net_Addr        = #net_addr_info_t{ip_addr = Ip_Addr,mac_addr = Mac_Addr},
    Vnc_Info        = #vnc_info_t{vnc_port = Vnc_Port,vnc_password = Vnc_Pass},
    Vm_Start_Req    = #vm_start_2s_req{vm_id = Vm_Id,os_type = OS_Type,vm_resource_req = Machin_Resource,vm_custom_info = Custom_Info,net_addr = Net_Addr,vnc_info = Vnc_Info},
    Vm_Start_Req.
    
request_process(Request,State) ->
		io:format("handle info ~p  ~p ~p ~n",[?MODULE,Request,State]), %% debug info
    Resp1 = case meta_data_ops:request_netaddr_res() of 
         			{ok,NetAddr_Res} ->
         				    io:format("get net addr resource ~p ~n",[NetAddr_Res]), %% debug info
         	      		NetAddr_Res;
         			_ -> 
         	  				io:format("no net add resource"),
            				throw({false,err_for_no_netres})
    				end,
    {Ip_Addr,Mac_Addr} =  Resp1,				
    [Vm_Phy_Res] = meta_data_ops:get_vm_res_conf(Request#vm_start_2m_req.vm_id), 
    io:format("Vm_Phy_Res resource ~p ~n",[Vm_Phy_Res]), %% debug info
    Resp2 = case meta_data_ops:request_node_res(Vm_Phy_Res#vm_resource_config.mem_size,Vm_Phy_Res#vm_resource_config.disk_size) of
         			{ok,Node_Name} -> 
               		io:format("Node_Name ~p ~n",[Node_Name]),
               		Node_Name;
         			_-> 
     							io:format("no valid node has such phy resouce now"),
     							meta_data_ops:return_netaddr_res(Ip_Addr),
            			throw({false,err_for_notfree_noderes})
            end,
    Vm_Start_Req = build_vm_start_req(Resp1,Vm_Phy_Res,Resp2),      			    
    io:format("Vm_Start_Req ~p ~n",[Vm_Start_Req]), %% debug info
	  case gen_server:call({node_server,list_to_atom(Resp2)},#node_req{node_name =Resp2, content=Vm_Start_Req},1000) of 
       ok ->
       	    io:format("node start vm succ ~n"), %% debug info
            meta_data_ops:add_vm_res_conf(
                  Request#vm_start_2m_req.vm_id,
                  Vm_Phy_Res#vm_resource_config.os_type,
								  1,
								  Vm_Phy_Res#vm_resource_config.mem_size,
								  Vm_Phy_Res#vm_resource_config.disk_size,
						      Ip_Addr,
								  Mac_Addr,
								  Vm_Phy_Res#vm_resource_config.user_password,
								  Vm_Phy_Res#vm_resource_config.user_hostname),
						io:format("node start vm succ 2  ~n"), %% debug info		  
						meta_data_ops:add_vm_runenv_info(
									Request#vm_start_2m_req.vm_id,
									Resp2,
									(Vm_Start_Req#vm_start_2s_req.vnc_info)#vnc_info_t.vnc_port,
									(Vm_Start_Req#vm_start_2s_req.vnc_info)#vnc_info_t.vnc_password,
									running),
						io:format("node start vm succ 3  ~n"), %% debug info		  
            {ok,Request#vm_start_2m_req.vm_id};
      {error,_Reason} ->
      	      io:format("node start vm error ~p ~n",[_Reason]), %% debug info
		        	meta_data_ops:return_netaddr_res(Ip_Addr),
							meta_data_ops:return_node_res(Resp2,Vm_Phy_Res#vm_resource_config.mem_size,Vm_Phy_Res#vm_resource_config.disk_size),
							{error,err_for_start_vm};
      _Other-> 
      	      io:format("node start vm error ~p ~n",[_Other]), %% debug info
      				meta_data_ops:return_netaddr_res(Ip_Addr),
							meta_data_ops:return_node_res(Resp2,Vm_Phy_Res#vm_resource_config.mem_size,Vm_Phy_Res#vm_resource_config.disk_size),
             _Other
    end.

%%
%% Local Functions
%%
-spec(check_parameter(term(),term()) -> ok|no_return()).
check_parameter(Request,State)->
	 io:format(" ~p check parameter ~n",[?MODULE]),
   Res_Conf    = meta_data_ops:get_vm_res_conf(Request#vm_start_2m_req.vm_id),
   io:format(" length(Res_Conf) ~p ~n",[length(Res_Conf)]),
   length(Res_Conf) == 1 orelse throw({false,err_for_nosuchvm}), 
   io:format(" ---------  ~n"),  
   Runenv_Info = meta_data_ops:get_vm_runenv_info(Request#vm_start_2m_req.vm_id),
   if 
     length(Runenv_Info) == 1 -> 
     	  io:format(" runenv info has exist for vm_id : ~p ~n",[Request#vm_start_2m_req.vm_id]),
   			case Runenv_Info#vm_runenv_info.vm_status of
        	runing when(length(Runenv_Info) == 1) -> 
            	throw({false,err_for_suchvm_runing}); %%??
        	_ -> 
           		Req_Mem_Size  = (Request#vm_create_2m_req.vm_resource_info)#machine_resoure_info_t.mem_size,
           		Req_Disk_Size = (Request#vm_create_2m_req.vm_resource_info)#machine_resoure_info_t.disk_size,
           		meta_data_ops:check_node_res(Req_Mem_Size,Req_Disk_Size) orelse throw({false,err_for_notfree_noderes}),
           		meta_data_ops:check_netaddr_res() orelse throw({false,err_for_no_netres}),
           		ok
   			end;
   	 true ->
   	 	  io:format(" runenv info has not exist for vm id: ~p ~n",[Request#vm_start_2m_req.vm_id]),
   	 	  ok
   end.

			
            
           





