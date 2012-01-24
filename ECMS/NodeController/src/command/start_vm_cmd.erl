%% Author: 77
%% Created: 2012-1-16
%% Description: TODO: Add description to start_vm
-module(start_vm_cmd).

%%
%% Include files
%%
-include("meta_schema.hrl").
-include("cmd_shema.hrl").
-define(MOCK_TEST,1).
%%
%% Exported Functions
%%
-export([request_process/2,check_parameter/2]).
%%
%% API Functions
%%


mock_start_vm(Vcpu_Count,Mem_Size,Disk_Size,Ip_Addr,Mac_Addr,Vnc_Pass,Vnc_Port,Vm_HostName,Vm_Pass,Vm_Id)->
	Res = "SUCC",
	Res.

start_vm(Vcpu_Count,Mem_Size,Disk_Size,Ip_Addr,Mac_Addr,Vnc_Pass,Vnc_Port,Vm_HostName,Vm_Pass,Vm_Id)->
	 Params = lists:concat(["bash ./priv/script/create_vm.sh ",
									integer_to_list(Vcpu_Count)," ",
									integer_to_list(Mem_Size), " ",
									integer_to_list(Disk_Size)," ",
									Ip_Addr," ",
									Mac_Addr," ",
									Vnc_Pass," ",
									integer_to_list(Vnc_Port)," ",
									Vm_HostName," ",
									Vm_Pass," ",
									integer_to_list(Vm_Id)]),
	  io:format("~p~n", [Params]),
	  Res = os:cmd(Params) -- "\r\n",
	  Res.



-ifdef(MOCK_TEST).
-define(START_VM(Vcpu_Count,Mem_Size,Disk_Size,Ip_Addr,Mac_Addr,Vnc_Pass,Vnc_Port,Vm_HostName,Vm_Pass,Vm_Id),mock_start_vm(Vcpu_Count,Mem_Size,Disk_Size,Ip_Addr,Mac_Addr,Vnc_Pass,Vnc_Port,Vm_HostName,Vm_Pass,Vm_Id)).
-else.
-define(START_VM(Vcpu_Count,Mem_Size,Disk_Size,Ip_Addr,Mac_Addr,Vnc_Pass,Vnc_Port,Vm_HostName,Vm_Pass,Vm_Id),start_vm(Vcpu_Count,Mem_Size,Disk_Size,Ip_Addr,Mac_Addr,Vnc_Pass,Vnc_Port,Vm_HostName,Vm_Pass,Vm_Id)).
-endif.


request_process(Request,State) ->
	%%{Vm_id,VcpuCount,MemSize,DiskSize,Ipaddr,VncPort,VncPass} = Content,
	%%{VmId,VcpuCount,MemSize,DiskSize,IpAddr} = Content,
	%%{VcpuCount,MemSize,DiskSize,IpAddr,MacAddr,VncPass,VncPort,VmUser,VmUserPass,VmId} = Content,
    Vm_Id = Request#vm_start_2s_req.vm_id,
    Mem_Size = (Request#vm_start_2s_req.vm_resource_req)#machine_resoure_info_t.mem_size,
    Vcpu_Count = (Request#vm_start_2s_req.vm_resource_req)#machine_resoure_info_t.cpu_count,
    Vm_Pass = (Request#vm_start_2s_req.vm_custom_info)#vm_custom_info_t.user_password,
    Vm_HostName =  (Request#vm_start_2s_req.vm_custom_info)#vm_custom_info_t.user_hostname,
    Ip_Addr = (Request#vm_start_2s_req.net_addr)#net_addr_info_t.ip_addr,
    Mac_Addr =  (Request#vm_start_2s_req.net_addr)#net_addr_info_t.mac_addr, 
    Vnc_Port = (Request#vm_start_2s_req.vnc_info)#vnc_info_t.vnc_port,
    Vnc_Pass = (Request#vm_start_2s_req.vnc_info)#vnc_info_t.vnc_password,
	  io:format("create vm request  ~p ~n", [Request]),
    Disk_Size = 0,
	  Res = ?START_VM(Vcpu_Count,Mem_Size,Disk_Size,Ip_Addr,Mac_Addr,Vnc_Pass,Vnc_Port,Vm_HostName,Vm_Pass,Vm_Id),
%		D = os:cmd("bash create_vm.sh") -- "\r\n",
		io:format("cmd result  ~p ~n", [Res]),
		if 
  		Res == "FAIL" ->
				  {error,err_for_createvm};
			Res == "SUCC" ->
				  ok
		end.

mock_check_vm_status(Vm_Id)->
	Res = "NOFIND",
	Res.

check_vm_status(Vm_Id)->
	  Params = lists:concat(["bash ./priv/script/check_vm_statu.sh", Vm_Id]),
    io:format("Params  ~p ~n", [Params]),
	  Res = os:cmd(Params) -- "\r\n",
	  Res.

-ifdef(MOCK_TEST).
-define(CHECK_VM_STATUS(Vm_Id),mock_check_vm_status(Vm_Id)).
-else.
-define(CHECK_VM_STATUS(Vm_Id),check_vm_status(Vm_Id)).
-endif.


-spec(check_parameter(term(),term()) -> ok|no_return()).
check_parameter(Request,State)->
	io:format("~p ~p ~p ~n",[?MODULE,Request,State]), %% debug info
	Vm_Id	  = Request#vm_start_2s_req.vm_id,
	Res = ?CHECK_VM_STATUS(Vm_Id),
  io:format("cmd result  ~p ~n", [Res]),
	if
      Res == "NOFIND" ->
		     ok;
	    true ->
			 throw({error,error_for_vm_exist})
	end.

