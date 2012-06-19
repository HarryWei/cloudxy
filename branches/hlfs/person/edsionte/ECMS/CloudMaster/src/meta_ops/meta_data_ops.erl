%% Author: 77
%% Created: 2011-7-3
%% Description: TODO: Add description to meta_data_ops
-module(meta_data_ops).
-author(kanghua ).  
-email(kanghua151@msn.com).  
%%
%% Include files
%%
-include("meta_schema.hrl").
-include_lib("stdlib/include/qlc.hrl").
-include_lib("kernel/include/inet.hrl").

-define(NOW ,erlang:localtime()).
%%
%% Exported Functions
%%

-export([init_table_netaddr_resource/1]).
-export([init_table_node_resource/1]).
-export([init_table_vm_resource_config/1]).
-export([init_table_vm_runenv_info/1]).
-export([init_table_vdisk_info/1]).
%%
-export([add_netaddr_res/1]).
-export([drop_netaddr_res/1]).
-export([request_netaddr_res/0]).
-export([check_netaddr_res/0]).
-export([return_netaddr_res/1]).
%%
-export([add_node_res/7]).
-export([drop_node_res/1]).
-export([request_node_res/2]).
-export([check_node_res/2]).
-export([return_node_res/3]).
-export([close_node_res/1]).
-export([open_node_res/1]).
-export([get_node_res/1]).
%%
-export([add_vm_res_conf/9]).
-export([drop_vm_res_conf/1]).
-export([get_vm_res_conf/1]).
%%
-export([add_vm_runenv_info/5]).
-export([drop_vm_runenv_info/1]).
-export([get_vm_runenv_info/1]).
-export([get_vms_runenv_info_by_node/1]).
%%



-spec(init_table_netaddr_resource(list(node())) -> boolean()).
init_table_netaddr_resource(Nodes)->	
    case catch mnesia:table_info(netaddr_resource,where_to_read) of
 	{'EXIT', _} -> %% non-existing table
        case mnesia:create_table(netaddr_resource, [
 		 	 {type,set},									
         	 {disc_copies, Nodes},
         	 {attributes, record_info(fields,netaddr_resource)}
        	 ]) of
    		{atomic,ok} ->
				io:format("table ~p create success ~n",[netaddr_resource]),
				true;
    		{aborted,Reason}->
        		io:format("error:~p ~n",[Reason]),
        		false
        end;
	Node ->
 		io:format("** Table counter already exists on ~p, just entering data~n", [Node]),
        true
    end.

-spec(init_table_node_resource(list(node())) -> boolean()).
init_table_node_resource(Nodes)->	
    case catch mnesia:table_info(node_resource,where_to_read) of
 	{'EXIT', _} -> %% non-existing table
        case mnesia:create_table(node_resource, [
 		 	 {type,set},									
         	 {disc_copies, Nodes},
         	 {attributes, record_info(fields,node_resource)}
        	 ]) of
    		{atomic,ok} ->
				io:format("table ~p create success ~n",[node_resource]),
				true;
    		{aborted,Reason}->
        		io:format("error:~p ~n",[Reason]),
        		false
        end;
	Node ->
 		io:format("** Table counter already exists on ~p, just entering data~n", [Node]),
        true
    end.
-spec(init_table_vm_resource_config(list(node())) -> boolean()).
init_table_vm_resource_config(Nodes)->	
    case catch mnesia:table_info(vm_resource_config,where_to_read) of
 	{'EXIT', _} -> %% non-existing table
        case mnesia:create_table(vm_resource_config, [
 		 	 {type,set},									
         	 {disc_copies, Nodes},
         	 {attributes, record_info(fields,vm_resource_config)}
        	 ]) of
    		{atomic,ok} ->
				io:format("table ~p create success ~n",[vm_resource_config]),
				true;
    		{aborted,Reason}->
        		io:format("error:~p ~n",[Reason]),
        		false
        end;
	Node ->
 		io:format("** Table vm_resource_config already exists on ~p, just entering data~n", [Node]),
        true
    end.
-spec(init_table_vm_runenv_info(list(node())) -> boolean()).
init_table_vm_runenv_info(Nodes)->	
    case catch mnesia:table_info(vm_runenv_info,where_to_read) of
 	{'EXIT', _} -> %% non-existing table
        case mnesia:create_table(vm_runenv_info, [
 		 	 {type,set},									
         	 {disc_copies, Nodes},
         	 {attributes, record_info(fields,vm_runenv_info)}
        	 ]) of
    		{atomic,ok} ->
				io:format("table ~p create success ~n",[vm_runenv_info]),
				true;
    		{aborted,Reason}->
        		io:format("error:~p ~n",[Reason]),
        		false
        end;
	Node ->
 		io:format("** Table vm_runenv_info already exists on ~p, just entering data~n", [Node]),
        true
    end.
-spec(init_table_vdisk_info(list(node())) -> boolean()).
init_table_vdisk_info(Nodes)->	
    case catch mnesia:table_info(vdisk_info,where_to_read) of
 	{'EXIT', _} -> %% non-existing table
        case mnesia:create_table(vdisk_info, [
 		 	 {type,set},									
         	 {disc_copies, Nodes},
         	 {attributes, record_info(fields,vdisk_info)}
        	 ]) of
    		{atomic,ok} ->
					 io:format("table ~p create success ~n",[vdisk_info]),
					 true;
    		{aborted,Reason}->
        		io:format("error:~p ~n",[Reason]),
        		false
        end;
	Node ->
 		io:format("** Table vdisk_info already exists on ~p, just entering data~n", [Node]),
        true
    end.

-spec(is_valid_ipaddr(atom())  -> boolean()).
is_valid_ipaddr(Ip_Addr) -> 
    case inet_parse:address(Ip_Addr) of
     {ok,_} -> true;
     _      -> false
    end.

-spec(add_netaddr_res(string()) -> ok|{error,err_for_invalidip}).
add_netaddr_res(Ip_Addr) ->
    case is_valid_ipaddr(Ip_Addr) of 
        true ->    
            Row =   #netaddr_resource{ ip_addr  = Ip_Addr, 
						       mac_addr  =mac_address:get_one_mac(Ip_Addr),
					 	       is_free = true,
					 	       update_timestamp = ?NOW},
	        io:format("row ~p ~n",[Row]), %% debug info
	        Fun = fun() ->mnesia:write(Row) end,
	        {atomic,Res} = mnesia:transaction(Fun),Res;
        false->
            {error,err_for_invalidip}
    end.

-spec(drop_netaddr_res(string()) ->ok|{error,err_for_invalidip}).
drop_netaddr_res(Ip_Addr)  ->
    case is_valid_ipaddr(Ip_Addr) of
       true -> 
	        Fun = fun() ->mnesia:delete(netaddr_resource,Ip_Addr) end,
	        {atomic, Res} = mnesia:transaction(Fun),Res;
       false ->
            {error,err_for_invalidip}
    end.
	
-spec(request_netaddr_res()-> {ok,{string(),string()}}|{error,err_for_nofree_netaddr}).
request_netaddr_res()->
	io:format("enter func request network ~n"), %% debug info
	Fun = fun() ->
		Q = qlc:q([E || E <- mnesia:table(netaddr_resource),E#netaddr_resource.is_free==true]),
		Results = qlc:e(Q),
		io:format("new results ~p ~n",[Results]),
		if 
       length(Results) == 0  -> 
              {error,err_for_nofree_netaddr};
       true ->
			   			Result = lists:nth(1,Results),
			   			NewOne = Result#netaddr_resource{is_free=false},
     		 			io:format("new result ~p ~n",[Result]),
	 		   			mnesia:write(NewOne),
			   			{ok,{Result#netaddr_resource.ip_addr,Result#netaddr_resource.mac_addr}}
        end
	end,
	{atomic, Res} = mnesia:transaction(Fun),Res.

-spec(check_netaddr_res()-> boolean()).
check_netaddr_res()->
	io:format("enter func request network ~n"), %% debug info
	Fun = fun() ->
		Q = qlc:q([E || E <- mnesia:table(netaddr_resource),E#netaddr_resource.is_free==true]),
		Results = qlc:e(Q),
		io:format("new results ~p ~n",[Results]),
        Results
	end,
	{atomic, Res} = mnesia:transaction(Fun),
    if length(Res) =:= 0
		-> false
	end.

-spec(return_netaddr_res(string())->ok).
return_netaddr_res(Ip_Addr) ->
    Fun = fun() ->
		Results = mnesia:read(netaddr_resource,Ip_Addr),
		Result = lists:nth(1, Results),
        NewOne = Result#netaddr_resource{is_free=true},
        io:format("new result ~p ~n",[NewOne]),
	    mnesia:write(NewOne)
	end,
    {atomic, Res} = mnesia:transaction(Fun),Res.
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec(add_node_res(string(),string(),pos_integer(),pos_integer(),pos_integer(),pos_integer(),pos_integer())->ok).
add_node_res(Host_Name,Ip_Addr,Vcpu_Count,Total_StorageSize,Total_MemorySize,Free_StorageSize,Free_MemorySize) ->
	io:format("new result ~p ~p ~p ~p ~p ~p ~p ~n",[Host_Name,Ip_Addr,Vcpu_Count,Total_StorageSize,Total_MemorySize,Free_StorageSize,Free_MemorySize]),
	Row = #node_resource{host_name 			= Host_Name, 
					 	 ip_addr 						= Ip_Addr,
					 	 cpu_count 		    	= Vcpu_Count,
					 	 total_storage_size = Total_StorageSize,
					 	 total_memory_size  = Total_MemorySize,
					 	 free_storage_size  = Free_StorageSize,
				     free_memory_size   = Free_MemorySize,
						 status 		    		= running,
					 	 create_timestamp   = ?NOW,
					 	 update_timestamp   = ?NOW},
	io:format("row ~p ~n",[Row]), %% debug info
	Fun = fun() ->mnesia:write(Row) end,
    {atomic, Res} = mnesia:transaction(Fun),Res.

-spec(drop_node_res(string()) -> atom()).
drop_node_res(Node) ->
	Fun = fun() ->mnesia:delete(node_resource,Node) end,
	{atomic, Res} = mnesia:transaction(Fun),Res.

-spec(check_node_res(pos_integer(),pos_integer())->boolean()).
check_node_res(Mem_Size,Store_Size) ->
	Fun = fun() ->
		MatchHead = #node_resource{host_name='$1',free_memory_size='$2',free_storage_size='$3',status=running, _='_'},
		Guard1 = {'>', '$2', Mem_Size},
		Guard2 = {'>', '$3', Store_Size},
        Res = '$_',
    	{Results,Cont} = mnesia:select(node_resource,[{MatchHead, [Guard1,Guard2], [Res]}],1,write),
	 	io:format("select results ~p ~n",[Results])
     end,
	 {atomic,Res} = mnesia:transaction(Fun),
	 if 
        length(Res) =:= 0 -> false
	 end.
   
 -spec(request_node_res(pos_integer(),pos_integer())-> {ok,{string()}}|{error,err_for_nofree_noderes}).
request_node_res(Mem_Size,Store_Size) ->
	io:format("Mem_Size Store_Size ~p ~p ~n",[Mem_Size,Store_Size]),
	Fun = fun() ->
		%MatchHead = #node_resource{host_name='$1',free_memory_size='$2',free_storage_size='$3',status=running, _='_'},
		%Guard1 = {'>', '$2', Mem_Size},
		%Guard2 = {'>', '$3', Store_Size},
    %Res = '$_',
		%case mnesia:select(node_resource,[{MatchHead, [Guard1,Guard2], [Res]}],1,write) of
		MatchHead = #node_resource{host_name='$1',free_memory_size='$2',status=running, _='_'},
		Guard1 = {'>', '$2', Mem_Size},
		Res = '$_',
		case mnesia:select(node_resource,[{MatchHead, [Guard1], [Res]}],1,write) of
			{Results,Cont} ->
	 				io:format("select results ~p ~n",[Results]),
    			%Result = lists:nth(random:uniform(length(Results)),Results),
					Result = lists:nth(1,Results),
	 				io:format("select result ~p ~n",[Result]),
     			Free_Memory  = Result#node_resource.free_memory_size - Mem_Size, 
	 				%Free_Storage = Result#node_resource.free_storage_size - Store_Size, 
	 				NewOne = Result#node_resource{free_memory_size=Free_Memory,update_timestamp=?NOW},
     			io:format("new result ~p ~n",[NewOne]),
	 				mnesia:write(NewOne),
					{ok,Result#node_resource.host_name};
			'$end_of_table' ->
				  io:format("no valid node  ~n"),
					{error,err_for_nofree_noderes}
		end
 	end,
  {atomic, Res} = mnesia:transaction(Fun),Res.


 -spec(close_node_res(string())->atom()).
close_node_res(Node) ->
    Host_name = Node,
	Fun = fun() ->
    	[P] = mnesia:wread({node_resource,Host_name}),
    	mnesia:write(P#node_resource{status=down})
    end,
    {atomic, Res} = mnesia:transaction(Fun),Res.

-spec(open_node_res(string())->atom()).
open_node_res(Node) ->
	Host_name = Node,
	Fun = fun() ->
    	[P] = mnesia:wread({node_resource,Host_name}),
    	mnesia:write(P#node_resource{status=running})
    end,
    {atomic, Res} = mnesia:transaction(Fun),Res.

-spec(return_node_res(string(),pos_integer(),pos_integer())->atom()).
return_node_res(Node,Mem_Size,Store_Size) ->
	Fun = fun() ->
		Q = qlc:q([E || E <- mnesia:table(node_resource),E#node_resource.host_name==Node]),
		Results = qlc:e(Q),
		io:format("new results ~p ~n",[Results]),
		Result = lists:nth(1,Results),
		Free_Memory  = Result#node_resource.free_memory_size  + Mem_Size, 
	 	Free_Storage = Result#node_resource.free_storage_size + Store_Size, 
		NewOne = Result#node_resource{free_memory_size=Free_Memory,free_storage_size=Free_Storage},
     	io:format("new result ~p ~n",[Result]),
	 	mnesia:write(NewOne)
	end,
    {atomic, Res} = mnesia:transaction(Fun),Res.

-spec(get_node_res(string())->list(#node_resource{})).
get_node_res(Node) ->
	io:format("get_node_info ! vmid is ~p ~n",  [Node]),
    Fun = fun() ->
		mnesia:read(node_resource,Node)
	end,
   {atomic,Res} = mnesia:transaction(Fun),Res.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec(add_vm_res_conf(pos_integer(),os_type(),pos_integer(),pos_integer(),pos_integer(),string(),string(),string(),string())->atom()).
add_vm_res_conf(Vm_Id,OS_Type,Vcpu_Count,Mem_Size,Disk_Size,Ip_Addr,Mac_Addr,User_Pass,User_HostName) ->
	io:format("vm resource config ! ~n"),
	Row = #vm_resource_config{vm_id = Vm_Id, 
								os_type       = OS_Type,
					 	 	  vcpu_count 		= Vcpu_Count,
							  mem_size 			= Mem_Size,
					 		  disk_size 		= Disk_Size,
					 	 	  ip_address 		= Ip_Addr,
					 	 	  mac_address 	= Mac_Addr,
					 	 	  user_password = User_Pass,
					 	 	  user_hostname = User_HostName,
					 		  update_timestamp 	= ?NOW},
	io:format("row ~p ~n",[Row]), %% debug info
	Fun = fun() ->
		mnesia:write(Row)
	end,
	{atomic, Res} = mnesia:transaction(Fun),Res.
-spec(drop_vm_res_conf(pos_integer())->atom()).
drop_vm_res_conf(Vm_Id)->
	Fun = fun() -> 
		mnesia:delete(vm_resource_config,Vm_Id) 
	end,
	{atomic, Res} = mnesia:transaction(Fun),Res.

-spec(get_vm_res_conf(pos_integer())->list(#vm_resource_config{})).
get_vm_res_conf(Vm_Id) ->
	io:format("get_vm_res_info ! vmid is ~p ~n",  [Vm_Id]),
    Fun = fun() ->
		mnesia:read(vm_resource_config,Vm_Id)
	end,
    {atomic,Res} = mnesia:transaction(Fun),Res.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec(add_vm_runenv_info(pos_integer(),string(),pos_integer(),string(),vm_status())->atom()).
add_vm_runenv_info(Vm_Id,Host_Name,Vnc_Port,Vnc_Pass,Vm_Status)->
	io:format("insert_vm_runtime_info ! ~p ~p ~p ~p ~p ~n",[Vm_Id,Host_Name,Vnc_Port,Vnc_Pass,Vm_Status]),
    Row = #vm_runenv_info{ vm_id = Vm_Id, 
					 	   host_name = Host_Name,
					 	   vnc_port = Vnc_Port,
					 	   vnc_password = Vnc_Pass,
					 	   vm_status = Vm_Status,
					 	   update_timestamp = ?NOW},
	Fun = fun() ->mnesia:write(Row) end,
	{atomic, Res} = mnesia:transaction(Fun),Res.


-spec(drop_vm_runenv_info(pos_integer())->atom()).
drop_vm_runenv_info(Vm_Id)->
   Fun = fun() ->mnesia:delete(vm_runenv_info,Vm_Id) end,
   {atomic, Res} = mnesia:transaction(Fun),Res.

-spec(get_vm_runenv_info(pos_integer())->list(#vm_runenv_info{})).
get_vm_runenv_info(Vm_Id) ->
%	VmId = atom_to_list(OldVmId),
	io:format("get_vm_runtime_info ! vmid is ~p ~n",  [Vm_Id]),
    Fun = fun() ->
		mnesia:read(vm_runenv_info,Vm_Id)
	end,
   {atomic,Res} = mnesia:transaction(Fun),Res.

-spec(get_vms_runenv_info_by_node(pos_integer())->list(#vm_runenv_info{})).
get_vms_runenv_info_by_node(Node) ->
	Fun = fun() ->
		Q = qlc:q([E || E <- mnesia:table(vm_runenv_info),E#vm_runenv_info.host_name==Node]),
		Results = qlc:e(Q),
		io:format("new results ~p ~n",[Results]),
		Results
	end,
    {atomic, Res} = mnesia:transaction(Fun),Res.



