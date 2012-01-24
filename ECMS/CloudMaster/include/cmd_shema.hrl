-author(kanghua ).  
-email(kanghua151@msn.com).

-ifndef(cmd_shema_hrl).
-define(cmd_shema_hrl,true).

-include("comm_define.hrl").

-record(net_addr_info_t,{
		ip_addr 	::string(),
		mac_addr 	::string()}).

-record(machine_resoure_info_t,{
		mem_size	::pos_integer(),
		cpu_count	::pos_integer(),
    disk_size	::pos_integer()}).

-record(vnc_info_t,{
		vnc_port	::port(),
		vnc_password::string()}).

-record(load_status_t,{
		cpu_used_rete    ::0..100,
		io_used_rate	 ::0..100,
		mem_used		 ::pos_integer(),
		storage_used	 ::pos_integer()}).
		
-record(vm_custom_info_t,{
		user_password		 ::string(),
    user_hostname		 ::string()}).

%%%%%% 
-record(cmd_res,{
	  error			 ::atom(),
		reason		 ::atom()}).
		
-record(node_req,{
    node_name ::string(),
    content   ::any()}).
%%%%% 

-record(vm_create_2m_req,{
	      vm_resource_info = #machine_resoure_info_t{},
        vm_custom_info   = #vm_custom_info_t{},
        os_type			 		 ::os_type()}).

-record(vm_create_2m_res_content_t,{
		vm_id			 ::pos_integer()}).

-record(vm_create_2m_res,{
	    err     		 = #cmd_res{},
		content 		 = #vm_create_2m_res_content_t{}}).
%%
-record(vm_start_2m_req,{
		vm_id	::pos_integer()}).
%%
-record(vm_stop_2m_req,{
		vm_id	::pos_integer()}).
%%
-record(vm_destroy_2m_req,{
		vm_id   ::pos_integer()}).
%%
-record(vdisk_create_2m_req,{
		vdisk_size ::pos_integer()}).

-record(vdisk_create_2m_res_content_t,{
		vdisk_id   ::pos_integer()}).

-record(vdisk_create_2m_res,{
	    err     = #cmd_res{},
		content = #vdisk_create_2m_res_content_t{}}).
%%
-record(vdisk_attach_2m_req,{
		vdisk_id  ::pos_integer(),
		vm_id	 ::pos_integer()}).
%%
-record(vdisk_deattach_2m_req,{
		vdisk_id  ::pos_integer(),
		vm_id    ::pos_integer()}).
%%
-record(vdisk_destroy_2m_req,{
		vdisk_id  ::pos_integer(),
		vm_id     ::pos_integer()}).
%%
-record(add_node_2m_req,{
		node_name ::string()}).
%%
-record(drop_node_2m_req,{
		node_name ::string()}).
%%
-record(get_node_info_2m_req,{
		node_name ::string()}).

%% we will support batch ops later
%% -record(get_node_info_2m_req,{
%%		   node_name = [] :: [string()]}).

-record(get_node_info_2m_res_content_t,{
		node_resource      = #machine_resoure_info_t{},
    node_free_resource = #machine_resoure_info_t{},
    net_addr           = #net_addr_info_t{}}).




%%
-record(get_vm_info_2m_req,{
		vm_id  ::pos_integer()}).
		
-record(get_vm_info_2m_res_content_t,{
		vm_id		::pos_integer(),
		os_type		::centos|ubuntu,
		net_addr 	= #net_addr_info_t{},
        vnc_info 	= #vnc_info_t{},
		machine_resource = #machine_resoure_info_t{},
		disk_ids	::[pos_integer()]}).
		


-record(get_node_status_2m_req,{
		node_name   ::string()}).

-record(get_node_status_2m_res_content_t,{
		node_name   ::string(),
		status		::node_status(),
		load 		= #load_status_t{},
		vm_ids      ::[pos_integer()]}).


-record(get_vm_status_2m_req,{
		vm_id		::pos_integer()}).

-record(get_vm_status_2m_res_content_t,{
		vm_id		::pos_integer(),
		status		::vm_status(),
		load 		= #load_status_t{}}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-record(vm_start_2s_req,{
		vm_id							::pos_integer(),
		vm_resource_req 	= #machine_resoure_info_t{},
    vm_custom_info  	= #vm_custom_info_t{},
		net_addr 					= #net_addr_info_t{},
    vnc_info 					= #vnc_info_t{},
		os_type						::os_type(),
		sys_disk_base_url ::string()}).
%%
-record(vm_stop_2s_req,{
        vm_id			  ::pos_integer()}).
%%
-record(vm_destroy_2s_req,{
        vm_id			  ::pos_integer()}).
%%
-record(disk_attach_2s_req,{
		disk_id			    ::pos_integer(),
		vm_id			      ::pos_integer()}).
%%
-record(disk_deattach_2s_req,{
		disk_id			::pos_integer(),
		vm_id			  ::pos_integer()}).
%%
-record(get_node_info_2s_req,{}).
-record(get_node_info_2s_res_content_t,{
		    node_resource     = #machine_resoure_info_t{},
        node_free_resource = #machine_resoure_info_t{},
        net_addr           = #net_addr_info_t{}}).
%%
-endif.