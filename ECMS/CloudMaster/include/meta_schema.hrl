-author(kanghua ).  
-email(kanghua151@msn.com).

-ifndef(meta_shema_hrl).
-define(meta_shema_hrl,true).

-include("comm_define.hrl").

-record(netaddr_resource, {
		ip_addr 		 ::string(),
		mac_addr		 ::string(),
		is_free			 ::boolean(),
		update_timestamp ::erlang:timestamp()}).

-record(node_resource, {
		host_name   					::string(),
		ip_addr     					::string(),
		cpu_count  						::pos_integer(),
		total_storage_size		::pos_integer(), 
		total_memory_size			::pos_integer(),
		free_storage_size	    ::non_neg_integer(),
		free_memory_size			::non_neg_integer(),
		status								::node_status(),
		create_timestamp			::erlang:timestamp(),
		update_timestamp      ::erlang:timestamp()}).
		
-record(vm_resource_config, {
		vm_id					  ::non_neg_integer(),
		os_type					::os_type(),
		vcpu_count				::pos_integer(),
		mem_size				::non_neg_integer(),
		disk_size				::non_neg_integer(),
		user_password           ::string(),
		user_hostname			::string(),
		ip_address				::pos_integer(),
		mac_address				::pos_integer(),
		update_timestamp		::erlang:timestamp()}).

-record(vm_runenv_info, {
		vm_id					::non_neg_integer(),
		host_name				::string(),	
		vnc_port				::pos_integer(),
		vnc_password			::string(),
		vm_status               ::vm_status(),
		update_timestamp		::erlang:timestamp()}).


-record(vdisk_info,{
		disk_id					::non_neg_integer(),
		disk_uri				::string(),
		disk_size				::non_neg_integer(),
		attached_vm_id			::non_neg_integer(),
    create_time				::erlang:timestamp()}).

-endif.


