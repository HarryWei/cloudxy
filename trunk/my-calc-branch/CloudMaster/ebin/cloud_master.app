{application, cloud_master,   
[{description, "Cloud Master Serivce For ECP "}, 
{vsn, "0.1.0"}, 
{modules, [cloud_master_app, cloud_master_sup, meta_server, status_monitor]}, 
{registered, []}, 
{applications, [kernel, stdlib, sasl]}, 
{mod, {cloud_master_app,[]}} 
]}.
