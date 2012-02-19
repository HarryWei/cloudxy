{application, node_controller,   
[{description, "Node Controller Service For ECP "}, 
{vsn, "0.1.0"}, 
{modules, [node_controller_app, node_controller_sup, node_server, status_monitor]}, 
{registered, []}, 
{applications, [kernel, stdlib, sasl]}, 
{mod, {node_controller_app,[]}} 
]}.
