package os.task_ro is

   --  Get the mbx permission for a given task

   function os_mbx_get_mbx_permission
     (task_id : os_task_id_param_t) return os_mbx_mask_t;

   --  Get the mbx priority for a given task

   function os_get_task_priority
     (task_id : os_task_id_param_t) return os_priority_t;

end os.task_ro;
