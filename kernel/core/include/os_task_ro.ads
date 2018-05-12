with types;
with os_task_list; use os_task_list;

package os_task_ro with
     Abstract_State => Os_Task_Ro_State is

   subtype os_priority_t is types.uint8_t;

   --  Get the mbx permission for a given task

   function os_mbx_get_mbx_permission
     (task_id : os_task_id_param_t) return os_mbx_mask_t with
      Global => (Input => Os_Task_Ro_State);

      --  Get the mbx priority for a given task

   function os_get_task_priority
     (task_id : os_task_id_param_t) return os_priority_t with
      Global => (Input => Os_Task_Ro_State);

end os_task_ro;
