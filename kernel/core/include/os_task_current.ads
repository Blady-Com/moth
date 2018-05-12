with os_task_list; use os_task_list;

package os_task_current with
     Abstract_State => Os_Task_Current_State is

   procedure os_sched_set_current_task_id (task_id : os_task_id_param_t);

   function os_sched_get_current_task_id return os_task_id_param_t;

end os_task_current;
