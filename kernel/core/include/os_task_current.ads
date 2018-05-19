with types; use types;

package os_task_current with
     Spark_Mode     => On,
     Abstract_State => Os_Task_Current_State is

   procedure os_sched_set_current_task_id (task_id : os_task_id_param_t);

   function os_sched_get_current_task_id return os_task_id_param_t;

end os_task_current;
