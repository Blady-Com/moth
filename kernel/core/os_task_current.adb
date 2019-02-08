package body os_task_current with
   Spark_Mode    => On,
   Refined_State => (Os_Task_Current_State => os_task_current)
 is

   ---------------------
   -- os_task_current --
   ---------------------
   --  This variable holds the ID of the current elected task.

   os_task_current : os_task_id_param_t;

   ----------------------------------
   -- os_sched_set_current_task_id --
   ----------------------------------

   procedure os_sched_set_current_task_id (task_id : os_task_id_param_t) with
      Refined_Global  => (Output => os_task_current),
      Refined_Depends => (os_task_current => task_id)
    is
   begin
      os_task_current := task_id;
   end os_sched_set_current_task_id;

   ----------------------------------
   -- os_sched_get_current_task_id --
   ----------------------------------

   function os_sched_get_current_task_id return os_task_id_param_t is (os_task_current) with
      Refined_Global  => (Input => os_task_current),
      Refined_Depends => (os_sched_get_current_task_id'Result => os_task_current);

end os_task_current;
