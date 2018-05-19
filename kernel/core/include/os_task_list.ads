with types; use types;

package os_task_list with
     Spark_Mode     => On,
     Abstract_State => Os_Task_State is

   procedure os_task_init (task_id : os_task_id_t);

   function os_sched_get_current_list_head return os_task_id_t;

   procedure os_sched_set_current_list_head (task_id : os_task_id_t);

   procedure os_sched_add_task_to_ready_list
     (task_id : os_task_id_param_t) with
      Pre  => os_ghost_task_list_is_well_formed,
      Post => os_ghost_task_list_is_well_formed and
      os_ghost_task_is_ready (task_id);

   procedure os_sched_remove_task_from_ready_list
     (task_id : os_task_id_param_t) with
      Pre => os_ghost_task_list_is_well_formed and
      os_ghost_current_task_is_ready,
      Post => os_ghost_task_list_is_well_formed and
      not os_ghost_task_is_ready (task_id);

   function os_mbx_get_waiting_mask
     (task_id : os_task_id_param_t) return os_mbx_mask_t;

   procedure os_mbx_set_waiting_mask
     (task_id : os_task_id_param_t;
      mask    : os_mbx_mask_t);

   function os_ghost_at_least_one_terminating_next return Boolean with
      Ghost => True;

   function os_ghost_at_least_one_terminating_prev return Boolean with
      Ghost => True;

   function os_ghost_task_list_is_well_formed return Boolean with
      Ghost  => True,
      Global => (Input => Os_Task_State);

   procedure os_ghost_task_ready_init (task_id : os_task_id_t) with
      Ghost => True;

   function os_ghost_task_is_ready
     (task_id : os_task_id_param_t) return Boolean with
      Ghost => True;

   function os_ghost_current_task_is_ready return Boolean with
      Ghost => True;

end os_task_list;
