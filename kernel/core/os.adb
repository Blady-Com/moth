with Interfaces;   use Interfaces;
with Interfaces.C; use Interfaces.C;
with os_arch;      use os_arch;
with os_task_ro;   use os_task_ro;
with os_task_current; use os_task_current;

package body os with
     Spark_Mode is

   ----------------------------------
   -- Private functions/procedures --
   ----------------------------------

   -----------------------
   -- os_sched_schedule --
   -----------------------

   procedure os_sched_schedule (task_id : out os_task_id_param_t)
   with
      Global => (Input => os_task_ro.OS_Task_RO_State,
                 In_Out => os_task_list.OS_Task_State,
                 Output => os_task_current.Os_Task_Current_State),
      Pre => os_task_list.os_ghost_task_list_is_well_formed,
      Post => os_task_list.os_ghost_task_list_is_well_formed and then
              os_task_list.os_ghost_task_is_ready (task_id) -- Q: os.adb:65:15: medium: postcondition might fail, cannot prove os_task_list.os_ghost_task_is_ready (task_id)
   is
   begin
      --  Check interrupt status
      if os_arch_interrupt_is_pending = 1 then
         --  Put interrupt task in ready list if int is set.
         os_sched_add_task_to_ready_list (OS_INTERRUPT_TASK_ID);
      end if;

      while os_sched_get_current_list_head = OS_TASK_ID_NONE loop
         --  No task is elected:
         --  Put processor in idle mode and wait for interrupt.
         os_arch_idle;

         --  Check interrupt status
         if os_arch_interrupt_is_pending = 1 then
            --  Put interrupt task in ready list if int is set.
            os_sched_add_task_to_ready_list (OS_INTERRUPT_TASK_ID);
         end if;
      end loop;

      task_id := os_sched_get_current_list_head;

      --  Select the elected task as current task.
      os_sched_set_current_task_id (task_id);

      --  Return the ID of the elected task to allow context switch at low
      --  (arch) level
   end os_sched_schedule;

   ----------------------------
   -- os_mbx_get_posted_mask --
   ----------------------------

   function os_mbx_get_posted_mask
     (task_id : os_task_id_param_t) return os_mbx_mask_t
   with
      Pre => os_task_mbx.os_ghost_task_mbx_are_well_formed (task_id)
   is
      mbx_mask  : os_mbx_mask_t := 0;
      mbx_index : os_mbx_index_t;
   begin

      if not os_mbx_is_empty (task_id) then
         mbx_index := os_mbx_get_mbx_head (task_id);
         for iterator in 1 .. os_mbx_get_mbx_count (task_id)
         loop
            if os_mbx_get_mbx_entry_sender (task_id, mbx_index) in os_task_id_param_t then
               mbx_mask :=
                 mbx_mask or
                 os_mbx_mask_t (Shift_Left
                                (Unsigned_32'(1),
                                   Natural (os_mbx_get_mbx_entry_sender (task_id, mbx_index))));
            end if;
            mbx_index := os_mbx_index_t'Succ(mbx_index);
         end loop;
      end if;

      return mbx_mask;
   end os_mbx_get_posted_mask;

   --------------------------
   -- os_mbx_send_one_task --
   --------------------------

   procedure os_mbx_send_one_task
     (status  : out os_status_t;
      dest_id :     os_task_id_param_t;
      mbx_msg :     os_mbx_msg_t)
   with
      Global => (Input  => (os_task_current.OS_Task_Current_State, os_task_ro.OS_Task_RO_State),
                 In_Out => (os_task_list.OS_Task_State, os_task_mbx.OS_Task_Mbx_State)),
      Pre => os_task_list.os_ghost_task_list_is_well_formed and then
             os_task_list.os_ghost_current_task_is_ready and then
             os_ghost_task_mbx_are_well_formed (dest_id),
      Post => os_task_list.os_ghost_task_list_is_well_formed and then
              os_task_list.os_ghost_current_task_is_ready and then -- Q: os.adb:139:15: medium: postcondition might fail, cannot prove os_task_list.os_ghost_current_task_is_ready
              os_ghost_task_mbx_are_well_formed (dest_id)
   is
      current        : constant os_task_id_param_t := os_sched_get_current_task_id;
      mbx_permission : constant os_mbx_mask_t :=
        os_mbx_get_mbx_permission (dest_id) and
        os_mbx_mask_t (Shift_Left (Unsigned_32'(1), Natural (current)));
   begin
      if mbx_permission /= 0 then
         if os_mbx_is_full (dest_id) then
            status := OS_ERROR_FIFO_FULL;
         else
            os_mbx_add_message (dest_id, current, mbx_msg);
            if (os_mbx_get_waiting_mask (dest_id) and
              os_mbx_mask_t (Shift_Left (Unsigned_32'(1), Natural (current)))) /= 0 then
               os_sched_add_task_to_ready_list (dest_id);
            end if;

            status := OS_SUCCESS;
         end if;
      else
         status := OS_ERROR_DENIED;
      end if;
   end os_mbx_send_one_task;

   --------------------------
   -- os_mbx_send_all_task --
   --------------------------

   procedure os_mbx_send_all_task
     (status  : out os_status_t;
      mbx_msg :     os_mbx_msg_t)
   with
      Global => (Input  => (os_task_current.OS_Task_Current_State, os_task_ro.OS_Task_RO_State),
                 In_Out => (os_task_list.OS_Task_State, os_task_mbx.OS_Task_Mbx_State)),
       Pre => os_task_list.os_ghost_task_list_is_well_formed and then
             os_task_list.os_ghost_current_task_is_ready and then
             (for all iterator in os_task_id_param_t'range => os_ghost_task_mbx_are_well_formed (iterator)),
      Post => os_task_list.os_ghost_task_list_is_well_formed and then
              os_task_list.os_ghost_current_task_is_ready
   is
      ret : os_status_t;
   begin
      status := OS_ERROR_DENIED;

      for iterator in os_task_id_param_t'range loop
         pragma Loop_Invariant (os_task_list.os_ghost_task_list_is_well_formed and then
             os_task_list.os_ghost_current_task_is_ready and then
             os_ghost_task_mbx_are_well_formed (iterator));  -- Q: os.adb:187:14: medium: loop invariant might fail after first iteration, cannot prove os_ghost_task_mbx_are_well_formed (iterator)

         os_mbx_send_one_task (ret, iterator, mbx_msg);

         if ret = OS_ERROR_FIFO_FULL then
            status := ret;
         else
            if status /= OS_ERROR_FIFO_FULL then
               if ret = OS_SUCCESS then
                  status := OS_SUCCESS;
               end if;
            end if;
         end if;
      end loop;
   end os_mbx_send_all_task;


   ----------------------
   --  Ghost functions --
   ----------------------

   ----------------------------------
   -- os_ghost_mbx_are_well_formed --
   ----------------------------------

   function os_ghost_mbx_are_well_formed return Boolean is
      (for all task_id in os_task_id_param_t'range =>
          os_task_mbx.os_ghost_task_mbx_are_well_formed (task_id))
   with
      Ghost => true;

   -------------------------------------------------
   -- os_ghost_head_list_task_has_higher_priority --
   -------------------------------------------------

   function os_ghost_head_list_task_has_higher_priority return Boolean is
      (os_sched_get_current_list_head /= OS_TASK_ID_NONE and then
       os_task_list.os_ghost_task_is_ready (os_sched_get_current_list_head) and then
         (for some task_id in os_task_id_param_t'range =>
            os_task_list.os_ghost_task_is_ready (task_id) and then
            os_get_task_priority (task_id)
               <= os_get_task_priority (os_sched_get_current_list_head)))
   with
      Ghost => true;

   ----------------
   -- Public API --
   ----------------

   ----------------------------------
   -- os_sched_get_current_task_id --
   ----------------------------------

   function os_sched_get_current_task_id return os_task_id_param_t
   is (os_task_current.os_sched_get_current_task_id);

   -------------------
   -- os_sched_wait --
   -------------------

   procedure os_sched_wait
     (task_id      : out os_task_id_param_t;
      waiting_mask :     os_mbx_mask_t)
   is
      tmp_mask : os_mbx_mask_t;
   begin
      task_id := os_sched_get_current_task_id;
      pragma assert (os_ghost_task_mbx_are_well_formed (task_id)); -- Q: os.adb:213:22: medium: assertion might fail, cannot prove os_ghost_task_mbx_are_well_formed (task_id), how to prove it since task_id is current task?

      tmp_mask := waiting_mask and os_mbx_get_mbx_permission (task_id);

      --  We remove the current task from the ready list.
      os_sched_remove_task_from_ready_list (task_id);
      pragma assert (os_ghost_task_list_is_well_formed);

      if tmp_mask /= 0 then
         os_mbx_set_waiting_mask (task_id, tmp_mask);
      pragma assert (os_ghost_task_list_is_well_formed); -- Q: os.adb:223:22: medium: assertion might fail, cannot prove (os_ghost_task_list_is_well_formed), how to neutralize action of os_mbx_set_waiting_mask?

         tmp_mask := tmp_mask and os_mbx_get_posted_mask (task_id);

         if tmp_mask /= 0 then
            --  If waited event is already here, put back the task in the
            --  ready list (after tasks of same priority).
            os_sched_add_task_to_ready_list (task_id);
         end if;
      elsif task_id /= OS_INTERRUPT_TASK_ID then
         --  This is an error/illegal case. There is nothing to wait for,
         --  so put back the task in the ready list.
         os_sched_add_task_to_ready_list (task_id);
      end if;

      --  We determine the new task.
      os_sched_schedule (task_id);
   end os_sched_wait;

   --------------------
   -- os_sched_yield --
   --------------------

   procedure os_sched_yield (task_id : out os_task_id_param_t)
   is
   begin
      task_id := os_sched_get_current_task_id;

      --  We remove the current task from the ready list.
      os_sched_remove_task_from_ready_list (task_id);

      --  We insert it back after the other tasks with same priority.
      os_sched_add_task_to_ready_list (task_id);

      --  We determine the new task.
      os_sched_schedule (task_id);
   end os_sched_yield;

   -------------------
   -- os_sched_exit --
   -------------------

   procedure os_sched_exit (task_id : out os_task_id_param_t)
   is
   begin
      task_id := os_sched_get_current_task_id;

      --  Remove the current task from the ready list.
      os_sched_remove_task_from_ready_list (task_id);

      --  We determine the new task.
      os_sched_schedule (task_id);
   end os_sched_exit;

   -------------
   -- os_init --
   -------------

   procedure os_init (task_id : out os_task_id_param_t)
   is
      prev_id : os_task_id_param_t := os_task_id_param_t'First;
   begin
      os_arch_cons_init;

      os_arch_space_init;

      os_sched_set_current_list_head (OS_TASK_ID_NONE);

      for task_iterator in os_task_id_param_t'range loop
         os_arch_space_switch (prev_id, task_iterator);

         os_arch_context_create (task_iterator);

         os_task_mbx_init (task_iterator);
         os_task_init (task_iterator);

         prev_id := task_iterator;

         os_ghost_task_ready_init (task_iterator);
      end loop;

      os_sched_schedule (task_id); -- Q: os.adb:301:07: medium: precondition might fail, cannot prove os_ghost_task_list_is_well_formed for the first time after init

      os_arch_context_set (task_id);

      os_arch_space_switch (prev_id, task_id);
   end os_init;

   --------------------
   -- os_mbx_receive --
   --------------------

   procedure os_mbx_receive
     (status    : out os_status_t;
      mbx_entry : out os_mbx_entry_t)
   is
      --  retrieve current task id
      current        : constant os_task_id_param_t := os_sched_get_current_task_id;
      mbx_index      : os_mbx_index_t;
      next_mbx_index : os_mbx_index_t;
   begin
      mbx_entry.sender_id := OS_TASK_ID_NONE;
      mbx_entry.msg       := 0;

      if os_mbx_is_empty (current) then
         --  mbx queue is empty, so we return with error
         status := OS_ERROR_FIFO_EMPTY;
      else
         --  initialize status to error in case we don't find a mbx.
         status := OS_ERROR_RECEIVE;

         --  Compute the first mbx_index for the loop
         mbx_index := os_mbx_get_mbx_head (current);

         --  go through received mbx for this task
         for iterator in 1 .. os_mbx_get_mbx_count (current) loop
            pragma Loop_Invariant (not os_mbx_is_empty (current));

            --  look into the mbx queue for a mbx that is waited for
            if os_mbx_is_waiting_mbx_entry (current, mbx_index) then

               --  copy the mbx into the task mbx entry
               mbx_entry := os_mbx_get_mbx_entry (current, mbx_index);

               if iterator = 1 then
                  --  if this was the first mbx, we just increase the mbx head
                  os_mbx_inc_mbx_head (current);
               elsif iterator < os_mbx_get_mbx_count (current) then
                  --  in other case, for now we "compact" the rest of the mbx
                  --  queue, so that there is no "hole" in it for the next mbx
                  --  search.
                  for iterator2 in os_mbx_count_t'Succ (iterator) ..
                          os_mbx_get_mbx_count (current)
                  loop
                     pragma Loop_Invariant (not os_mbx_is_empty (current));
                     next_mbx_index := os_mbx_index_t'Succ (mbx_index);
                     os_mbx_set_mbx_entry
                       (current,
                        mbx_index,
                        os_mbx_get_mbx_entry (current, next_mbx_index));
                     mbx_index := next_mbx_index;
                  end loop;
               end if;

               --  remove the mbx from the mbx queue and decrement the mbx count
               os_mbx_remove_mbx_entry (current, mbx_index);

               --  We found a matching mbx
               status := OS_SUCCESS;
               exit;
            end if;
            --  Compute the next mbx_index for the loop
            mbx_index := os_mbx_index_t'Succ(mbx_index);
         end loop;
      end if;
   end os_mbx_receive;

   -----------------
   -- os_mbx_send --
   -----------------

   procedure os_mbx_send
     (status  : out os_status_t;
      dest_id :     types.int8_t;
      mbx_msg :     os_mbx_msg_t)
   is
      -- dest_id comes from uncontroled C calls
   begin
      if dest_id = OS_TASK_ID_ALL then
         os_mbx_send_all_task (status, mbx_msg);
      elsif dest_id in os_task_id_param_t then
         os_mbx_send_one_task (status, dest_id, mbx_msg);
      else
         status := OS_ERROR_PARAM;
      end if;
   end os_mbx_send;

end os;
