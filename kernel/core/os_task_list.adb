with os_task_ro; use os_task_ro;
with os_task_current;

package body os_task_list with
   Spark_Mode    => On,
   Refined_State =>
   (Os_Task_State =>
      (os_task_list_rw, os_task_ready_list_head, os_ghost_task_ready))
 is

   type os_task_rw_t is record
      next             : os_task_id_t;
      prev             : os_task_id_t;
      mbx_waiting_mask : os_mbx_mask_t;
   end record;

   ---------------------
   -- os_task_list_rw --
   ---------------------

   os_task_list_rw : array (os_task_id_param_t) of os_task_rw_t;

   -----------------------------
   -- os_task_ready_list_head --
   -----------------------------
   --  This variable holds the ID of the first task in the ready list (the next
   --  one that will be elected). Note: Its value could be OS_TASK_ID_NONE if
   --  no task is ready.

   os_task_ready_list_head : os_task_id_t;

   -------------------------------------
   -- Ghost variable for task's state --
   -------------------------------------

   os_ghost_task_ready : array (os_task_id_param_t) of Boolean with
      Ghost => True;

      ------------------
      -- os_task_init --
      ------------------

   procedure os_task_init (task_id : os_task_id_param_t) with
      Refined_Global => (In_Out => os_task_list_rw,
       input => os_ghost_task_ready),
      Refined_Depends => (os_task_list_rw =>+ (task_id, os_ghost_task_ready))
    is
   begin
      os_task_list_rw (task_id).mbx_waiting_mask := 0;
      os_task_list_rw (task_id).next             := OS_TASK_ID_NONE;
      os_task_list_rw (task_id).prev             := OS_TASK_ID_NONE;
      os_sched_add_task_to_ready_list (task_id);
   end os_task_init;

   ------------------------------
   -- os_ghost_task_ready_init --
   ------------------------------

   procedure os_ghost_task_ready_init (task_id : os_task_id_param_t) with
      Refined_Global  => (In_Out => os_ghost_task_ready),
      Refined_Depends => (os_ghost_task_ready =>+ task_id)
    is
   begin
      os_ghost_task_ready (task_id) := False;
   end os_ghost_task_ready_init;

   ----------------------------
   -- os_ghost_task_is_ready --
   ----------------------------

   function os_ghost_task_is_ready
     (task_id : os_task_id_param_t) return Boolean is
     (os_ghost_task_ready (task_id)) with
      Refined_Global  => (Input => os_ghost_task_ready),
      Refined_Depends =>
      (os_ghost_task_is_ready'Result => (os_ghost_task_ready, task_id));

      ------------------------------------
      -- os_ghost_current_task_is_ready --
      ------------------------------------

   function os_ghost_current_task_is_ready return Boolean is
     (os_ghost_task_is_ready
        (os_task_current.os_sched_get_current_task_id)) with
      Refined_Global  => (Input => os_ghost_task_ready),
      Refined_Depends =>
      (os_ghost_current_task_is_ready'Result => os_ghost_task_ready);

      ------------------------------------
      -- os_sched_get_current_list_head --
      ------------------------------------

   function os_sched_get_current_list_head return os_task_id_t is
     (os_task_ready_list_head) with
      Refined_Global  => (Input => os_task_ready_list_head),
      Refined_Depends =>
      (os_sched_get_current_list_head'Result => os_task_ready_list_head);

      ------------------------------------
      -- os_sched_set_current_list_head --
      ------------------------------------

   procedure os_sched_set_current_list_head (task_id : os_task_id_t) with
      Refined_Global  => (In_Out => os_task_ready_list_head),
      Refined_Depends => (os_task_ready_list_head =>+ task_id)
    is
   begin
      os_task_ready_list_head := task_id;
   end os_sched_set_current_list_head;

   -----------------------------
   -- os_mbx_get_waiting_mask --
   -----------------------------

   function os_mbx_get_waiting_mask
     (task_id : os_task_id_param_t) return os_mbx_mask_t is
     (os_task_list_rw (task_id).mbx_waiting_mask) with
      Refined_Global  => (Input => os_task_list_rw),
      Refined_Depends =>
      (os_mbx_get_waiting_mask'Result => (os_task_list_rw, task_id));

      -----------------------------
      -- os_mbx_set_waiting_mask --
      -----------------------------

   procedure os_mbx_set_waiting_mask (task_id : os_task_id_param_t;
      mask                                    : os_mbx_mask_t)
   is
   begin
      os_task_list_rw (task_id).mbx_waiting_mask := mask;
   end os_mbx_set_waiting_mask;

   -------------------------------------
   -- os_sched_add_task_to_ready_list --
   -------------------------------------

   procedure os_sched_add_task_to_ready_list
     (task_id : os_task_id_param_t) with
      Refined_Global  => (In_Out => (os_task_list_rw, os_ghost_task_ready)),
      Refined_Depends =>
      ((os_task_list_rw, os_ghost_task_ready) =>
         + (task_id, os_ghost_task_ready))
    is
      index_id : os_task_id_t := os_sched_get_current_list_head;
   begin

      if index_id = OS_TASK_ID_NONE then
         --  No task in the ready list. Add this task at list head
         os_task_list_rw (task_id).next := OS_TASK_ID_NONE;
         os_task_list_rw (task_id).prev := OS_TASK_ID_NONE;
         os_sched_set_current_list_head (task_id);
      else
         while index_id /= OS_TASK_ID_NONE loop
            if index_id = task_id then
               --  Already in the ready list
               exit;
            elsif os_get_task_priority (task_id) >
              os_get_task_priority (index_id) then
               declare
                  prev_id : constant os_task_id_t :=
                    os_task_list_rw (index_id).prev;
               begin
                  os_task_list_rw (task_id).next  := index_id;
                  os_task_list_rw (index_id).prev := task_id;

                  if index_id = os_sched_get_current_list_head then
                     os_task_list_rw (task_id).prev := OS_TASK_ID_NONE;
                     os_sched_set_current_list_head (task_id);
                  else
                     os_task_list_rw (task_id).prev := prev_id;
                     if prev_id in os_task_id_param_t then
                        os_task_list_rw (prev_id).next := task_id;
                     end if;
                  end if;
                  exit;
               end;
            elsif os_task_list_rw (index_id).next = OS_TASK_ID_NONE then
               os_task_list_rw (index_id).next := task_id;
               os_task_list_rw (task_id).prev  := index_id;
               os_task_list_rw (task_id).next  := OS_TASK_ID_NONE;
               exit;
            else
               index_id := os_task_list_rw (index_id).next;
            end if;
         end loop;
      end if;

      os_ghost_task_ready (task_id) := True;

   end os_sched_add_task_to_ready_list;

   ------------------------------------------
   -- os_sched_remove_task_from_ready_list --
   ------------------------------------------

   procedure os_sched_remove_task_from_ready_list
     (task_id : os_task_id_param_t) with
      Refined_Global  => (In_Out => (os_task_list_rw, os_ghost_task_ready)),
      Refined_Depends =>
      ((os_task_list_rw, os_ghost_task_ready) =>
         + (task_id, os_ghost_task_ready))
    is
      next : constant os_task_id_t := os_task_list_rw (task_id).next;
   begin
      if task_id = os_sched_get_current_list_head then
         --  We are removing the current running task. So put the next task at
         --  list head. Note: there could be no next task (OS_TASK_ID_NONE)
         if next /= OS_TASK_ID_NONE then
            os_task_list_rw (next).prev := OS_TASK_ID_NONE;
         end if;
         os_sched_set_current_list_head (next);
      elsif os_sched_get_current_list_head /= OS_TASK_ID_NONE then
         --  The list is not empty and
         --  The task is not at the list head (it has a predecesor). Link
         --  previous next to our next
         declare
            prev : constant os_task_id_t := os_task_list_rw (task_id).prev;
         begin
            if prev /= OS_TASK_ID_NONE then
               os_task_list_rw (prev).next := next;
            end if;

            if next /= OS_TASK_ID_NONE then
               --  if we have a next, link next previous to our previous.
               os_task_list_rw (next).prev := prev;
            end if;
         end;
      end if;

      --  reset our next and previous
      os_task_list_rw (task_id).next := OS_TASK_ID_NONE;
      os_task_list_rw (task_id).prev := OS_TASK_ID_NONE;

      os_ghost_task_ready (task_id) := False;

   end os_sched_remove_task_from_ready_list;

   --------------------------------------------
   -- os_ghost_at_least_one_terminating_next --
   --------------------------------------------

   function os_ghost_at_least_one_terminating_next return Boolean is
     (for some task_id in os_task_id_param_t'Range =>
        os_task_list_rw (task_id).next = OS_TASK_ID_NONE) with
      Refined_Global  => (Input => os_task_list_rw),
      Refined_Depends =>
      (os_ghost_at_least_one_terminating_next'Result => os_task_list_rw);

      --------------------------------------------
      -- os_ghost_at_least_one_terminating_prev --
      --------------------------------------------

   function os_ghost_at_least_one_terminating_prev return Boolean is
     (for some task_id in os_task_id_param_t'Range =>
        os_task_list_rw (task_id).prev = OS_TASK_ID_NONE) with
      Refined_Global  => (Input => os_task_list_rw),
      Refined_Depends =>
      (os_ghost_at_least_one_terminating_prev'Result => os_task_list_rw);

      ------------------------------
      -- os_ghost_not_next_twice --
      ------------------------------
      --  A task_id should not be twice in next attribute

   function os_ghost_not_next_twice (task_id : os_task_id_t) return Boolean is
     (not
      (for some next_id in os_task_id_param_t'Range =>
         os_task_list_rw (next_id).next = task_id
         and then
         (for some next_id2 in next_id .. os_task_list_rw'Last =>
            os_task_list_rw (next_id2).next = task_id))) with
      Ghost => True;

      ------------------------------
      -- os_ghost_not_prev_twice --
      ------------------------------
      --  A task_id should not be twice in prev attribute

   function os_ghost_not_prev_twice (task_id : os_task_id_t) return Boolean is
     (not
      (for some prev_id in os_task_id_param_t'Range =>
         os_task_list_rw (prev_id).prev = task_id
         and then
         (for some prev_id2 in prev_id .. os_task_list_rw'Last =>
            os_task_list_rw (prev_id2).prev = task_id))) with
      Ghost => True;

      ---------------------------------------
      -- os_ghost_task_list_is_well_formed --
      ---------------------------------------

   function os_ghost_task_list_is_well_formed return Boolean is
      --  The mbx fifo of all tasks need to be well formed.
     (
      --  The list might be empty. This is legal.
      (os_sched_get_current_list_head = OS_TASK_ID_NONE and
      --  then all element are disconnected (not in a list)
       (for all task_id in os_task_id_param_t'Range =>
      -- no next
          os_task_list_rw (task_id).next = OS_TASK_ID_NONE
      -- no prev
       and
          os_task_list_rw (task_id).prev = OS_TASK_ID_NONE
      --  and all tasks are in not ready state
       and
          not (os_ghost_task_is_ready (task_id))))
      or else
      --  If there is at least one task in the list.
      (os_sched_get_current_list_head /= OS_TASK_ID_NONE
      --  the first task of the list should not have any prev task.
       and then os_task_list_rw (os_sched_get_current_list_head).prev =
         OS_TASK_ID_NONE
      --  the first task needs to be in ready state
       and then os_ghost_task_is_ready (os_sched_get_current_list_head)
      --  there need to ba at least one terminating next.
       and then os_ghost_at_least_one_terminating_next
      --  First element is well formed. Go through the list
       and then
       (for all task_id in os_task_id_param_t'Range =>
      --  a task cannot have itself as next.
          os_task_list_rw (task_id).next /= task_id
      --  a task cannot have itself as prev.
          and then os_task_list_rw (task_id).prev /= task_id
      --  a task could not be next more than once
          and then os_ghost_not_next_twice (task_id)
      --  a task could not be prev more than once
          and then os_ghost_not_prev_twice (task_id)
      --  If there is a next
          and then
      --  If a task has a next then it is part of the ready list
          (if os_task_list_rw (task_id).next /= OS_TASK_ID_NONE then
      --  Its next needs to be in ready state
             os_ghost_task_is_ready (os_task_list_rw (task_id).next)
      --  It needs to be in ready state itself
             and then os_ghost_task_is_ready (task_id)
      --  The next needs to have the actual task as prev
             and then os_task_list_rw (os_task_list_rw (task_id).next).prev =
               task_id
      --  prev and next need to be different
             and then os_task_list_rw (task_id).next /=
               os_task_list_rw (task_id).prev
      --  It needs to be ordered on priority
             and then os_get_task_priority (os_task_list_rw (task_id).next) <=
               os_get_task_priority (task_id))))) with
      Refined_Global =>
      (Input =>
         (os_task_list_rw, os_ghost_task_ready, os_task_ready_list_head)),
      Refined_Depends =>
      (os_ghost_task_list_is_well_formed'Result =>
         (os_task_list_rw, os_ghost_task_ready),
       null => os_task_ready_list_head);

end os_task_list;
