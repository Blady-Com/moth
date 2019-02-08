with Interfaces; use Interfaces;
--  with os_task_list;

package body os_task_mbx with
   Spark_Mode    => On,
   Refined_State => (Os_Task_Mbx_State => os_task_mbx_rw)
 is

   type os_mbx_t_array is array (os_mbx_index_t) of os_mbx_entry_t;

   type os_mbx_t is record
      head      : os_mbx_index_t;
      count     : os_mbx_count_t;
      mbx_array : os_mbx_t_array;
   end record;

   ---------------------
   -- os_task_mbx_rw --
   ---------------------

   os_task_mbx_rw : array (os_task_id_param_t) of os_mbx_t;

   ----------------------
   -- + os_mbx_index_t --
   ----------------------

   function "+" (Left : os_mbx_index_t; Right : os_mbx_count_t) return os_mbx_index_t is (Left + os_mbx_index_t'Mod (Right));

   ---------------------
   -- os_mbx_is_empty --
   ---------------------

   function os_mbx_is_empty (task_id : os_task_id_param_t) return Boolean is
     (os_task_mbx_rw (task_id).count = os_mbx_count_t'First) with
      Refined_Global  => (Input => os_task_mbx_rw),
      Refined_Depends => (os_mbx_is_empty'Result => (os_task_mbx_rw, task_id));

      --------------------
      -- os_mbx_is_full --
      --------------------

   function os_mbx_is_full (task_id : os_task_id_param_t) return Boolean is
     (os_task_mbx_rw (task_id).count = os_mbx_count_t'Last) with
      Refined_Global  => (Input => os_task_mbx_rw),
      Refined_Depends => (os_mbx_is_full'Result => (os_task_mbx_rw, task_id));

      -------------------------
      -- os_mbx_get_mbx_head --
      -------------------------

   function os_mbx_get_mbx_head (task_id : os_task_id_param_t) return os_mbx_index_t is (os_task_mbx_rw (task_id).head) with
      Refined_Global  => (Input => os_task_mbx_rw),
      Refined_Depends => (os_mbx_get_mbx_head'Result => (os_task_mbx_rw, task_id));

      -------------------------
      -- os_mbx_inc_mbx_head --
      -------------------------

   procedure os_mbx_inc_mbx_head (task_id : os_task_id_param_t) with
      Refined_Global  => (In_Out => os_task_mbx_rw),
      Refined_Depends => (os_task_mbx_rw =>+ task_id)
    is
   begin
      os_task_mbx_rw (task_id).head := os_mbx_index_t'Succ (os_mbx_get_mbx_head (task_id));
   end os_mbx_inc_mbx_head;

   --------------------------
   -- os_mbx_get_mbx_count --
   --------------------------

   function os_mbx_get_mbx_count (task_id : os_task_id_param_t) return os_mbx_count_t is (os_task_mbx_rw (task_id).count) with
      Refined_Global  => (Input => os_task_mbx_rw),
      Refined_Depends => (os_mbx_get_mbx_count'Result => (os_task_mbx_rw, task_id));

      ---------------------------
      -- os_ghost_get_mbx_tail --
      ---------------------------

   function os_ghost_get_mbx_tail (task_id : os_task_id_param_t) return os_mbx_index_t is
     (os_mbx_get_mbx_head (task_id) + os_mbx_count_t'Pred (os_mbx_get_mbx_count (task_id))) with
      Refined_Global  => (Input => os_task_mbx_rw),
      Refined_Depends => (os_ghost_get_mbx_tail'Result => (os_task_mbx_rw, task_id));

      --------------------------
      -- os_mbx_inc_mbx_count --
      --------------------------

   procedure os_mbx_inc_mbx_count (task_id : os_task_id_param_t) with
      Refined_Global  => (In_Out => os_task_mbx_rw),
      Refined_Depends => (os_task_mbx_rw =>+ task_id)
    is
   begin
      os_task_mbx_rw (task_id).count := os_mbx_count_t'Succ (os_mbx_get_mbx_count (task_id));
   end os_mbx_inc_mbx_count;

   --------------------------
   -- os_mbx_dec_mbx_count --
   --------------------------

   procedure os_mbx_dec_mbx_count (task_id : os_task_id_param_t) with
      Refined_Global  => (In_Out => os_task_mbx_rw),
      Refined_Depends => (os_task_mbx_rw =>+ task_id)
    is
   begin
      os_task_mbx_rw (task_id).count := os_mbx_count_t'Pred (os_mbx_get_mbx_count (task_id));
   end os_mbx_dec_mbx_count;

   -----------------------
   -- os_mbx_remove_mbx --
   -----------------------

   procedure os_mbx_remove_mbx_entry (task_id : os_task_id_param_t; mbx_index : os_mbx_index_t) with
      Refined_Global  => (In_Out => os_task_mbx_rw),
      Refined_Depends => (os_task_mbx_rw =>+ (mbx_index, task_id))
    is
   begin
      os_task_mbx_rw (task_id).mbx_array (mbx_index).sender_id :=  -- call clear ?
        OS_TASK_ID_NONE;
      os_task_mbx_rw (task_id).mbx_array (mbx_index).msg := 0;
      os_mbx_dec_mbx_count (task_id);
   end os_mbx_remove_mbx_entry;

   ------------------------
   -- os_mbx_add_message --
   ------------------------

   procedure os_mbx_add_message (dest_id : os_task_id_param_t; src_id : os_task_id_param_t; mbx_msg : os_mbx_msg_t) with
      Refined_Global  => (In_Out => os_task_mbx_rw),
      Refined_Depends => (os_task_mbx_rw =>+ (dest_id, src_id, mbx_msg))
    is
      mbx_index : constant os_mbx_index_t := os_mbx_get_mbx_head (dest_id) + os_mbx_get_mbx_count (dest_id);
   begin
      os_task_mbx_rw (dest_id).mbx_array (mbx_index).sender_id := src_id;
      os_task_mbx_rw (dest_id).mbx_array (mbx_index).msg       := mbx_msg;
      os_mbx_inc_mbx_count (dest_id);
   end os_mbx_add_message;

   function os_mbx_get_tail_sender (task_id : os_task_id_param_t) return os_task_id_t is
     (os_task_mbx_rw (task_id).mbx_array (os_mbx_get_mbx_head (task_id) + os_mbx_get_mbx_count (task_id)).sender_id) with
      Refined_Global  => (Input => os_task_mbx_rw),
      Refined_Depends => (os_mbx_get_tail_sender'Result => (os_task_mbx_rw, task_id));

      ----------------------------
      -- os_mbx_clear_mbx_entry --
      ----------------------------

--     procedure os_mbx_clear_mbx_entry
--       (task_id   : os_task_id_param_t;
--        mbx_index : os_mbx_index_t) with post =>  os_mbx_get_mbx_entry (task_id, mbx_index) = (OS_TASK_ID_NONE, 0) is
--     begin
--        os_task_mbx_rw (task_id).mbx_array (mbx_index).sender_id :=
--          OS_TASK_ID_NONE;
--        os_task_mbx_rw (task_id).mbx_array (mbx_index).msg := 0;
--     end os_mbx_clear_mbx_entry;

      --------------------------
      -- os_mbx_set_mbx_entry --
      --------------------------

   procedure os_mbx_set_mbx_entry (task_id : os_task_id_param_t; mbx_index : os_mbx_index_t; mbx_entry : os_mbx_entry_t) with
      Refined_Global  => (In_Out => os_task_mbx_rw),
      Refined_Depends => (os_task_mbx_rw =>+ (task_id, mbx_index, mbx_entry))
    is
   begin
      os_task_mbx_rw (task_id).mbx_array (mbx_index) := mbx_entry;
   end os_mbx_set_mbx_entry;

   --------------------------
   -- os_mbx_get_mbx_entry --
   --------------------------

   function os_mbx_get_mbx_entry (task_id : os_task_id_param_t; mbx_index : os_mbx_index_t) return os_mbx_entry_t is
     (os_task_mbx_rw (task_id).mbx_array (mbx_index)) with
      Refined_Global  => (Input => os_task_mbx_rw),
      Refined_Depends => (os_mbx_get_mbx_entry'Result => (os_task_mbx_rw, task_id, mbx_index));

      ---------------------------------
      -- os_mbx_get_mbx_entry_sender --
      ---------------------------------

   function os_mbx_get_mbx_entry_sender (task_id : os_task_id_param_t; mbx_index : os_mbx_index_t) return os_task_id_t is
     (os_task_mbx_rw (task_id).mbx_array (mbx_index).sender_id) with
      Refined_Global  => (Input => os_task_mbx_rw),
      Refined_Depends => (os_mbx_get_mbx_entry_sender'Result => (os_task_mbx_rw, task_id, mbx_index));

      ---------------------------------
      -- os_mbx_is_waiting_mbx_entry --
      ---------------------------------

   function os_mbx_is_waiting_mbx_entry (task_id : os_task_id_param_t; mbx_index : os_mbx_index_t) return Boolean is
     (os_mbx_get_mbx_entry_sender (task_id, mbx_index) in os_task_id_param_t
      and then
        (os_task_list.os_mbx_get_waiting_mask (task_id) and
         os_mbx_mask_t (Shift_Left (Unsigned_32'(1), Natural (os_mbx_get_mbx_entry_sender (task_id, mbx_index))))) /=
        0) with
      Refined_Global  => (Input => (os_task_list.Os_Task_State, os_task_mbx_rw)),
      Refined_Depends => (os_mbx_is_waiting_mbx_entry'Result => (os_task_list.Os_Task_State, os_task_mbx_rw, task_id, mbx_index));

      ----------------------
      -- os_task_mbx_init --
      ----------------------

   procedure os_task_mbx_init (task_id : os_task_id_param_t) with
      Refined_Global  => (In_Out => os_task_mbx_rw),
      Refined_Depends => (os_task_mbx_rw =>+ task_id)
    is
   begin
      os_task_mbx_rw (task_id).head  := 0;
      os_task_mbx_rw (task_id).count := 0;
      for mbx_iterator in os_mbx_index_t'Range loop
         os_task_mbx_rw (task_id).mbx_array (mbx_iterator).sender_id := OS_TASK_ID_NONE;
         os_task_mbx_rw (task_id).mbx_array (mbx_iterator).msg       := 0;
      end loop;
   end os_task_mbx_init;

   ---------------------------------------
   -- os_ghost_task_mbx_are_well_formed --
   ---------------------------------------
   --  MBX are circular FIFO (contained in an array) where head is the index of the fisrt element of the FIFO and count is the
   --  number of element stored in the FIFO. When an element of the FIFO is filled its sender_id field needs to be >= 0. When an
   --  element in the circular FIFO is empty, its sender_if field is -1 (OS_TASK_ID_NONE). So this condition makes sure that all
   --  non empty element of the circular FIFO have sender_id >= 0 and empty elements of the FIFO have sender_id = -1.

   function os_ghost_task_mbx_are_well_formed (task_id : os_task_id_param_t) return Boolean is
     (for all index in os_mbx_index_t'Range =>
        (if
           (((not os_mbx_is_empty (task_id)) and then (os_ghost_get_mbx_tail (task_id) < os_mbx_get_mbx_head (task_id))
             and then ((index >= os_mbx_get_mbx_head (task_id)) or (index <= os_ghost_get_mbx_tail (task_id))))
            or else
            ((not os_mbx_is_empty (task_id)) and then (index in os_mbx_get_mbx_head (task_id) .. os_ghost_get_mbx_tail (task_id))))
         then os_task_mbx_rw (task_id).mbx_array (index).sender_id in os_task_id_param_t
         else os_task_mbx_rw (task_id).mbx_array (index).sender_id = OS_TASK_ID_NONE)) with
      Refined_Global  => (Input => os_task_mbx_rw),
      Refined_Depends => (os_ghost_task_mbx_are_well_formed'Result => (os_task_mbx_rw, task_id));

end os_task_mbx;
