with Interfaces.C;
with types;
with OpenConf;
with os_task_list; use os_task_list;

package os_task_mbx with
     Abstract_State => Os_Task_Mbx_State is

   use type Interfaces.C.signed_char;
   use type Interfaces.C.unsigned_char;

   OS_MAX_MBX_CNT : constant := OpenConf.CONFIG_TASK_MBX_COUNT;
   OS_MAX_MBX_ID  : constant := OS_MAX_MBX_CNT - 1;

   OS_MBX_MSG_SZ : constant := OpenConf.CONFIG_MBX_SIZE;

   type os_mbx_msg_t is range 0 .. 2**OS_MBX_MSG_SZ - 1;
   for os_mbx_msg_t'Size use OS_MBX_MSG_SZ;

   type os_mbx_entry_t is record
      sender_id : os_task_id_t;
      msg       : os_mbx_msg_t;
   end record;
   pragma Convention (C_Pass_By_Copy, os_mbx_entry_t);

   type os_mbx_index_t is mod OS_MAX_MBX_ID;

   subtype os_mbx_count_t is types.uint8_t range 0 .. OS_MAX_MBX_CNT;

   --  Check if the mbx fifo of a given task is empty.

   function os_mbx_is_empty (task_id : os_task_id_param_t) return Boolean;

   --  Check if the mbx fifo of a given task is full.

   function os_mbx_is_full (task_id : os_task_id_param_t) return Boolean with
      Global => (Input => Os_Task_Mbx_State),
      Post   => os_ghost_task_mbx_are_well_formed (task_id);

      --  Retrieve the mbx head index of the given task.

   function os_mbx_get_mbx_head
     (task_id : os_task_id_param_t) return os_mbx_index_t;

   --  Increment the mbx head index of the given task. No contract, it will be
   --  inlined

   procedure os_mbx_inc_mbx_head (task_id : os_task_id_param_t) with
      Pre  => not os_mbx_is_empty (task_id),
      Post => not os_mbx_is_empty (task_id);

      --  Retrieve the mbx count of the given task.

   function os_mbx_get_mbx_count
     (task_id : os_task_id_param_t) return os_mbx_count_t;

   --  Retrieve the mbx tail index of the given task.

   function os_ghost_get_mbx_tail
     (task_id : os_task_id_param_t) return os_mbx_index_t with
      Pre   => not os_mbx_is_empty (task_id),
      Ghost => True;

      --  Increment the mbx count of the given task.

   procedure os_mbx_inc_mbx_count (task_id : os_task_id_param_t) with
      Pre  => (not os_mbx_is_full (task_id)),
      Post =>
      (os_mbx_get_mbx_count (task_id) =
       os_mbx_get_mbx_count (task_id)'Old + 1);

      --  Decrement the mbx count of the given task.

   procedure os_mbx_dec_mbx_count (task_id : os_task_id_param_t) with
      Pre  => not os_mbx_is_empty (task_id),
      Post =>
      (os_mbx_get_mbx_count (task_id) =
       os_mbx_get_mbx_count (task_id)'Old - 1);

      --  Retrieve the tail mbx sender of the given task.
   function os_mbx_get_tail_sender
     (task_id : os_task_id_param_t) return os_task_id_t;

   --  Add a mbx to the mbx fifo of a given task.

   procedure os_mbx_add_message
     (dest_id : os_task_id_param_t;
      src_id  : os_task_id_param_t;
      mbx_msg : os_mbx_msg_t) with
      Pre =>
      ((not os_mbx_is_full (dest_id)) and
       os_ghost_task_mbx_are_well_formed (dest_id)),
      Post =>
      ((not os_mbx_is_empty (dest_id)) and
       ((
         (os_mbx_get_mbx_count (dest_id) =
          os_mbx_get_mbx_count (dest_id)'Old + 1) and
         (os_mbx_get_mbx_head (dest_id) =
          os_mbx_get_mbx_head (dest_id)'Old) and
         (os_mbx_get_tail_sender (dest_id) = src_id))));

   procedure os_mbx_clear_mbx_entry
     (task_id   : os_task_id_param_t;
      mbx_index : os_mbx_index_t) with
      Pre  => not os_mbx_is_empty (task_id),
      Post => not os_mbx_is_empty (task_id);

   procedure os_mbx_set_mbx_entry
     (task_id   : os_task_id_param_t;
      mbx_index : os_mbx_index_t;
      mbx_entry : os_mbx_entry_t) with
      Pre  => not os_mbx_is_empty (task_id),
      Post => not os_mbx_is_empty (task_id);

   function os_mbx_get_mbx_entry
     (task_id   : os_task_id_param_t;
      mbx_index : os_mbx_index_t) return os_mbx_entry_t;

   function os_mbx_get_mbx_entry_sender
     (task_id   : os_task_id_param_t;
      mbx_index : os_mbx_index_t) return os_task_id_param_t;

   function os_mbx_is_waiting_mbx_entry
     (task_id   : os_task_id_param_t;
      mbx_index : os_mbx_index_t) return Boolean;

   procedure os_task_mbx_init (task_id : os_task_id_param_t);

   --  MBX are circular FIFO (contained in an array) where head is the index of
   --  the fisrt element of the FIFO and count is the number of element stored
   --  in the FIFO. When an element of the FIFO is filled its sender_id field
   --  needs to be >= 0. When an element in the circular FIFO is empty, its
   --  sender_if field is -1 (OS_TASK_ID_NONE). So this condition makes sure
   --  that all non empty element of the circular FIFO have sender_id >= 0 and
   --  empty elements of the FIFO have sender_id = -1.

   function os_ghost_task_mbx_are_well_formed
     (task_id : os_task_id_param_t) return Boolean with
      Ghost => True;

end os_task_mbx;
