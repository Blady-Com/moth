with Ada.Text_IO; use Ada.Text_IO;
package body os_arch is

   --------------------------
   -- interrupt_is_pending --
   --------------------------

   function interrupt_is_pending return types.uint8_t is
   begin
      Put_Line ("interrupt_is_pending");
      return 0;
   end interrupt_is_pending;

   ----------
   -- idle --
   ----------

   procedure idle is
   begin
      Put_Line ("idle");
   end idle;

   --------------------
   -- context_create --
   --------------------

   procedure context_create (task_id : Moth.os_task_id_param_t) is
   begin
      Put_Line ("context_create" & task_id'Image);
   end context_create;

   --------------------
   -- context_switch --
   --------------------

   procedure context_switch (prev_id : Moth.os_task_id_param_t;
      next_id                        : Moth.os_task_id_param_t)
   is
   begin
      Put_Line ("context_switch" & prev_id'Image & next_id'Image);
   end context_switch;

   -----------------
   -- context_set --
   -----------------

   procedure context_set (task_id : Moth.os_task_id_param_t) is
   begin
      Put_Line ("context_set" & task_id'Image);
   end context_set;

   ----------------
   -- space_init --
   ----------------

   procedure space_init is
   begin
      Put_Line ("space_init");
   end space_init;

   ------------------
   -- space_switch --
   ------------------

   procedure space_switch (old_context_id : Moth.os_task_id_param_t;
      new_context_id                      : Moth.os_task_id_param_t)
   is
   begin
      Put_Line ("space_switch" & old_context_id'Image & new_context_id'Image);
   end space_switch;

   ---------------
   -- cons_init --
   ---------------

   procedure cons_init is
   begin
      Put_Line ("cons_init");
   end cons_init;

end os_arch;
