with Ada.Text_IO; use Ada.Text_IO;
with Moth.Mailbox;
with Moth.Current;
with Moth.Scheduler;

procedure Test01 is

   TID : Moth.os_task_id_param_t;
   ST  : Moth.os_status_t;
   MBE : moth.Mailbox.os_mbx_entry_t;

begin
   Moth.init (TID);
   Put_Line (TID'Image);
   Moth.Current.set_current_task_id (3);
   Moth.Scheduler.wait (TID, moth.OS_MBX_MASK_ALL);
   Put_Line (TID'Image);
   Moth.Current.set_current_task_id (0);
   Moth.Mailbox.send (ST, 3, 99);
   Put_Line (ST'Image);
   Moth.Current.set_current_task_id (3);
   moth.Mailbox.receive (ST, MBE);
   Put_Line (ST'Image & MBE.sender_id'Image & MBE.msg'Image);
end Test01;
