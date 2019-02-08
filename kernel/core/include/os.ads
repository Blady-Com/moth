--
--  Copyright (c) 2017 Jean-Christophe Dubois All rights reserved.
--
--  This program is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 2, or (at your option) any later version.
--
--  This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License along with this program; if not, write to the Free Software
--  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  --
--  @file
--  @author Jean-Christophe Dubois (jcd@tribudubois.net) @brief
--

with types;        use types;
with os_task_list; use os_task_list;
with os_task_mbx;  use os_task_mbx;

package os with
   SPARK_mode => On
 is

   OS_MBX_MASK_ALL : constant := 16#ffffffff#;

   OS_SUCCESS          : constant := 0;
   OS_ERROR_FIFO_FULL  : constant := -1;
   OS_ERROR_FIFO_EMPTY : constant := -2;
   OS_ERROR_DENIED     : constant := -3;
   OS_ERROR_RECEIVE    : constant := -4;
   OS_ERROR_PARAM      : constant := -5;
   OS_ERROR_MAX        : constant := OS_ERROR_PARAM;

   subtype os_status_t is types.int32_t range OS_ERROR_MAX .. OS_SUCCESS;

   function os_ghost_mbx_are_well_formed return Boolean with
      Ghost => true;

   function os_sched_get_current_task_id return os_task_id_param_t;
   pragma Export (C, os_sched_get_current_task_id, "os_sched_get_current_task_id");

   procedure os_sched_wait (task_id : out os_task_id_param_t; waiting_mask : os_mbx_mask_t) with
      Pre  => os_ghost_task_list_is_well_formed and os_ghost_mbx_are_well_formed and os_ghost_current_task_is_ready,
      Post => os_ghost_task_list_is_well_formed and os_ghost_task_is_ready (task_id);
   pragma Export (C, os_sched_wait, "os_sched_wait");

   procedure os_sched_yield (task_id : out os_task_id_param_t) with
      Pre  => os_ghost_task_list_is_well_formed and os_ghost_current_task_is_ready,
      Post => os_ghost_task_list_is_well_formed and os_ghost_task_is_ready (task_id);
   pragma Export (C, os_sched_yield, "os_sched_yield");

   procedure os_sched_exit (task_id : out os_task_id_param_t) with
      Pre  => os_ghost_task_list_is_well_formed and os_ghost_current_task_is_ready,
      Post => os_ghost_task_list_is_well_formed and os_ghost_task_is_ready (task_id);
   pragma Export (C, os_sched_exit, "os_sched_exit");

   procedure os_init (task_id : out os_task_id_param_t) with
      Post => os_ghost_task_list_is_well_formed and os_ghost_task_is_ready (task_id);
   pragma Export (C, os_init, "os_init");

   procedure os_mbx_receive (status : out os_status_t; mbx_entry : out os_mbx_entry_t) with
      Pre  => os_ghost_task_list_is_well_formed and os_ghost_mbx_are_well_formed and os_ghost_current_task_is_ready,
      Post => os_ghost_task_list_is_well_formed and os_ghost_current_task_is_ready;
   pragma Export (C, os_mbx_receive, "os_mbx_receive");

   procedure os_mbx_send (status : out os_status_t; dest_id : types.int8_t; mbx_msg : os_mbx_msg_t) with
      Pre  => os_ghost_task_list_is_well_formed and os_ghost_mbx_are_well_formed and os_ghost_current_task_is_ready,
      Post => os_ghost_task_list_is_well_formed and os_ghost_current_task_is_ready;
   pragma Export (C, os_mbx_send, "os_mbx_send");

end os;
