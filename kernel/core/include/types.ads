with Interfaces.C; use Interfaces.C;
with Interfaces.C.Extensions;
with OpenConf;

package types with
   Spark_Mode => On
 is

   --
   --  Copyright (c) 2017 Jean-Christophe Dubois All rights reserved.
   --
   --  This program is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as
   --  published by the Free Software Foundation; either version 2, or (at your option) any later version.
   --
   --  This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty
   --  of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
   --
   --  You should have received a copy of the GNU General Public License along with this program; if not, write to the Free Software
   --  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
   --
   --  @file
   --  @author Jean-Christophe Dubois (jcd@tribudubois.net) @brief
   --

   subtype uint8_t is unsigned_char;
   function "=" (R, L : uint8_t) return Boolean renames Interfaces.C."=";
   function ">" (R, L : uint8_t) return Boolean renames Interfaces.C.">";
   function "<" (R, L : uint8_t) return Boolean renames Interfaces.C."<";
   function "<=" (R, L : uint8_t) return Boolean renames Interfaces.C."<=";
   function "+" (R, L : uint8_t) return uint8_t renames Interfaces.C."+";
   function "-" (R, L : uint8_t) return uint8_t renames Interfaces.C."-";

   subtype uint16_t is unsigned_short;

   subtype uint32_t is unsigned;
   function "=" (R, L : uint32_t) return Boolean renames Interfaces.C."=";
   function "and" (R, L : uint32_t) return uint32_t renames Interfaces.C."and";
   function "or" (R, L : uint32_t) return uint32_t renames Interfaces.C."or";

   subtype uint64_t is Extensions.unsigned_long_long;

   subtype int8_t is signed_char;
   function "=" (R, L : int8_t) return Boolean renames Interfaces.C."=";

   subtype int16_t is short;

   subtype int32_t is int;
   function "=" (R, L : int32_t) return Boolean renames Interfaces.C."=";

   subtype int64_t is Long_Long_Integer;

   subtype size_t is unsigned;

   subtype intptr_t is unsigned_long;

   -- Global types

   subtype os_priority_t is types.uint8_t;

   subtype os_mbx_mask_t is types.uint32_t;

   OS_INTERRUPT_TASK_ID : constant := 0;

   OS_TASK_ID_NONE : constant := -1;
   OS_TASK_ID_ALL  : constant := -2;

   OS_MAX_TASK_CNT : constant := OpenConf.CONFIG_MAX_TASK_COUNT;
   OS_MAX_TASK_ID  : constant := OS_MAX_TASK_CNT - 1;
   OS_MIN_TASK_ID  : constant := 0;

   subtype os_task_dest_id_t is types.int8_t range OS_TASK_ID_ALL .. OS_MAX_TASK_ID;

   subtype os_task_id_t is os_task_dest_id_t range OS_TASK_ID_NONE .. OS_MAX_TASK_ID;

   subtype os_task_id_param_t is os_task_id_t range OS_MIN_TASK_ID .. OS_MAX_TASK_ID;

end types;
