------------------------------------------------------------------------------
--                                                                          --
--                     Copyright (C) 2018, Adacore                          --
--                                                                          --
--  Redistribution and use in source and binary forms, with or without      --
--  modification, are permitted provided that the following conditions are  --
--  met:                                                                    --
--     1. Redistributions of source code must retain the above copyright    --
--        notice, this list of conditions and the following disclaimer.     --
--     2. Redistributions in binary form must reproduce the above copyright --
--        notice, this list of conditions and the following disclaimer in   --
--        the documentation and/or other materials provided with the        --
--        distribution.                                                     --
--     3. Neither the name of the copyright holder nor the names of its     --
--        contributors may be used to endorse or promote products derived   --
--        from this software without specific prior written permission.     --
--                                                                          --
--   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS    --
--   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT      --
--   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR  --
--   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT   --
--   HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, --
--   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT       --
--   LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,  --
--   DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY  --
--   THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT    --
--   (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE  --
--   OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.   --
--                                                                          --
------------------------------------------------------------------------------

with HAL;
with System;

with Interfaces; use Interfaces;
with Interfaces.C; use Interfaces.C;

package IOCTL is

   type Direction is
     (None,
      Write,
      Read)
   with Size => 2;
   for Direction use
     (None  => 2#00#,
      Write  => 2#01#,
      Read => 2#10#);

   type Request is record
      dir: Direction;
      typ: HAL.UInt8;
      nr: HAL.UInt8;
      size: HAL.UInt14;
   end record
   with Size => 32,
   Bit_Order => System.Low_Order_First;
   --for Request use record
   --   dir at 0 range 0..1;
   --   typ at 0 range 2..9;
   --   nr at 0 range 10..17;
   --   size at 0 range 18..31;
   --end record;

   for Request use record
      dir at 0 range 30..31;
      typ at 0 range 8..15;
      nr at 0 range 0..7;
      size at 0 range 16..29;
   end record;
   --pragma Pack (Request);

   type File_Id is new int;
   type File_Mode is new int;
   type Size is new Long_Integer;

   -- FIXME: Is Errno on 64 Bit machines 32 Bit ot 64 Bit?
   Err_No : Unsigned_32;
   pragma Thread_Local_Storage (Err_No);
   pragma Import (C, Err_No, "errno");


   function Open (Path : String;
                  Flags : int;
                  Mode : File_Mode)
                  return File_Id;
   pragma Import(C, open, "open");

   function Read (File_Desc : File_Id;
                  Data : System.Address;
                  Length : Size)
                  return Size;
   pragma Import(C, read, "read");

   function Write (File_Desc : File_Id;
                   Data : System.Address;
                   Length : Size)
                   return Size;
   pragma Import(C, write, "write");

end IOCTL;
