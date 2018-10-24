------------------------------------------------------------------------------
--                                                                          --
--                  Copyright (C) 2018, AdaCore                        --
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

with Ada.Unchecked_Conversion;
package body BMP280.I2C is

   function Rep is new Ada.Unchecked_Conversion (SDO_Pin, UInt8);

   overriding
   procedure Read_Port (This : I2C_BMP280_Device;
                        Address : UInt8;
                        Data : out Byte_Array) is

      Status : I2C_Status;

      Communication_Error : exception;

   begin
      This.Port.Mem_Read (UInt10 (Rep (This.SDO)), UInt16 (Address),
                          Memory_Size_8b, I2C_Data (Data (Data'Range)),
                          Status);

      if Status /= Ok then
         raise Communication_Error;
      end if;
   end Read_Port;

   overriding
   procedure Write_Port (This : I2C_BMP280_Device;
                         Address : UInt8;
                         Data : UInt8) is

      Port_Data : constant I2C_Data (1 .. 1) := (1 => Data);
      Status : I2C_Status;

      Communication_Error : exception;
   begin
      This.Port.Mem_Write (UInt10 (Rep (This.SDO)), UInt16 (Address),
                          Memory_Size_8b, Port_Data, Status);

      if Status /= Ok then
         raise Communication_Error;
      end if;
   end Write_Port;


end BMP280.I2C;
