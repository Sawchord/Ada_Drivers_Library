------------------------------------------------------------------------------
--                                                                          --
--                    Copyright (C) 2018, AdaCore                           --
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
--     3. Neither the name of STMicroelectronics nor the names of its       --
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

with HAL; use HAL;
with HAL.I2C; use HAL.I2C;
with System;
with System.Address_Image;

with Interfaces;
with Interfaces.C; use Interfaces.C;

with Posix; use Posix;

with Ada.Unchecked_Conversion;

-- TODO: Remove this after finishing Debuging
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

package Native.I2C is

   type I2C_Port is new HAL.I2C.I2C_Port with private;

   -- TODO: Check whether this is even supported by I2CDev
   type I2C_Acknowledgement is (Ack_Disable, Ack_Enable);

   type I2C_Addressing_Mode is
     (Addressing_Mode_7bit,
      Addressing_Mode_10bit);

   type I2C_Device_Mode is
     (I2C_Mode,
      SMBusDevice_Mode,
      SMBusHost_Mode);

   type I2C_Configuration is record
      Addressing_Mode : I2C_Addressing_Mode;
      Ack : I2C_Acknowledgement;
      Device_Mode : I2C_Device_Mode;
   end record;

   function Configure (Device : String;
                       Conf : I2C_Configuration;
                       Status : out HAL.I2C.I2C_Status)
                       return I2C_Port;

   overriding
   procedure Master_Transmit
     (This    : in out I2C_Port;
      Addr    : HAL.I2C.I2C_Address;
      Data    : HAL.I2C.I2C_Data;
      Status  : out HAL.I2C.I2C_Status;
      Timeout : Natural := 1000);

   overriding
   procedure Master_Receive
     (This    : in out I2C_Port;
      Addr    : HAL.I2C.I2C_Address;
      Data    : out HAL.I2C.I2C_Data;
      Status  : out HAL.I2C.I2C_Status;
      Timeout : Natural := 1000);

   overriding
   procedure Mem_Write
     (This          : in out I2C_Port;
      Addr          : HAL.I2C.I2C_Address;
      Mem_Addr      : UInt16;
      Mem_Addr_Size : HAL.I2C.I2C_Memory_Address_Size;
      Data          : HAL.I2C.I2C_Data;
      Status        : out HAL.I2C.I2C_Status;
      Timeout       : Natural := 1000);

   overriding
   procedure Mem_Read
     (This          : in out I2C_Port;
      Addr          : HAL.I2C.I2C_Address;
      Mem_Addr      : UInt16;
      Mem_Addr_Size : HAL.I2C.I2C_Memory_Address_Size;
      Data          : out HAL.I2C.I2C_Data;
      Status        : out HAL.I2C.I2C_Status;
Timeout : Natural := 1000);


private

   type I2C_Port is new HAL.I2C.I2C_Port with record
      File_Desc : File_Id;
      Config : I2C_Configuration;
   end record;

   -- These values originate from linux/i2c-dev.h
   I2C_RETRIES     : constant HAL.UInt32 := 16#0701#;
   I2C_TIMEOUT     : constant HAL.UInt32 := 16#0702#;
   I2C_SLAVE       : constant HAL.UInt32 := 16#0703#;
   I2C_SLAVE_FORCE : constant HAL.UInt32 := 16#0706#;
   I2C_TENBIT      : constant HAL.UInt32 := 16#0704#;
   I2C_FUNCS       : constant HAL.UInt32 := 16#0705#;
   I2C_RDWR        : constant HAL.UInt32 := 16#0707#;
   I2C_PEC         : constant HAL.UInt32 := 16#0708#;
   I2C_SMBUS       : constant HAL.UInt32 := 16#0720#;



end Native.I2C;
