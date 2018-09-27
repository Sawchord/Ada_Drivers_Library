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
with HAL.SPI; use HAL.SPI;
with System;
with System.Address_Image;

with Ada.Strings.Bounded;

with Interfaces;
with Interfaces.C; use Interfaces.C;

with IOCTL; use IOCTL;



-- TODO: Remove this after finishing Debuging
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

package Native.SPI is

   type SPI_Port is new HAL.SPI.SPI_Port with private;

   type SPI_Clock_Polarity is (High, Low);

   type SPI_Clock_Phase is (P1Edge, P2Edge);

   type SPI_First_Bit is (MSB, LSB);

   package Device_String is new
     Ada.Strings.Bounded.Generic_Bounded_Length (Max => 80);
   type SPI_Configuration is record
      Data_Size : HAL.SPI.SPI_Data_Size;
      Clock_Polarity : SPI_Clock_Polarity;
      Clock_Phase : SPI_Clock_Phase;
      Baud_Rate : Positive;
   end record;

   function Configure (Device : in String;
                       Conf : in SPI_Configuration;
                       Status : out HAL.SPI.SPI_Status)
                       return SPI_Port;

   overriding
   function Data_Size (This : SPI_Port) return HAL.SPI.SPI_Data_Size;

   overriding
   procedure Transmit
     (This   : in out SPI_Port;
      Data   : HAL.SPI.SPI_Data_8b;
      Status : out HAL.SPI.SPI_Status;
      Timeout : Natural := 1000);

   overriding
   procedure Transmit
     (This   : in out SPI_Port;
      Data   : HAL.SPI.SPI_Data_16b;
      Status : out HAL.SPI.SPI_Status;
      Timeout : Natural := 1000);

   overriding
   procedure Receive
     (This    : in out SPI_Port;
      Data    : out HAL.SPI.SPI_Data_8b;
      Status  : out HAL.SPI.SPI_Status;
      Timeout : Natural := 1000);

     procedure Receive
     (This    : in out SPI_Port;
      Data    : out HAL.SPI.SPI_Data_16b;
      Status  : out HAL.SPI.SPI_Status;
Timeout : Natural := 1000);

private

   type SPI_Port is new HAL.SPI.SPI_Port with record
      File_Desc : File_Id;
      Data_Size : HAL.SPI.SPI_Data_Size;
   end record;

   SPI_MAGIC : HAL.UInt8 := HAL.Uint8(107); -- from spidev.h (value of 'k')

   function SPI_MODE (dir : in Direction) return Request is
     (dir  => dir,
      typ  => SPI_MAGIC,
      nr   => 1,	-- from spidev.h
      size => 1);	-- from spidev.h (size of u8)

   function SPI_BITS_PER_WORD (dir : in Direction) return Request is
     (dir  => dir,
      typ  => SPI_MAGIC,
      nr   => 3,	-- from spidev.h
      size => 1);	-- deom spidev.h (size of u8)

   function SPI_MAX_SPEED_HZ (dir : in Direction) return Request is
     (dir  => dir,
      typ  => SPI_MAGIC,
      nr   => 4,	-- from spidev.h
      size => 4);	-- from spidev.h (size of u32)

end Native.Spi;
