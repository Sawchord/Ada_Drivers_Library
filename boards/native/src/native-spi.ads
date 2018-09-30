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

with Interfaces;
with Interfaces.C; use Interfaces.C;

with Posix; use Posix;

with Ada.Unchecked_Conversion;

-- TODO: Remove this after finishing Debuging
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

package Native.SPI is

   type SPI_Port is new HAL.SPI.SPI_Port with private;

   type SPI_Clock_Polarity is (High, Low);

   type SPI_Clock_Phase is (P1Edge, P2Edge);

   type SPI_First_Bit is (MSB, LSB);

   type SPI_Slave_Management is (Software_Managed, Hardware_Managed);

   type SPI_IOC_Transfer is record
      Tx_Buf        : HAL.UInt64;
      Rx_Buf        : HAL.UInt64;
      Len           : HAL.UInt32;
      Speed_Hz      : HAL.UInt32;
      Delay_Usecs   : HAL.UInt16;
      Bits_Per_Word : HAL.UInt8;
      Cs_Change     : HAL.UInt8;
      Tx_Nbits      : HAL.UInt8;
      Rx_Nbits      : HAL.UInt8;
      Pad           : HAL.Uint16 := 0;
   end record;
   pragma Pack (SPI_IOC_Transfer);

   type SPI_Configuration is record
      Data_Size      : HAL.SPI.SPI_Data_Size;
      Clock_Polarity : SPI_Clock_Polarity;
      Clock_Phase    : SPI_Clock_Phase;
      First_Bit      : SPI_First_Bit;
      Slave_Manager  : SPI_Slave_Management;
      Baud_Rate      : Positive;
   end record;

   function Configure (Device : in String;
                       Conf   : in SPI_Configuration;
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
      Config : SPI_Configuration;
   end record;

   type Tranceive_Mode is (Transmit, Receive, Transceive);
   -- TODO: Add transceive function using the IOCTL call
   procedure Transceive
     (This : in out SPI_Port;
      Out_Data : out HAL.SPI.SPI_Data_16b;
      In_Data : in HAL.SPI.SPI_Data_16b;
      Mode : Tranceive_Mode;
      Status : out HAL.SPI.SPI_Status)
     with Pre => Mode /= Transceive or Out_Data'Length = In_Data'Length;

   SPI_MAGIC : HAL.UInt8 := HAL.Uint8(107); -- from spidev.h (value of 'k')

   function SPI_TRANSFER (n : Positive) return Request is
     (dir => Write,
      typ => SPI_MAGIC,
      nr => 0,
      size => HAL.UInt14(n * (SPI_IOC_Transfer'Size / 8) ));

   function SPI_MODE (dir : Direction) return Request is
     (dir  => dir,
      typ  => SPI_MAGIC,
      nr   => 1,	-- from spidev.h
      size => 1);	-- from spidev.h (size of u8)

   function SPI_LSB_FIRST (dir : Direction) return Request is
     (dir  => dir,
      typ  => SPI_MAGIC,
      nr   => 2,	-- from spidev.h
      size => 1);	-- from spidev.h (size of u8)

   function SPI_BITS_PER_WORD (dir : Direction) return Request is
     (dir  => dir,
      typ  => SPI_MAGIC,
      nr   => 3,	-- from spidev.h
      size => 1);	-- deom spidev.h (size of u8)

   function SPI_MAX_SPEED_HZ (dir : Direction) return Request is
     (dir  => dir,
      typ  => SPI_MAGIC,
      nr   => 4,	-- from spidev.h
      size => 4);	-- from spidev.h (size of u32)

end Native.Spi;
