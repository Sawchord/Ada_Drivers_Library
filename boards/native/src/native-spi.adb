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

package body Native.SPI is

   function Configure (Device : String;
                       Conf   : SPI_Configuration;
                       Status : out HAL.SPI.SPI_Status)
                       return SPI_Port is

      File : File_Id;
      Ret : Interfaces.C.int;
   begin

      -- Open file
      -- TODO: Make Mode_Flags an enum (in ioctl.ads?)
      File := Open (Device, 8#02#, 777);

      if (Integer(Err_No) /= 0) then
         Status := Err_Error;
         return SPI_Port'(File_Desc => -1, Config => Conf);
      end if;

      -- Set the mode of the SPI Device
      declare
         Mode : HAL.UInt8;
      begin
         case Conf.Clock_Phase is
           when P1Edge => Mode := 2#0000#;
           when P2Edge => Mode := 2#0001#;
         end case;

         if Conf.Clock_Polarity = Low then
            Mode := Mode or 2#0010#;
         end if;

         -- NOTE: Many devices do not support LSB_First
         if Conf.First_Bit = LSB then
            Mode := Mode or 2#1000#;
         end if;

         Ret := Ioctl (File, SPI_MODE(Write), Mode'Address);

      end;

      if Integer(Ret) /= 0 then
         Status := Err_Error;
         return SPI_Port'(File_Desc => -1, Config => Conf);
      end if;

      -- Set Bits per Word
      -- NOTE: Many devices (e.g. RPI) do not support 16 bits per word
      -- We simulate 16 bits per word by transmitting 2 8 bit words.
      declare
         BPW : HAL.UInt8 := 8;
      begin
         Ret := Ioctl (File, SPI_BITS_PER_WORD(Write), BPW'Address);
      end;

      if Integer(Ret) /= 0 then
         Status := Err_Error;
         return SPI_Port'(File_Desc => -1, Config => Conf);
      end if;

      -- Set the Baudrate
      -- NOTE: Untested
      declare
         Baud : HAL.Uint32 := HAL.UInt32(Conf.Baud_Rate);
      begin
         Ret := Ioctl (File, SPI_MAX_SPEED_HZ(Write), Baud'Address);
      end;

      if Integer(Ret) /= 0 then
         Status := Err_Error;
         return SPI_Port'(File_Desc => -1, Config => Conf);
      end if;

      Status := HAL.SPI.Ok;
      return SPI_Port'(File_Desc => File, Config => Conf);
   end Configure;

   overriding
   function Data_Size (This : SPI_Port) return HAL.SPI.SPI_Data_Size is
   begin
      return This.Config.Data_Size;
   end Data_Size;

   overriding
   procedure Transmit
     (This   : in out SPI_Port;
      Data   : HAL.SPI.SPI_Data_8b;
      Status : out HAL.SPI.SPI_Status;
      Timeout : Natural := 1000) is

      Ret : Size;
   begin
      -- TODO: Check whether HAL should deselect CS after every transfer, or
      -- keep CS active during the transfer

      -- Check if provided data matches configuration
      if (This.Data_Size /= HAL.SPI.Data_Size_8b) then
         Status := HAL.SPI.Err_Error;
         return;
      end if;

      Ret := Write (This.File_Desc, Data'Address, Data'Length);

      if Integer(ret) /= Data'Length then
         Status := Err_Error;
      else
         Status := Ok;
      end if;

   end Transmit;


   overriding
   procedure Transmit
     (This   : in out SPI_Port;
      Data   : in HAL.SPI.SPI_Data_16b;
      Status : out HAL.SPI.SPI_Status;
      Timeout : Natural := 1000) is

      --Ret : Size;
      Out_Data :HAL.SPI.SPI_Data_16b(0..0);
   begin

      -- TODO: This Function can only transmit in little endian.

      -- Check if provided data matches configuration
      if (This.Data_Size /= HAL.SPI.Data_Size_16b) then
         Status := HAL.SPI.Err_Error;
         return;
      end if;

      --Ret := Write (This.File_Desc, Data'Address, 2 * Data'Length);

      --if Integer(ret) /= 2 * Data'Length then
      --   Status := Err_Error;
      --else
      --   Status := Ok;
      --end if;

      This.Transceive(Out_Data, Data, Transmit, Status);

   end Transmit;

   overriding
   procedure Receive
     (This    : in out SPI_Port;
      Data    : out HAL.SPI.SPI_Data_8b;
      Status  : out HAL.SPI.SPI_Status;
      Timeout : Natural := 1000) is
   begin

      null;

   end Receive;

   procedure Receive
     (This    : in out SPI_Port;
      Data    : out HAL.SPI.SPI_Data_16b;
      Status  : out HAL.SPI.SPI_Status;
      Timeout : Natural := 1000) is
   begin

      null;

   end Receive;

   procedure Transceive
     (This : in out SPI_Port;
      Out_Data : out HAL.SPI.SPI_Data_16b;
      In_Data : in HAL.SPI.SPI_Data_16b;
      Mode : Tranceive_Mode;
      Status : out HAL.SPI.SPI_Status) is

      Tx_Buf : HAL.UInt16 := 0;
      Rx_Buf : HAL.UInt16 := 0;

      pragma Warnings (Off, "types for unchecked conversion have different sizes");
      function Address_To_UInt64
      is new Ada.Unchecked_Conversion(Source => System.Address,
                                      Target => HAL.UInt64);
      pragma Warnings (On, "types for unchecked conversion have different sizes");

      Loop_Begin : Integer;
      Loop_End : Integer;
   begin

      if Mode = Transmit then
         Loop_Begin := In_Data'First;
         Loop_End := In_Data'Last;
      else
         Loop_Begin := Out_Data'First;
         Loop_End := Out_Data'Last;
      end if;

      for I in Loop_Begin..Loop_End loop

         if Mode = Transmit or Mode = Transceive then
            Tx_Buf := In_Data(I);
         end if;

         declare
            Transmission : SPI_IOC_Transfer :=
              (Tx_Buf => Address_To_UInt64 (Tx_Buf'Address),
               Rx_Buf => Address_To_UInt64 (Rx_Buf'Address),
               Len => 2,
               Speed_Hz => Hal.Uint32 (This.Config.Baud_Rate),
               Delay_Usecs => 0,
               Bits_Per_Word => 8,
               Cs_Change => 0,
               Tx_Nbits => 0,
               Rx_Nbits => 0,
               Pad => 0);

            Ret : Interfaces.C.int;
         begin
            Ret := Ioctl (This.File_Desc, SPI_TRANSFER(1),
                          Transmission'Address);

            if Integer (Ret) /= 2 then
               Status := Err_Error;
               return;
            end if;
         end;

         if Mode = Receive or Mode = Transceive then
            Out_Data(I) := Rx_Buf;
         end if;

      end loop;

   end Transceive;


end Native.SPI;
