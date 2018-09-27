package body Native.SPI is

   function Configure (Device : String;
                       Conf : SPI_Configuration;
                       Status : out HAL.SPI.SPI_Status)
                       return SPI_Port is

      File : File_Id;
      Ret : Interfaces.C.int;

      -- TOOD: Move this function into IOCTL Package
      function Ioctl (File_Desc : in File_Id;
                      Req : in Request;
                      Data : in System.Address)
                      return Interfaces.C.int;
      pragma Import (C, Ioctl, "ioctl");
      pragma Import_Function(Ioctl, Mechanism => (File_Desc => Value,
                                                  Req => Value,
                                                  Data => Value));
   begin

      -- TODO: Return Error if Errno is -1 or Ioctl returns not 0

      -- Open file
      -- TODO: Make Mode_Flags an enum (in ioctl.ads?)
      File := Open (Device, 8#02#, 777);

      if (Integer(Err_No) /= 0) then
         Status := Err_Error;
         return SPI_Port'(File_Desc => -1, Data_Size => Conf.Data_Size);
      end if;

      -- Set the mode of the SPI Device
      declare
         Mode : HAL.UInt8;
      begin
         case Conf.Clock_Phase is
           when P1Edge => Mode := 0;
           when P2Edge => Mode := 1;
         end case;

         case Conf.Clock_Polarity is
            when High => null;
            when Low  => Mode := Mode or 2#10#;
         end case;

         Ret := Ioctl (File, SPI_MODE(Write), Mode'Address);

      end;

      if Integer(Ret) /= 0 then
         Status := Err_Error;
         return SPI_Port'(File_Desc => -1, Data_Size => Conf.Data_Size);
      end if;

      declare
         BPW : HAL.UInt8 := 8;
      begin
         Ret := Ioctl (File, SPI_BITS_PER_WORD(Write), BPW'Address);
      end;

      if Integer(Ret) /= 0 then
         Status := Err_Error;
         return SPI_Port'(File_Desc => -1, Data_Size => Conf.Data_Size);
      end if;

      declare
         Baud : HAL.Uint32 := HAL.UInt32(Conf.Baud_Rate);
      begin
         Ret := Ioctl (File, SPI_MAX_SPEED_HZ(Write), Baud'Address);
      end;

      if Integer(Ret) /= 0 then
         Status := Err_Error;
         return SPI_Port'(File_Desc => -1, Data_Size => Conf.Data_Size);
      end if;

      Status := HAL.SPI.Ok;
      return SPI_Port'(File_Desc => File, Data_Size => Conf.Data_Size);
   end Configure;

   overriding
   function Data_Size (This : SPI_Port) return HAL.SPI.SPI_Data_Size is
   begin
      return This.Data_Size;
   end Data_Size;

   overriding
   procedure Transmit
     (This   : in out SPI_Port;
      Data   : HAL.SPI.SPI_Data_8b;
      Status : out HAL.SPI.SPI_Status;
      Timeout : Natural := 1000) is

      Ret : Size;
   begin

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
      Data   : HAL.SPI.SPI_Data_16b;
      Status : out HAL.SPI.SPI_Status;
      Timeout : Natural := 1000) is
   begin

      -- Check if provided data matches configuration
      if (This.Data_Size /= HAL.SPI.Data_Size_16b) then
         Status := HAL.SPI.Err_Error;
         return;
      end if;

      -- 16 BPW is currently unimplemented
      Status := Err_Error;

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


end Native.SPI;
