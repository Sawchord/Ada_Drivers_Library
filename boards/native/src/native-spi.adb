package body Native.SPI is

   function Configure (Device : String;
                       Conf : SPI_Configuration) return SPI_Port is

      File : File_Id;
      Ret : Interfaces.C.int;

      function Ioctl (File_Desc : in File_Id;
                      Req : in Request;
                      Data : in out HAL.UInt32)
                      return Interfaces.C.int;
      pragma Import (C, Ioctl, "ioctl");
      pragma Import_Function(Ioctl, Mechanism => (File_Desc => Value,
                                                  Req => Value,
                                                  Data => Reference));
   begin

      -- Open file
      -- TODO: Make Mode_Flags an enum (in ioctl.ads?)
      File := Open (Device, 8#02#, 777);
      Put ("Opened File:");
      Put (Integer(File), 2);
      New_Line;

      Put ("File Open Errno:");
      Put (Integer(Err_No), 2);
      New_Line;

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

         Ret := Ioctl (File, SPI_MODE(Write), HAL.UInt32(Mode));

      end;

      Put ("Ioctl return value:");
      Put (Integer(Ret), 2);
      New_Line;

      Put ("Mode Selection Errno:");
      Put (Integer(Err_No), 2);
      New_Line;

      declare
         BPW : HAL.UInt8;
      begin
         case Conf.Data_Size is
            when HAL.SPI.Data_Size_8b => BPW := 8;
            when HAL.SPI.Data_Size_16b => BPW := 16;
         end case;

         Ret := Ioctl (File, SPI_BITS_PER_WORD(Write), HAL.UInt32(BPW));

      end;

      Put ("Ioctl return value:");
      Put (Integer(Ret), 2);
      New_Line;

      Put ("Bits per Word Errno:");
      Put (Integer(Err_No), 2);
      New_Line;

      declare
         Baud : HAL.Uint32 := HAL.UInt32(Conf.Baud_Rate);
      begin
         Ret := Ioctl (File, SPI_MAX_SPEED_HZ(Write), Baud);
      end;

      Put ("Ioctl return value:");
      Put (Integer(Ret), 2);
      New_Line;

      Put ("Baud Rate Errno:");
      Put (Integer(Err_No), 2);
      New_Line;

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
   begin
      null;
   end Transmit;


   overriding
   procedure Transmit
     (This   : in out SPI_Port;
      Data   : HAL.SPI.SPI_Data_16b;
      Status : out HAL.SPI.SPI_Status;
      Timeout : Natural := 1000) is
   begin
      null;
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
