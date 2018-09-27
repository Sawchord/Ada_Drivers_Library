package body Native.SPI is

   function Configure (Device : String;
                       Conf : SPI_Configuration) return SPI_Port is
      File : File_Id;
      Mode : HAL.UInt8;
   begin

      -- Open file
      -- TODO: Make Mode_Flags an enum (in ioctl.ads?)
      File := Open (Device, 8#02#, 777);
      Put ("Opened File:");
      Put (Integer(File), 1);
      New_Line;

      -- Set the mode of the SPI Device
      declare

         Ret : Interfaces.C.int;
         Req : Request;
      begin
         case Conf.Clock_Phase is
           when P1Edge => Mode := 0;
           when P2Edge => Mode := 1;
         end case;

         case Conf.Clock_Polarity is
            when High => null;
            when Low  => Mode := Mode or 2#10#;
         end case;

         Put ("Selected Mode:");
         Put (Integer(Mode), 1);
         New_Line;

         Req := SPI_MODE(Write);
         Ret := Ioctl (File, Req, Mode);

         Put ("Mode selection returned:");
         Put (Integer(Err_No), 1);
         New_Line;

      end;

      return SPI_Port'(File_Desc => 0, Data_Size => HAL.SPI.Data_Size_16b);
   end Configure;

   overriding
   function Data_Size (This : SPI_Port) return HAL.SPI.SPI_Data_Size is
   begin
      return HAL.SPI.Data_Size_16b;
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
