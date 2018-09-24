package body Native.SPI is

   function Configure (Conf : SPI_Configuration) return SPI_Port is
   begin
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
