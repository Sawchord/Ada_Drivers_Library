
with Text_IO; use Text_IO;
with IOCTL;

with HAL;
with HAL.SPI; use HAL.SPI;
with Native.SPI; -- use Native.SPI;

procedure Spi_Test is
   Device : String := "/dev/spidev0.0";
   Conf : Native.SPI.SPI_Configuration :=
       (Data_Size      => HAL.SPI.Data_Size_16b,
        Clock_Polarity => Native.SPI.Low,
        Clock_Phase    => Native.SPI.P2Edge,
        Slave_Manager  => Native.SPI.Hardware_Managed,
        First_Bit      => Native.SPI.MSB,
        Baud_Rate      => 100_000);

   Port : Native.SPI.SPI_Port;
   Status : HAL.SPI.SPI_Status;

   Data : HAL.SPI.SPI_Data_16b := (1,2,3,4);
begin
   Put_Line ("Hello SPI");

   -- Initialize the Port with given Configuration
   Port := Native.SPI.Configure(Device, Conf, Status);

   if (Status /= HAL.SPI.Ok) then
      Put_Line ("Error while initializing SPI Device");
      return;
   end if;

   -- Send Data over the Wire
   Put_Line ("SPI Initialized, attempt to send");
   Port.Transmit(Data, Status);

   if (Status /= HAL.SPI.Ok) then
      Put_Line ("Error while transmitting data");
      return;
   end if;
   Put_Line ("Transmission was successfull");
end;
