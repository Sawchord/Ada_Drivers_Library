
with Text_IO; use Text_IO;
with IOCTL;

with HAL;
with HAL.SPI;
with Native.SPI;

procedure Spi_Test is
   Device : String := "/dev/spidev0.0";
   Conf : Native.SPI.SPI_Configuration :=
       (Data_Size => HAL.SPI.Data_Size_8b,
        Clock_Polarity => Native.SPI.High,
        Clock_Phase => Native.SPI.P1Edge,
        Baud_Rate => 100_000);

   Port : Native.SPI.SPI_Port;
begin
   Put_Line("Hello SPI");

   Port := Native.SPI.Configure(Device, Conf);

end;
