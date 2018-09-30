
with Text_IO; use Text_IO;
with Posix;

with HAL;
with HAL.SPI; use HAL.SPI;
with HAL.I2C; use HAL.I2C;
with Native.SPI; -- use Native.SPI;

with Native.I2C;

procedure Spi_Test is
   SPI_Device : String := "/dev/spidev0.0";
   SPI_Conf : Native.SPI.SPI_Configuration :=
       (Data_Size      => HAL.SPI.Data_Size_16b,
        Clock_Polarity => Native.SPI.High,
        Clock_Phase    => Native.SPI.P1Edge,
        Slave_Manager  => Native.SPI.Hardware_Managed,
        First_Bit      => Native.SPI.MSB,
        Baud_Rate      => 500_000);

   SPI_Port : Native.SPI.SPI_Port;
   SPI_Status : HAL.SPI.SPI_Status;

   SPI_Data : HAL.SPI.SPI_Data_16b := (1,2,3,4);

   I2C_Device : String := "/dev/i2c-1";
   I2C_Conf : Native.I2C.I2C_Configuration :=
     (Addressing_Mode => Native.I2C.Addressing_Mode_7bit,
      Ack => Native.I2C.Ack_Enable,
      Device_Mode => Native.I2C.I2C_Mode);

   I2C_Port : Native.I2C.I2C_Port;
   I2C_Status : HAL.I2C.I2C_Status;

   I2C_Addr : HAL.I2C.I2C_Address := 16#0A#;
   I2C_Data : HAL.I2C.I2C_Data := (5,6,7,8);
begin
   Put_Line ("Hello SPI");

   -- Initialize the SPI_Port with given Configuration
   SPI_Port := Native.SPI.Configure (SPI_Device, SPI_Conf, SPI_Status);

   if SPI_Status /= HAL.SPI.Ok then
      Put_Line ("Error while initializing SPI Device");
      return;
   end if;

   -- Send Data over the Wire
   Put_Line ("SPI Initialized, attempt to send");
   SPI_Port.Transmit(SPI_Data, SPI_Status);

   if SPI_Status /= HAL.SPI.Ok then
      Put_Line ("Error while transmitting SPI data");
      return;
   end if;
   Put_Line ("Transmission was successfull");

   Put_Line ("Hello I2C");

   I2C_Port := Native.I2C.Configure (I2C_Device, I2C_Conf, I2C_Status);

   if I2C_Status /= HAL.I2C.Ok then
      Put_Line ("Error while initializing I2C Device");
   end if;

   I2C_Port.Master_Transmit (I2C_Addr, I2C_Data, I2C_Status);

   if I2C_Status /= HAL.I2C.Ok then
      Put_Line ("Error while transmitting I2C data");
   end if;

end;
