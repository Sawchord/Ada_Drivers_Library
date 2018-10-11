package body BMP280 is

   -- TODO: Introduce Status Value
   procedure Configure (This : in out BMP280_Device;
                        Configuration : BMP280_Configuration) is

      function Config_To_UInt8 is new
        Ada.Unchecked_Conversion (Source => BMP280_Config,
                                  Target => UInt8);

      function Control_To_UInt8 is new
        Ada.Unchecked_Conversion (Source => BMP280_Control,
                                  Target => UInt8);

      function To_Calibration is new
        Ada.Unchecked_Conversion (Source => Byte_Array,
                                  Target => BMP280_Calibration);

      D_Id : Byte_Array(1..1);
      Config : BMP280_Config;
      Control : BMP280_Control;
      C_Data : Byte_Array(1..BMP280_Calibration'Size/8);
   begin

      -- Check the Device Id
      Read_Port (This, BMP280_Device_Id_Address, D_Id);
      if D_Id(1) /= BMP280_Device_Id then
         return;
      end if;

      Config := (t_sb => Configuration.Standby_Time,
                 filter => Configuration.Filter_Coefficient,
                 spi3w => False);
      Write_Port (This, BMP280_Config_Address, Config_To_Uint8(Config));

      Control := (osrs_t => Configuration.Temperature_Oversampling,
                  osrs_p => Configuration.Pressure_Oversampling,
                  mode => Normal);
      Write_Port (This, BMP280_Control_Address, Control_To_Uint8(Control));

      -- Read the Calibration Data
      Read_Port (This, BMP280_Calibration_Address, C_Data);
      This.Cal := To_Calibration(C_Data);
   end Configure;

   procedure Read_Values_Int (This : BMP280_Device;
                               Value : BMP280_Values_Int) is
   begin
      null;
   end Read_Values_Int;

   procedure Read_Values_Float (This : BMP280_Device;
                                Values : BMP280_Values_Float) is
   begin
      null;
   end Read_Values_Float;

   procedure Read_Port (This : BMP280_Device;
                   Address : UInt8;
                   Data : out Byte_Array) is

      SPI_Address : constant SPI_Data_8b(1..1) := (1=> Address);
      SPI_Data : SPI_Data_8b(Data'Range);
      Status : SPI_Status;
   begin

      -- TODO: Make this possible without need to copy data afterwards
      This.Cs.Clear;
      This.Port.Transmit(SPI_Address, Status);
      This.Port.Receive(SPI_Data, Status);
      This.Cs.Set;

      Data(Data'Range) := Byte_Array(SPI_Data(Data'Range));
   end Read_Port;


   procedure Write_Port (This : BMP280_Device; Address : UInt8; Data : UInt8) is
      Write_Mask : constant UInt8 := 2#01111111#;
      SPI_Data : constant SPI_Data_8b(1..2) := (1 => Address and Write_Mask, 2=> Data);
      Status : SPI_Status;
   begin
      This.Cs.Clear;
      This.Port.Transmit(SPI_Data, Status);
      This.Cs.Set;
   end Write_Port;

end BMP280;
