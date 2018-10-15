with Ada.Unchecked_Conversion;


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
      Read_Port(This, BMP280_Calibration_Address, C_Data);
      This.Cal := To_Calibration(C_Data);
   end Configure;

   procedure Read_Values_Int (This : BMP280_Device;
                              Value : out BMP280_Values_Int) is

      --function To_Readout is new Ada.Unchecked_Conversion(Source => Byte_Array,
      --                                                    Target => BMP280_Raw_Readout);

      function To_Readout (C_Data : Byte_Array) return BMP280_Raw_Readout is
      begin
         return BMP280_Raw_Readout'(Temperature => UInt20(
                                    Shift_Left(Unsigned_32(C_Data(4)), 12)
                                    or Shift_Left(Unsigned_32(C_Data(5)), 4)
                                    or Shift_Right(Unsigned_32(C_Data(6)), 4)),
                                    Reserved_20_23 => 0,
                                    Pressure => UInt20(
                                      Shift_Left(Unsigned_32(C_Data(1)), 12)
                                      or Shift_Left(Unsigned_32(C_Data(2)), 4)
                                      or Shift_Right(Unsigned_32(C_Data(3)), 4)),
                                    Reserved_44_47 => 0);
      end To_Readout;

      Readout : BMP280_Raw_Readout;
      C_Data : Byte_Array(1..BMP280_Raw_Readout'Size/8);
   begin

      Read_Port(This, BMP280_Readout_Address, C_Data);
      Readout := To_Readout(C_Data);
      Value.Temperature := Compensate_Temperature(This, Readout);
      Value.Pressure := Compensate_Pressure(This, Readout, Value.Temperature);

   end Read_Values_Int;


   procedure Read_Values_Float (This : BMP280_Device;
                                Values : out BMP280_Values_Float) is

      Int_Values : BMP280_Values_Int;
   begin

      Read_Values_Int(This, Int_Values);

      Values.Temperature := Float(Int_Values.Temperature) / 100.0;
      Values.Pressure := Float(Int_Values.Pressure) / 256.0;

   end Read_Values_Float;



   function Compensate_Temperature (This : BMP280_Device;
                                    Readout : BMP280_Raw_Readout)
                                    return Integer_32 is

      function U2I is new Ada.Unchecked_Conversion (Source => Unsigned_32,
                                                    Target => Integer_32);
      function I2U is new Ada.Unchecked_Conversion (Source => Integer_32,
                                                    Target => Unsigned_32);
      function Shr (i : Integer_32; v : Integer) return Integer_32 is
      begin
         return U2I(Shift_Right_Arithmetic(I2U(i), v));
      end Shr;
      function Shl (i : Integer_32; v : Integer) return Integer_32 is
      begin
         return U2I(Shift_Left(I2U(i), v));
      end Shl;

      T : Integer_32;
      var1, var2 : Integer_32;
   begin

      T := Integer_32(Readout.Temperature);

      var1 := Shr(T, 3) - Shl(Integer_32(This.Cal.dig_T1), 1);
      var1 := Shr(var1 * Integer_32(This.Cal.dig_T2), 11);

      var2 := (Shr(T, 4) - Integer_32(This.Cal.dig_T1))**2;
      var2 := Shr(Shr(var2, 12) * Integer_32(This.Cal.dig_T3), 14);

      return Shr((var1 + var2) * 5 + 128, 8);

   end Compensate_Temperature;

   function Compensate_Pressure (This : BMP280_Device;
                                 Readout : BMP280_Raw_Readout;
                                 Temperature : Integer_32)
                                 return Integer_64 is
      function U2I is new Ada.Unchecked_Conversion (Source => Unsigned_64,
                                                    Target => Integer_64);
      function I2U is new Ada.Unchecked_Conversion (Source => Integer_64,
                                                    Target => Unsigned_64);
      function Shr (i : Integer_64; v : Integer) return Integer_64 is
      begin
         return U2I(Shift_Right_Arithmetic(I2U(i), v));
      end Shr;
      function Shl (i : Integer_64; v : Integer) return Integer_64 is
      begin
         return U2I(Shift_Left(I2U(i), v));
      end Shl;

      P : Integer_64;
      var1, var2 : Integer_64;
      t_fine : Integer_64;
   begin

      P := Integer_64(Readout.Pressure);
      t_fine := (Shl(Integer_64(Temperature), 8) - 128 ) / 5;

      var1 := t_fine - 128000;
      var2 := (var1**2) * Integer_64(This.Cal.dig_P6);
      var2 := var2 + Shl(var1 * Integer_64(This.Cal.dig_P5), 17);
      var2 := var2 + Shl(Integer_64(This.Cal.dig_P4), 35);

      var1 := Shr((var1**2) * Integer_64(This.Cal.dig_P3), 8)
        + Shl(var1 * Integer_64(This.Cal.dig_P2), 12);
      var1 := Shr((Shl(Integer_64(1), 47) + var1)
                  * Integer_64(This.Cal.dig_P1), 33);

      if var1 = 0 then
         return 0;
      end if;

      p := 1048576 - p;
      p := ((Shl(p, 31) - var2) * 3125) / var1;

      var1 := Shr(Integer_64(This.Cal.dig_P9) * (Shr(p, 13)**2), 25);
      var2 := Shr(Integer_64(This.Cal.dig_P8) * p, 19);

      p := Shr(p + var1 + var2, 8) + Shl(Integer_64(This.Cal.dig_P7), 4);

      return p;
   end Compensate_Pressure;


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
