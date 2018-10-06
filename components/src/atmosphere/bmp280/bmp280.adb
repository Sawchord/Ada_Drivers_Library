package body BMP280 is

   -- TODO: Introduce Status Value
   procedure Configure (This : BMP280_Device;
                        Configuration : BMP280_Configuration) is
      Data : Byte_Array(1..1);
      Config : BMP280_Config;
      Control : BMP280_Control;
   begin

      -- Check the Device Id
      Read_Port (This, BMP280_Device_Id_Address, Data);
      if Data(1) /= BMP280_Device_Id then
         return;
      end if;

      Config := (t_sb => Configuration.Standby_Time,
                 filter => Configuration.Filter_Coefficient,
                 spi3w => False);

      --Write_Port (This, BMP280_Config_Address, UInt8(Config));

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

   function Convert_To_Rec(Input : Byte_Array) return REC is
      Result : REC with
        Import, Convention => Ada, Address => Input'Address;
   begin
      Return Result;
   end Convert_To_Rec;

   function Convert_From_Rec(Input : REC) return Byte_Array is
      Result : Constant Byte_Array(1..REC'Size);
      for Result'Address use Input'Address;
      pragma Import( Convention => Ada, Entity => Result );
   begin
      Return Result;
   end Convert_From_Rec;

   function Convert (Input : Byte_Array) return BMP280_Calibration is new Convert_To_Rec (BMP280_Calibration);

   -- TODO: Use generics
   --function Convert(Input : Byte_Array) return BMP280_Calibration is
   --   Result : BMP280_Calibration with
   --     Import, Convention => Ada, Address => Input'Address;
   --begin
   --   Return Result;
   --end Convert;

   --function Convert(Input : Byte_Array) return BMP280_Raw_Readout is
   --   Result : BMP280_Raw_Readout with
   --     Import, Convention => Ada, Address => Input'Address;
   --begin
   --   Return Result;
   --end Convert;



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
