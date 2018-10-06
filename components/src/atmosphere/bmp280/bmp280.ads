with HAL; use HAL;
with HAL.SPI; use HAL.SPI;
with HAL.GPIO; use HAL.GPIO;
with HAL.Time;

with Interfaces; use Interfaces;

package BMP280 is

   type BMP280_Device (Port: Any_SPI_Port;
                       Cs: Any_GPIO_Point;
                       Time : not null HAL.Time.Any_Delays) is private;

   type BMP280_Values_Int is record
      Temperature : Integer_32;
      Pressure : Integer_64;
   end record;

   type BMP280_Values_Float is record
         Temperature : Float;
         Pressure : Float;
   end record;

   type BMP280_Oversampling_Rate is (Skip, x1, x2, x4, x8, x16)
     with Size => 3;
   -- These Values are from BMP280 Datasheet
   for BMP280_Oversampling_Rate use (Skip => 2#000#,
                                     x1   => 2#001#,
                                     x2   => 2#010#,
                                     x4   => 2#011#,
                                     x8   => 2#100#,
                                     x16  => 2#101#);

   type BMP280_Standby_Time is (us500, us65000, ms125, ms250, ms500, ms1000,
                                  ms2000, ms4000)
       with Size => 3;
   -- These Values are from BMP280 Datascheet
   for BMP280_Standby_Time use (us500   => 2#000#,
                                us65000 => 2#001#,
                                ms125   => 2#010#,
                                ms250   => 2#011#,
                                ms500   => 2#100#,
                                ms1000  => 2#101#,
                                ms2000  => 2#110#,
                                ms4000  => 2#111#);

   type BMP280_Configuration is record
      Standby_Time : BMP280_Standby_Time;
      Temperature_Oversampling : BMP280_Oversampling_Rate;
      Pressure_Oversampling : BMP280_Oversampling_Rate;
      Filter_Coefficient : Uint3;
   end record;


   procedure Configure (This : BMP280_Device;
                        Configuration : BMP280_Configuration);

   procedure Read_Values_Int (This : BMP280_Device;
                              Value : BMP280_Values_Int);

   procedure Read_Values_Float (This : BMP280_Device;
                                Values : BMP280_Values_Float);

private

   BMP280_Device_Id : constant Uint16 := 16#58#;
   BMP280_Reset_Magic : constant Uint16 := 16#B6#;

   type Power_Mode is (Sleep, Force, Normal)
   with Size => 2;
   for Power_Mode use (Sleep  => 2#00#,
                       Force  => 2#01#,
                       Normal => 2#11#);


   type Config is record
      t_sb  : UInt3;
      filter : Uint3;
      spi3w : Boolean;
   end record
     with Size => 8;
   for Config use record
      t_sb   at 0 range 0..2;
      filter at 0 range 3..5; -- TODO: Check if it is ok, to leave out a bit
      spi3w  at 0 range 7..7;
   end record;

   type Control is record
      osrs_t : BMP280_Oversampling_Rate;
      osrs_p : BMP280_Oversampling_Rate;
      mode : Power_Mode;
   end record
     with Size => 8;
   for Control use record
      osrs_t at 0 range 0..2;
      osrs_p at 0 range 3..5;
      mode   at 0 range 6..7;
   end record;

   type Status is record
      measuring : Boolean;
      im_update : Boolean;
   end record
     with Size => 8;
   for Status use record
      measuring at 0 range 4..4;
      im_update at 0 range 7..7;
   end record;

   -- See BMP280 Reference Manual
   type Calibration is record
      dig_T1 : Unsigned_16;
      dig_T2 : Integer_16;
      dig_T3 : Integer_16;
      dig_P3 : Unsigned_16;
      dig_P4 : Integer_16;
      dig_P5 : Integer_16;
      dig_P6 : Integer_16;
      dig_P7 : Integer_16;
      dig_P8 : Integer_16;
      dig_P9 : Integer_16;
   end record;
   pragma Pack(Calibration);

   type Raw_Readout is record
      Pressure : Uint20;
      Reserved_20_23 : UInt4;
      Temperature : UInt20;
      Reserved_44_47 : UInt4;
   end record
     with Size => 48;
   for Raw_Readout use record -- TODO : Check LSB issues
      Pressure       at 16#0# range 0..19;
      Reserved_20_23 at 16#0# range 20..23;
      Temperature    at 16#3# range 0..19;
      Reserved_44_47 at 16#3# range 20..23;
   end record;


   type BMP280_Device (Port: Any_SPI_Port;
                       Cs: Any_GPIO_Point;
                       Time : not null HAL.Time.Any_Delays) is record
      Cal : Calibration;
   end record;



end BMP280;
