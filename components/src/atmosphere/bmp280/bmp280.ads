------------------------------------------------------------------------------
--                                                                          --
--                  Copyright (C) 2018, AdaCore                        --
--                                                                          --
--  Redistribution and use in source and binary forms, with or without      --
--  modification, are permitted provided that the following conditions are  --
--  met:                                                                    --
--     1. Redistributions of source code must retain the above copyright    --
--        notice, this list of conditions and the following disclaimer.     --
--     2. Redistributions in binary form must reproduce the above copyright --
--        notice, this list of conditions and the following disclaimer in   --
--        the documentation and/or other materials provided with the        --
--        distribution.                                                     --
--     3. Neither the name of the copyright holder nor the names of its     --
--        contributors may be used to endorse or promote products derived   --
--        from this software without specific prior written permission.     --
--                                                                          --
--   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS    --
--   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT      --
--   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR  --
--   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT   --
--   HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, --
--   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT       --
--   LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,  --
--   DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY  --
--   THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT    --
--   (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE  --
--   OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.   --
--                                                                          --
------------------------------------------------------------------------------

with HAL; use HAL;
with Interfaces; use Interfaces;

package BMP280 is

   type BMP280_Device is tagged limited private;

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

   --  Values are from BMP280 Datasheet
   for BMP280_Oversampling_Rate use (Skip => 2#000#,
                                     x1   => 2#001#,
                                     x2   => 2#010#,
                                     x4   => 2#011#,
                                     x8   => 2#100#,
                                     x16  => 2#101#);

   type BMP280_Standby_Time is (us500, us65000, ms125, ms250, ms500, ms1000,
                                  ms2000, ms4000)
       with Size => 3;
   --  Values are from BMP280 Datascheet
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
      Filter_Coefficient : UInt3;
   end record;

   procedure Configure (This : in out BMP280_Device;
                        Configuration : BMP280_Configuration);

   procedure Read_Values_Int (This : BMP280_Device;
                              Value : out BMP280_Values_Int);

   procedure Read_Values_Float (This : BMP280_Device;
                                Values : out BMP280_Values_Float);

private

   type Byte_Array is array (Positive range <>) of UInt8
     with Alignment => 2;

   BMP280_Device_Id : constant UInt8 := 16#58#;
   BMP280_Reset_Magic : constant UInt8 := 16#B6#;

   BMP280_Calibration_Address : constant UInt8 := 16#88#;
   BMP280_Device_Id_Address : constant UInt8 := 16#D0#;
   BMP280_Reset_Address : constant UInt8 := 16#E0#;
   BMP280_Status_Address : constant UInt8 := 16#F3#;
   BMP280_Control_Address : constant UInt8 := 16#F4#;
   BMP280_Config_Address : constant UInt8 := 16#F5#;
   BMP280_Readout_Address : constant UInt8 := 16#F7#;

   type BMP280_Power_Mode is (Sleep, Force, Normal)
   with Size => 2;
   for BMP280_Power_Mode use (Sleep  => 2#00#,
                       Force  => 2#01#,
                       Normal => 2#11#);


   type BMP280_Config is record
      t_sb  : BMP280_Standby_Time;
      filter : UInt3;
      reserved_1 : Boolean := False;
      spi3w : Boolean;
   end record
     with Size => 8;
   for BMP280_Config use record
      t_sb       at 0 range 5 .. 7;
      filter     at 0 range 2 .. 4;
      reserved_1 at 0 range 1 .. 1;
      spi3w      at 0 range 0 .. 0;
   end record;

   type BMP280_Control is record
      osrs_t : BMP280_Oversampling_Rate;
      osrs_p : BMP280_Oversampling_Rate;
      mode : BMP280_Power_Mode;
   end record
     with Size => 8;
   for BMP280_Control use record
      osrs_t at 0 range 5 .. 7;
      osrs_p at 0 range 2 .. 4;
      mode   at 0 range 0 .. 1;
   end record;

   type BMP280_Status is record
      measuring : Boolean;
      im_update : Boolean;
   end record
     with Size => 8;
   for BMP280_Status use record
      measuring at 0 range 4 .. 4;
      im_update at 0 range 7 .. 7;
   end record;

   --  See BMP280 Reference Manual
   type BMP280_Calibration is record
      dig_T1 : Unsigned_16;
      dig_T2 : Integer_16;
      dig_T3 : Integer_16;
      dig_P1 : Unsigned_16;
      dig_P2 : Integer_16;
      dig_P3 : Integer_16;
      dig_P4 : Integer_16;
      dig_P5 : Integer_16;
      dig_P6 : Integer_16;
      dig_P7 : Integer_16;
      dig_P8 : Integer_16;
      dig_P9 : Integer_16;
   end record
     with Size => 192;
   pragma Pack (BMP280_Calibration);

   type BMP280_Raw_Readout is record
      Pressure : UInt20;
      Reserved_20_23 : UInt4 := 0;
      Temperature : UInt20;
      Reserved_44_47 : UInt4 := 0;
   end record
     with Size => 48;
   for BMP280_Raw_Readout use record
      -- TODO : Check LSB issues
      -- TODO : Make this ue decimals
      Pressure       at 16#0# range 0 .. 19;
      Reserved_20_23 at 16#0# range 20 .. 23;
      Temperature    at 16#3# range 0 .. 19;
      Reserved_44_47 at 16#3# range 20 .. 23;
   end record;

   type BMP280_Device is tagged limited record
      Cal : BMP280_Calibration;
      Raw : BMP280_Raw_Readout;
   end record;


   function Compensate_Temperature (This : BMP280_Device;
                                    Readout : BMP280_Raw_Readout)
                                    return Integer_32;

   function Compensate_Pressure (This : BMP280_Device;
                                 Readout : BMP280_Raw_Readout;
                                 Temperature : Integer_32)
                                 return Integer_64;


   procedure Read_Port (This : BMP280_Device;
                        Address : UInt8;
                        Data : out Byte_Array);

   procedure Write_Port (This : BMP280_Device;
                         Address : UInt8;
                         Data : UInt8);

end BMP280;
