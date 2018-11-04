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

package MPU60x0 is

   --  TODO: Definition of Configuration and Value record
   type MPU60x0_Sensor_Reading is record
      Accel_X : UInt16;
      Accel_Y : UInt16;
      Accel_Z : UInt16;
      Temp    : UInt16;
      Gyro_X  : UInt16;
      Gyro_Y  : UInt16;
      Gyro_Z  : UInt16;
   end record
     with Size => 112;
   pragma Pack (MPU60x0_Sensor_Reading);

   type MPU60x0_Sensor_Reading_Float is record
      Accel_X : Float;
      Accel_Y : Float;
      Accel_Z : Float;
      Temp    : Float;
      Gyro_X  : Float;
      Gyro_Y  : Float;
      Gyro_Z  : Float;
   end record
     with Size => 224;
   pragma Pack (MPU60x0_Sensor_Reading_Float);

   type MPU60x0_Gyro_Scale_Range is (deg_250_s, deg_500_s,
                                     deg_1000_s, deg_2500_s)
     with Size => 2;
   for MPU60x0_Gyro_Scale_Range use (deg_250_s => 0, deg_500_s => 1,
                                     deg_1000_s => 2, deg_2500_s => 3);

   type MPU60x0_Accel_Scale_Range is (g2, g4, g8, g16)
     with Size => 2;
   for MPU60x0_Accel_Scale_Range use (g2 => 0, g4 => 1, g8 => 2, g16 => 3);

   type MPU60x0_Configuration is record
      Accel_Scale_Range : MPU60x0_Accel_Scale_Range;
      Gyro_Scale_Range : MPU60x0_Gyro_Scale_Range;
      --  TODO: Digital Filter setting
      --  TODO: Sample Speed setting
      --  TODO: Clock source settin
   end record;
   type MPU60x0_Config_Access is access all MPU60x0_Configuration;

   type MPU60x0_Device is tagged limited private;

   procedure Configure (This : in out MPU60x0_Device;
                        Conf : MPU60x0_Configuration);
   procedure Read_Values (This : MPU60x0_Device;
                          Values : in out MPU60x0_Sensor_Reading);
   procedure Read_Values_Float (This : MPU60x0_Device;
                                Values : in out MPU60x0_Sensor_Reading_Float);

private

   type Self_Test_Registers is record
      XA_Test_H    : UInt3;
      XG_Test      : UInt5;
      YA_Test_H    : UInt3;
      YG_Test      : UInt5;
      ZA_Test_H    : UInt3;
      ZG_Test      : UInt5;
      Reserved_6_7 : UInt2;
      XA_Test_L    : UInt2;
      YA_Test_L    : UInt2;
      ZA_Test_L    : UInt2;
   end record
     with Size => 32;
   for Self_Test_Registers use record
      XA_Test_H    at 0 range 5 .. 7;
      XG_Test      at 0 range 0 .. 4;
      YA_Test_H    at 1 range 5 .. 7;
      YG_Test      at 1 range 0 .. 4;
      ZA_Test_H    at 2 range 5 .. 7;
      ZG_Test      at 2 range 0 .. 4;
      Reserved_6_7 at 3 range 6 .. 7;
      XA_Test_L    at 3 range 4 .. 5;
      YA_Test_L    at 3 range 2 .. 3;
      ZA_Test_L    at 3 range 0 .. 1;
   end record;
   SELF_TEST_REG_ADDRESS : constant UInt8 := 16#0D#;

   type Sample_Rate_Divider is new UInt8;
   SR_DIV_ADDRESS : constant UInt8 := 16#19#;

   type MPU60x0_Filter_Setting is new UInt3;
   type Config is record
      Reserved_3_7   : UInt5;
      Filter_Setting : MPU60x0_Filter_Setting;
   end record
     with Size => 8;
   for Config use record
      Reserved_3_7   at 0 range 3 .. 7;
      Filter_Setting at 0 range 0 .. 2;
   end record;
   CONFIG_ADDRESS : constant UInt8 := 16#1A#;

   type Gyro_Config is record
      XG_Selftest  : Boolean;
      YG_Selftest  : Boolean;
      ZG_Selftest  : Boolean;
      Scale_Range  : MPU60x0_Gyro_Scale_Range;
      Reserved_0_2 : UInt3;
   end record
     with Size => 8;
   for Gyro_Config use record
      XG_Selftest  at 0 range 7 .. 7;
      YG_Selftest  at 0 range 6 .. 6;
      ZG_Selftest  at 0 range 5 .. 5;
      Scale_Range  at 0 range 3 .. 4;
      Reserved_0_2 at 0 range 0 .. 2;
   end record;
   GYRO_CONFIG_ADDRESS : constant UInt8 := 16#1B#;

   type Accel_Config is record
      XA_Selftest  : Boolean;
      YA_Selftest  : Boolean;
      ZA_Selftest  : Boolean;
      Scale_range  : MPU60x0_Accel_Scale_Range;
      Reserved_0_2 : UInt3;
   end record
     with Size => 8;
   for Accel_Config use record
      XA_Selftest  at 0 range 7 .. 7;
      YA_Selftest  at 0 range 6 .. 6;
      ZA_Selftest  at 0 range 5 .. 5;
      Scale_Range  at 0 range 3 .. 4;
      Reserved_0_2 at 0 range 0 .. 2;
   end record;
   ACCEL_CONFIG_ADDRESS : constant UInt8 := 16#1C#;

   --  TODO : FIFO enable register


   type Int_Pin_Config is record
      Int_Level         : Boolean;
      Int_Open          : Boolean;
      Latch_Int_Enable  : Boolean;
      Int_Read_Clear    : Boolean;
      FSync_Int_Level   : Boolean;
      FSync_Int_Enable  : Boolean;
      I2C_Bypass_Enable : Boolean;
      Reserved_0        : Boolean;
   end record
     with Size => 8;
   for Int_Pin_Config use record
      Int_Level         at 0 range 7 .. 7;
      Int_Open          at 0 range 6 .. 6;
      Latch_Int_Enable  at 0 range 5 .. 5;
      Int_Read_Clear    at 0 range 4 .. 4;
      FSync_Int_Level   at 0 range 3 .. 3;
      FSync_Int_Enable  at 0 range 2 .. 2;
      I2C_Bypass_Enable at 0 range 1 .. 1;
      Reserved_0        at 0 range 0 .. 0;
   end record;
   INT_PIN_CFG_ADDRESS : constant UInt8 := 16#37#;

   type Int_Enable is record
      Reserved_5_7          : UInt3;
      Fifo_Overflow_Enable  : Boolean;
      I2C_Master_Int_Enable : Boolean;
      Reserved_1_2          : UInt2;
      Data_Ready_Enable     : Boolean;
   end record
     with Size => 8;
   for Int_Enable use record
      Reserved_5_7          at 0 range 5 .. 7;
      Fifo_Overflow_Enable  at 0 range 4 .. 4;
      I2C_Master_Int_Enable at 0 range 3 .. 3;
      Reserved_1_2          at 0 range 1 .. 2;
      Data_Ready_Enable     at 0 range 0 .. 0;
   end record;
   INT_ENABLE_ADDRESS : constant UInt8 := 16#38#;

   type Int_Status is record
      Reserved_5_7   : UInt3;
      Fifo_Overflow  : Boolean;
      I2C_Master_Int : Boolean;
      Reserved_1_2   : UInt2;
      Data_Ready     : Boolean;
   end record
     with Size => 8;
   for Int_Status use record
      Reserved_5_7   at 0 range 5 .. 7;
      Fifo_Overflow  at 0 range 4 .. 4;
      I2C_Master_Int at 0 range 3 .. 3;
      Reserved_1_2   at 0 range 1 .. 2;
      Data_Ready     at 0 range 0 .. 0;
   end record;
   INT_STATUS_ADDRESS : constant UInt8 := 16#3A#;


   SENSOR_DATA_ADDRESS : constant UInt8 := 16#3B#;


   type Signal_Path_Reset is record
      Reserved_3_7 : UInt5;
      Gyro_Reset   : Boolean;
      Accel_Reset  : Boolean;
      Temp_Reset   : Boolean;
   end record
     with Size => 8;
   for Signal_Path_Reset use record
      Reserved_3_7 at 0 range 3 .. 7;
      Gyro_Reset   at 0 range 2 .. 2;
      Accel_Reset  at 0 range 1 .. 1;
      Temp_Reset   at 0 range 0 .. 0;
   end record;
   SIGNAL_PATH_RESET_ADDRESS : constant UInt8 := 16#68#;

   type User_Control is record
      Reserved_7        : Boolean;
      Fifo_Enable       : Boolean;
      I2C_Master_Enable : Boolean;
      I2C_IF_Enable     : Boolean;
      Reserved_3        : Boolean;
      Fifo_Reset        : Boolean;
      I2C_Master_Reset  : Boolean;
      Sensor_Reset      : Boolean;
   end record
     with Size => 8;
   for User_Control use record
      Reserved_7        at 0 range 7 .. 7;
      Fifo_Enable       at 0 range 6 .. 6;
      I2C_Master_Enable at 0 range 5 .. 5;
      I2C_IF_Enable     at 0 range 4 .. 4;
      Reserved_3        at 0 range 3 .. 3;
      Fifo_Reset        at 0 range 2 .. 2;
      I2C_Master_Reset  at 0 range 1 .. 1;
      Sensor_Reset      at 0 range 0 .. 0;
   end record;
   USER_CONTROL_ADDRESS : constant UInt8 := 16#6A#;


   type MPU60x0_Clock_Selection is (Internal, PLL_X, PLL_Y, PLL_Z,
                            PLL_Ext_32kHz, PLL_Ext_19MHz, Stop)
     with Size => 3;
   for MPU60x0_Clock_Selection use (Internal => 0,
                            PLL_X => 1,
                            PLL_Y => 2,
                            PLL_Z => 3,
                            PLL_Ext_32kHz => 4,
                            PLL_Ext_19MHz => 5,
                            Stop => 7);

   type Power_Management1 is record
      Device_Reset    : Boolean;
      Sleep           : Boolean;
      Cycle           : Boolean;
      Reserved_4      : Boolean;
      Temp_Disable    : Boolean;
      Clock_Selection : MPU60x0_Clock_Selection;
   end record
     with Size => 8;
   for Power_Management1 use record
      Device_Reset    at 0 range 7 .. 7;
      Sleep           at 0 range 6 .. 6;
      Cycle           at 0 range 5 .. 5;
      Reserved_4      at 0 range 4 .. 4;
      Temp_Disable    at 0 range 3 .. 3;
      Clock_Selection at 0 range 0 .. 2;
   end record;
   POWER_MANAGEMENT1_ADDRESS : constant UInt8 := 16#6B#;

   type MPU60x0_WakeUp_Freq is (Hz1_25, Hz5, Hz20, Hz40)
     with Size => 2;
   for MPU60x0_WakeUp_Freq use (Hz1_25 => 0, Hz5 => 1, Hz20 => 2, Hz40 => 3);

   type Power_Management2 is record
      WakeUp_Freq : MPU60x0_WakeUp_Freq;
      XA_Standby  : Boolean;
      YA_Standby  : Boolean;
      ZA_Standby  : Boolean;
      XG_Standby  : Boolean;
      YG_Standby  : Boolean;
      ZG_Standby  : Boolean;
   end record
     with Size => 8;
   for Power_Management2 use record
      WakeUp_Freq at 0 range 6 .. 7;
      XA_Standby  at 0 range 5 .. 5;
      YA_Standby  at 0 range 4 .. 4;
      ZA_Standby  at 0 range 3 .. 3;
      XG_Standby  at 0 range 2 .. 2;
      YG_Standby  at 0 range 1 .. 1;
      ZG_Standby  at 0 range 0 .. 0;
   end record;
   POWER_MANAGEMENT2_ADDRESS : constant UInt8 := 16#6C#;

   --  TODO : FiFO count and Data

   type Who_Am_I is record
      Reserved_7 : Boolean;
      Whoami     : UInt6;
      Reserved_0 : Boolean;
   end record
     with Size => 8;
   for Who_Am_I use record
      Reserved_7 at 0 range 7 .. 7;
      Whoami     at 0 range 1 .. 6;
      Reserved_0 at 0 range 0 .. 0;
   end record;
   WHOAMI_ADDRESS : constant UInt8 := 16#75#;
   WHOAMI_VALUE : constant UInt6 := 2#110100#;


   type Byte_Array is array (Positive range <>) of UInt8
     with Alignment => 2;

   type MPU60x0_Device is tagged limited record
      Conf : MPU60x0_Configuration;
   end record;

   procedure Read_Port (This : MPU60x0_Device;
                        Address : UInt8;
                        Data : out Byte_Array);

   procedure Write_Port (This : MPU60x0_Device;
                         Address : UInt8;
                         Data : UInt8);

end MPU60x0;
