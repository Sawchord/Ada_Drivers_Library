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

with Ada.Unchecked_Conversion;
with Ada.Numerics.Generic_Elementary_Functions;

package body MPU60x0 is

   package Float_Math is new Ada.Numerics.Generic_Elementary_Functions (Float);
   use Float_Math;

   -- TODO: Since all the unchecked conversions are the same, they should
   -- be declared here, rather than at function level

   subtype Dispatch is MPU60x0_Device'Class;

   procedure Configure (This : in out MPU60x0_Device;
                        Conf : MPU60x0_Configuration) is

      Device_Not_Found : Exception;

      D_Id : Byte_Array (1..1);

      ST_Values : MPU60x0_Self_Test_Response;
   begin
      --  Save configuration
      This.Conf := Conf;

      --  Check WHOAMI
      Dispatch (This).Read_Port (WHOAMI_ADDRESS, D_Id);
      if D_Id (1) /= WHOAMI_VALUE then
         raise Device_Not_Found;
      end if;

      -- TODO : Make it possible to skip Selftest
      -- TODO : Move Selftest behind the Prescaler and Power settings
      This.Get_ST_Results (ST_Values);

      -- Configure Accel and Gyro
      declare
         function A2U is new Ada.Unchecked_Conversion (Accel_Config, UInt8);
         Acc_Conf : constant Accel_Config :=
           (XA_Selftest => False, YA_Selftest => False, ZA_Selftest => False,
            Scale_Range => This.Conf.Accel_Scale_Range,
            others => 0);

         function G2U is new Ada.Unchecked_Conversion (Gyro_Config, UInt8);
         Gyro_Conf : constant Gyro_Config :=
           (XG_Selftest => False, YG_Selftest => False, ZG_Selftest => False,
            Scale_Range => This.Conf.Gyro_Scale_Range,
            others => 0);
      begin
         Dispatch (This).Write_Port (ACCEL_CONFIG_ADDRESS, A2U(Acc_Conf));
         Dispatch (This).Write_Port (GYRO_CONFIG_ADDRESS, G2U(Gyro_Conf));
      end;

      -- TODO: Make the Filter value settable and calculate the prescaler value
      Dispatch (This).Write_Port (SR_DIV_ADDRESS, 16#F8#);

      --  Wait for the clock to settle
      This.Conf.Time.Delay_Microseconds (1000);

      --  Set the Power Management accordingly
      declare
         function PM12U is new Ada.Unchecked_Conversion (Power_Management1,
                                                         UInt8);
         --  TODO: Make it possible to disable readouts
         PM1_Conf : constant Power_Management1 :=
           (Temp_Disable => False,
            Clock_Selection => PLL_X,
            others => False);

         function PM22U is new Ada.Unchecked_Conversion (Power_Management2,
                                                         UInt8);
         PM2_Conf : constant Power_Management2 :=
           (WakeUp_Freq => Hz1_25,
            XA_Standby => False, YA_Standby => False, ZA_Standby => False,
            XG_Standby => False, YG_Standby => False, ZG_Standby => False);
      begin
         Dispatch (This).Write_Port (POWER_MANAGEMENT2_ADDRESS,
                                     PM22U (PM2_Conf));
         Dispatch (This).Write_Port (POWER_MANAGEMENT1_ADDRESS,
                                     PM12U (PM1_Conf));
      end;

   end Configure;

   procedure Read_Values (This : MPU60x0_Device;
                          Values : in out MPU60x0_Sensor_Reading) is

      function Raw2Value is
        new Ada.Unchecked_Conversion (Byte_Array, MPU60x0_Sensor_Reading);

      C_Data : Byte_Array (1 .. MPU60x0_Sensor_Reading'Size / 8);
   begin
      Dispatch (This).Read_Port (SENSOR_DATA_ADDRESS, C_Data);
      Values := Raw2Value (C_Data);
   end Read_Values;

   procedure Read_Values_Float (This : MPU60x0_Device;
                                Values : in out MPU60x0_Sensor_Reading_Float) is

      Int_Values : MPU60x0_Sensor_Reading;

      Accel_LSB : Float;
      Gyro_LSB : Float;
   begin
      This.Read_Values (Int_Values);

      -- TODO: Can we make this prettier?
      case This.Conf.Accel_Scale_Range is
         when g2  => Accel_LSB := 16_384.0;
         when g4  => Accel_LSB := 8_192.0;
         when g8  => Accel_LSB := 4_096.0;
         when g16 => Accel_LSB := 2_048.0;
      end case;
      case This.Conf.Gyro_Scale_Range is
         when deg_250_s  => Gyro_LSB := 131.0;
         when deg_500_s  => Gyro_LSB := 65.5;
         when deg_1000_s => Gyro_LSB := 32.8;
         when deg_2500_s => Gyro_LSB := 16.4;
      end case;

      Values :=
        (Accel_X => Float (Int_Values.Accel_X) / Accel_LSB,
         Accel_Y => Float (Int_Values.Accel_Y) / Accel_LSB,
         Accel_Z => Float (Int_Values.Accel_Z) / Accel_LSB,
         Temp => Float (Int_Values.Temp) / 340.0 + 35.0,
         Gyro_X => Float (Int_Values.Gyro_X) / Gyro_LSB,
         Gyro_Y => Float (Int_Values.Gyro_Y) / Gyro_LSB,
         Gyro_Z => Float (Int_Values.Gyro_Z) / Gyro_LSB);
   end Read_Values_Float;


   procedure Read_Port (This : MPU60x0_Device;
                        Address : UInt8;
                        Data : out Byte_Array) is
      Not_Implemented_Error : exception;
   begin
      raise Not_Implemented_Error;
   end Read_Port;

   procedure Write_Port (This : MPU60x0_Device;
                         Address : UInt8;
                         Data : UInt8) is
      Not_Implemented_Error : exception;
   begin
      raise Not_Implemented_Error;
   end Write_Port;

   procedure Get_ST_Results (This : MPU60x0_Device;
                             Values : out MPU60x0_Self_Test_Response) is

      function To_ST_Reg is new Ada.Unchecked_Conversion (Byte_Array,
                                                          Self_Test_Registers);

      ST_Reg : Self_Test_Registers;
      Read1, Read2 : MPU60x0_Sensor_Reading_Float;
      Data : Byte_Array(1 .. 4);
   begin

      --  Read the Factory trimming setting
      Dispatch (This).Read_Port (SELF_TEST_REG_ADDRESS, Data);
      ST_Reg := To_ST_Reg (Data);

      Values.XA := Get_A_FT (ST_Reg.XA_Test_H, ST_Reg.XA_Test_L);
      Values.YA := Get_A_FT (ST_Reg.YA_Test_H, ST_Reg.YA_Test_L);
      Values.ZA := Get_A_FT (ST_Reg.ZA_Test_H, ST_Reg.ZA_Test_L);
      Values.XG := Get_G_FT (ST_Reg.XG_Test);
      Values.YG := Get_G_FT (ST_Reg.YG_Test);
      Values.ZG := Get_G_FT (ST_Reg.ZG_Test);

      -- Set the Device under selftest
      declare
         function A2U is new Ada.Unchecked_Conversion (Accel_Config, UInt8);
         Acc_Conf : constant Accel_Config :=
           (XA_Selftest => True, YA_Selftest => True, ZA_Selftest => True,
            Scale_Range => g2, --  From Datasheet
            others => 0);

         function G2U is new Ada.Unchecked_Conversion (Gyro_Config, UInt8);
         Gyro_Conf : constant Gyro_Config :=
           (XG_Selftest => True, YG_Selftest => True, ZG_Selftest => True,
            Scale_Range => deg_250_s, --  From Datasheet
            others => 0);
      begin
         Dispatch (This).Write_Port (ACCEL_CONFIG_ADDRESS, A2U(Acc_Conf));
         Dispatch (This).Write_Port (GYRO_CONFIG_ADDRESS, G2U(Gyro_Conf));
      end;

      -- TODO : Make this dependent on the samplerate
      --  Wait for the next sample
      This.Conf.Time.Delay_Microseconds (200000);
      This.Read_Values_Float (Read1);

      --  Set device back to normal measurement
      declare
         function A2U is new Ada.Unchecked_Conversion (Accel_Config, UInt8);
         Acc_Conf : constant Accel_Config :=
           (XA_Selftest => False, YA_Selftest => False, ZA_Selftest => False,
            Scale_Range => g2,
            others => 0);

         function G2U is new Ada.Unchecked_Conversion (Gyro_Config, UInt8);
         Gyro_Conf : constant Gyro_Config :=
           (XG_Selftest => False, YG_Selftest => False, ZG_Selftest => False,
            Scale_Range => deg_250_s,
            others => 0);
      begin
         Dispatch (This).Write_Port (ACCEL_CONFIG_ADDRESS, A2U(Acc_Conf));
         Dispatch (This).Write_Port (GYRO_CONFIG_ADDRESS, G2U(Gyro_Conf));
      end;

      This.Conf.Time.Delay_Microseconds (200000);
      This.Read_Values_Float (Read2);

      Values.XA := Get_Deriv (Read1.Accel_X, Read2.Accel_X, Values.XA);
      Values.YA := Get_Deriv (Read1.Accel_Y, Read2.Accel_Y, Values.YA);
      Values.ZA := Get_Deriv (Read1.Accel_Z, Read2.Accel_Z, Values.ZA);
      Values.XG := Get_Deriv (Read1.Gyro_X, Read2.Gyro_X, Values.XG);
      Values.YG := Get_Deriv (Read1.Gyro_Y, Read2.Gyro_Y, Values.YG);
      Values.ZG := Get_Deriv (Read1.Gyro_Z, Read2.Gyro_Z, Values.ZG);

   end Get_ST_Results;

   function Get_Deriv (Read1 : Float; Read2 : Float; FT : Float) return Float is
   begin
      return ((FT - (Read1 - Read2)) / FT) - 1.0;
   end Get_Deriv;

   function Get_G_FT (Input : UInt5) return Float is
   begin
      return 25.0 * 131.0 * (1.046**(Float (Input - 1)));
   end Get_G_FT;

   function Get_A_FT (Input_H : UInt3; Input_L : UInt2) return Float is
      Val : constant UInt6 := UInt6 (Input_H) * 4 + UInt6 (Input_L);
   begin
      return 4096.0 * 0.34 * ((0.92 / 0.34)**(Float (Val - 1) / 30.0));
   end Get_A_FT;

end MPU60x0;
