------------------------------------------------------------------------------
--                                                                          --
--                  Copyright (C) 2016-2017, AdaCore                        --
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

with Last_Chance_Handler;      pragma Unreferenced (Last_Chance_Handler);
-- with Output_Utils; use Output_Utils;

with Ada.Real_Time; use Ada.Real_Time;

with HAL; use HAL;
with HAL.SPI; use HAL.SPI;
with HAL.UART; use HAL.UART;
with HAL.I2C; use HAL.I2C;

with STM32.Device;  use STM32.Device;

with STM32;      use STM32;
with STM32.GPIO; use STM32.GPIO;
with STM32.SPI;  use STM32.SPI;
with STM32.I2C; use STM32.I2C;
with STM32.USARTs; use STM32.USARTs;

with STM32_SVD.I2C;

with BMP280; use BMP280;
with BMP280.SPI;
with BMP280.I2C;

with System.Machine_Code;

procedure Demo_BMP280 is

   Clk_Pin  : constant GPIO_Point := PB3;
   Miso_Pin : constant GPIO_Point := PB4;
   Mosi_Pin : constant GPIO_Point := PB5;
   SPI_Pins : constant GPIO_Points := (Clk_Pin, Miso_Pin, Mosi_Pin);
   Cs_Pin : GPIO_Point := PD7;

   Sca_Pin :  GPIO_Point := PB10;
   Scl_Pin :  GPIO_Point := PB11;
   --Sca_Pin :  GPIO_Point := PC9;
   --Scl_Pin :  GPIO_Point := PA8;
   I2C_Pins : constant GPIO_Points := (Sca_Pin, Scl_Pin);

   UART_Pins : constant GPIO_Points := (PA2, PA3);


   procedure Initialize_UART is
   begin

      Enable_Clock (UART_Pins);
      Enable_Clock (USART_2);

      Configure_IO(UART_Pins,
                   (Mode => Mode_AF,
                    AF => GPIO_AF_USART1_7,
                    Resistors => Pull_Up,
                    AF_Speed => Speed_50MHz,
                    AF_Output_Type => Push_Pull));

      Disable (USART_2);

      USART_2.Set_Baud_Rate(115200);
      USART_2.Set_Mode(Tx_Rx_Mode);
      USART_2.Set_Word_Length(Word_Length_9);
      USART_2.Set_Parity(No_Parity);
      USART_2.Set_Flow_Control(No_Flow_Control);

      Enable (USART_2);

   end Initialize_UART;

   procedure Initialize_I2C is
   begin

      Enable_Clock (I2C_Pins);
      Enable_Clock (I2C_2);

      Configure_IO(I2C_Pins,
                   (Mode => Mode_AF,
                    AF => GPIO_AF_I2C1_4,
                    Resistors => Pull_Up,
                    AF_Speed => Speed_50MHz,
                    AF_Output_Type => Open_Drain));

      Configure(I2C_2, (
                Clock_Speed => 100_000,
                Mode => I2C_Mode,
                Duty_Cycle => DutyCycle_2,
                Addressing_Mode => Addressing_Mode_7bit,
                Own_Address => 0,
                others => <>));
      I2C_2.Set_State (True);

   end Initialize_I2C;

   procedure Initialize_SPI is
   begin
      Enable_Clock (SPI_1);
      Enable_Clock (SPI_Pins);
      Enable_Clock (Cs_Pin);

      Configure_IO(SPI_Pins,
                   (Mode           => Mode_AF,
                    AF             => GPIO_AF_SPI1_5,
                    Resistors      => Pull_Up,
                    AF_Speed       => Speed_50MHz,
                    AF_Output_Type => Push_Pull));

      Configure_IO(Cs_Pin,
                   (Mode           => Mode_Out,
                    Output_Type => Push_Pull,
                    Speed => Speed_50MHz,
                    Resistors => Floating));

      Disable (SPI_1);

      Configure (SPI_1, (
                 Direction => D2Lines_FullDuplex,
                 Mode => Master,
                 Data_Size => Data_Size_8b,
                 Clock_Polarity => Low,
                 Clock_Phase => P1Edge,
                 Slave_Management => Software_Managed,
                 Baud_Rate_Prescaler => BRP_256,
                 First_Bit => MSB,
                 CRC_Poly => 0
                ));

      Enable (SPI_1);

   end Initialize_SPI;

   procedure Await_Tx is
   begin
      while not USART_2.Tx_Ready loop
         null;
      end loop;
   end Await_Tx;


   procedure Put (s : String) is
   begin
      for I in s'First..s'Last loop
         Await_Tx;
         USART_2.Transmit(Uint9(Character'Pos(s(I))));
      end loop;
   end Put;

   procedure New_Line is
   begin
      Await_Tx;
      USART_2.Transmit (UInt9(13));
      Await_Tx;
      USART_2.Transmit (UInt9(10));
   end New_Line;

   procedure Put_Line(s : String) is
   begin
      Put(s);
      New_Line;
   end Put_Line;

   function Height_From_Pressure(P : Float) return Float is
   begin
      return (101300.0 - P) / 11.7;
   end Height_From_Pressure;

   T : Time := Clock;
   Count : Integer := 0;

   package BMP280_SPI is new BMP280.SPI (BMP280_Device);
   Sensor1 : BMP280_SPI.SPI_BMP280_Device (SPI_1'Access, PD7'Access);

   package BMP280_I2C is new BMP280.I2C (BMP280_Device);
   Sensor2 : BMP280_I2C.I2C_BMP280_Device (I2C_2'Access, BMP280_I2C.Low);

   IData : SPI_Data_8b := (16#D0#, 0);
begin

   Initialize_UART;
   T := T + Milliseconds (50);
   delay until T;

   Initialize_I2C;
   Initialize_SPI;
   Cs_Pin.Set;
   T := T + Milliseconds (50);
   delay until T;

   -- Configure the pressure sensor
   declare
      Conf : BMP280_Configuration :=
        (Standby_Time => ms125,
         Temperature_Oversampling => x16,
         Pressure_Oversampling => x16,
         Filter_Coefficient => 0);
   begin
      Sensor1.Configure(Conf);
      Sensor2.Configure(Conf);
      null;
   end;

   loop

      declare
         Values : BMP280_Values_Float;
      begin
         Sensor1.Read_Values_Float(Values);
         Put_Line("Temp1: " & Values.Temperature'Img);
         Put_Line("Pres1: " & Values.Pressure'Img);

         Sensor2.Read_Values_Float(Values);
         Put_Line("Temp2: " & Values.Temperature'Img);
         Put_Line("Pres2: " & Values.Pressure'Img);

         --Put_Line("Height: " & Height_From_Pressure(Values.Pressure)'Img);
         null;
      end;

      Put_Line ("Hello  " & Count'Img);
      Count := Count + 1;

      T := T + Milliseconds (200);
      delay until T;

   end loop;
end Demo_BMP280;
