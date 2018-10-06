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

--  This program demonstrates the on-board gyro provided by the L3GD20 chip
--  on the STM32F429 Discovery boards. The pitch, roll, and yaw values are
--  continuously displayed on the LCD, as are the adjusted raw values. Move
--  the board to see them change. The values will be positive or negative,
--  depending on the direction of movement. Note that the values are not
--  constant, even when the board is not moving, due to noise.

--  This program demonstrates use of interrupts rather than polling.

--  NB: You may need to reset the board after downloading.

with Last_Chance_Handler;      pragma Unreferenced (Last_Chance_Handler);
-- with Output_Utils; use Output_Utils;

with Ada.Real_Time; use Ada.Real_Time;
--with Ada.Synchronous_Task_Control; use Ada.Synchronous_Task_Control;

with HAL; use HAL;
with HAL.SPI; use HAL.SPI;
with HAL.UART; use HAL.UART;

with STM32.Device;  use STM32.Device;
--with STM32.Board;   use STM32.Board;

with STM32;      use STM32;
with STM32.GPIO; use STM32.GPIO;
with STM32.SPI;  use STM32.SPI;
with STM32.USARTs; use STM32.USARTs;
-- with STM32.EXTI; use STM32.EXTI;

procedure Demo_BMP280 is

   Clk_Pin  : constant GPIO_Point := PE12;
   Miso_Pin : constant GPIO_Point := PE13;
   Mosi_Pin : constant GPIO_Point := PE14;
   SPI_Pins : constant GPIO_Points := (Clk_Pin, Miso_Pin, Mosi_Pin);
   Cs_Pin : GPIO_Point := PE11;

   UART_Pins : constant GPIO_Points := (PA9, PA10);


   procedure Initialize_IO is
   begin
      Enable_Clock (SPI_4);
      Enable_Clock (SPI_Pins);
      Enable_Clock (Cs_Pin);

      Enable_Clock (UART_Pins);
      Enable_Clock (USART_1);


      Configure_IO(SPI_Pins,
                   (Mode           => Mode_AF,
                    AF             => GPIO_AF_SPI4_5,
                    Resistors      => Pull_Up,
                    AF_Speed       => Speed_50MHz,
                    AF_Output_Type => Push_Pull));

      Configure_IO(Cs_Pin,
                   (Mode           => Mode_Out,
                    Output_Type => Push_Pull,
                    Speed => Speed_50MHz,
                    Resistors => Floating));

      Configure_IO(UART_Pins,
                   (Mode => Mode_AF,
                    AF => GPIO_AF_USART1_7,
                    Resistors => Pull_Up,
                    AF_Speed => Speed_50MHz,
                    AF_Output_Type => Push_Pull));

   end Initialize_IO;

   procedure Initialize is
   begin

      Disable (SPI_4);

      Initialize_IO;

      Configure (SPI_4, (
                 Direction => D2Lines_FullDuplex,
                 Mode => Master,
                 Data_Size => Data_Size_8b,
                 Clock_Polarity => Low,
                 Clock_Phase => P1Edge,
                 Slave_Management => Software_Managed,
                 Baud_Rate_Prescaler => BRP_256,
                 First_Bit => LSB,
                 CRC_Poly => 0
                ));

      Enable (SPI_4);

      Disable (USART_1);

      USART_1.Set_Baud_Rate(115200);
      USART_1.Set_Mode(Tx_Rx_Mode);
      USART_1.Set_Word_Length(Word_Length_9);
      USART_1.Set_Parity(No_Parity);
      USART_1.Set_Flow_Control(No_Flow_Control);

      Enable (USART_1);
   end Initialize;


   procedure Await_Tx is
   begin
      while not USART_1.Tx_Ready loop
         null;
      end loop;
   end Await_Tx;


   procedure Put (s : String) is
   begin
      for I in s'First..s'Last loop
         Await_Tx;
         USART_1.Transmit(Uint9(Character'Pos(s(I))));
      end loop;
   end Put;

   procedure New_Line is
   begin
      Await_Tx;
      USART_1.Transmit (UInt9(13));
      Await_Tx;
      USART_1.Transmit (UInt9(10));
   end New_Line;

   procedure Put_Line(s : String) is
   begin
      Put(s);
      New_Line;
   end Put_Line;

   T : Time := Clock;

   Test_Data : constant SPI_Data_8b := (1, 2, 3, 4);
   Status1 : SPI_Status := Err_Error;

   Count : Integer := 0;
begin

   Initialize;

   loop

      Cs_Pin.Clear;
      SPI_4.Transmit(Test_Data, Status1);
      Cs_Pin.Set;

      Put_Line ("Hello  " & Count'Img);

      Count := Count + 1;

      T := T + Milliseconds (200);
      delay until T;

   end loop;
end Demo_BMP280;
