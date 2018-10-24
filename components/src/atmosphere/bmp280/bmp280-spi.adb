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
package body BMP280.SPI is

   overriding
   procedure Read_Port (This : SPI_BMP280_Device;
                        Address : UInt8;
                        Data : out Byte_Array) is

      SPI_Address : constant SPI_Data_8b (1 .. 1) := (1 => Address);
      SPI_Data : SPI_Data_8b (Data'Range);
      Status : SPI_Status;
   begin

      --   TODO: Make this possible without need to copy data afterwards
      This.Cs.Clear;
      This.Port.Transmit (SPI_Address, Status);
      This.Port.Receive (SPI_Data, Status);
      This.Cs.Set;

      Data (Data'Range) := Byte_Array (SPI_Data (Data'Range));
   end Read_Port;

   overriding
   procedure Write_Port (This : SPI_BMP280_Device;
                         Address : UInt8;
                         Data : UInt8) is

      Write_Mask : constant UInt8 := 2#01111111#;
      SPI_Data : constant SPI_Data_8b (1 .. 2) := (1 => Address and Write_Mask,
                                                   2 => Data);
      Status : SPI_Status;
   begin
      This.Cs.Clear;
      This.Port.Transmit (SPI_Data, Status);
      This.Cs.Set;
   end Write_Port;


end BMP280.SPI;
