------------------------------------------------------------------------------
-- Copyright (c) 2013, Natacha Porté                                        --
--                                                                          --
-- Permission to use, copy, modify, and distribute this software for any    --
-- purpose with or without fee is hereby granted, provided that the above   --
-- copyright notice and this permission notice appear in all copies.        --
--                                                                          --
-- THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES --
-- WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF         --
-- MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR  --
-- ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES   --
-- WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN    --
-- ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF  --
-- OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.           --
------------------------------------------------------------------------------

with Natools.S_Expressions.Test_Tools;

package body Natools.S_Expressions.Encodings.Tests is

   procedure All_Tests (Report : in out NT.Reporter'Class) is
   begin
      Report.Section ("Encodings in S-expressions");
      Hexadecimal_Test (Report);
      Base64_Test (Report);
      Report.End_Section;
   end All_Tests;


   procedure Hexadecimal_Test (Report : in out NT.Reporter'Class) is
      All_Octets : Atom (1 .. 256);
   begin
      for I in All_Octets'Range loop
         All_Octets (I) := Octet (I - All_Octets'First);
      end loop;

      declare
         Name : constant String := "Decoding upper-case hexadecimal encoded";
      begin
         Test_Tools.Test_Atom
           (Report, Name,
            All_Octets,
            Decode_Hex (Encode_Hex (All_Octets, Upper)));
      exception
         when Error : others => Report.Report_Exception (Name, Error);
      end;

      declare
         Name : constant String := "Decoding lower-case hexadecimal encoded";
      begin
         Test_Tools.Test_Atom
           (Report, Name,
            All_Octets,
            Decode_Hex (Encode_Hex (All_Octets, Lower)));
      exception
         when Error : others => Report.Report_Exception (Name, Error);
      end;

      declare
         Name : constant String := "Decoding garbage-laced text";
      begin
         Test_Tools.Test_Atom
           (Report, Name,
            (16#01#, 16#23#, 16#45#, 16#67#, 16#89#,
             16#AB#, 16#CD#, 16#EF#, 16#AB#, 16#CD#, 16#EF#),
            Decode_Hex (All_Octets));
      exception
         when Error : others => Report.Report_Exception (Name, Error);
      end;
   end Hexadecimal_Test;


   procedure Base64_Test (Report : in out NT.Reporter'Class) is
   begin
      declare
         Name : constant String := "Decoding encoding of all octet triplets";
         Success : Boolean := True;
         Expected : Atom (1 .. 3);
      begin
         for A in Octet loop
            Expected (1) := A;
            for B in Octet loop
               Expected (2) := B;
               for C in Octet loop
                  Expected (3) := C;

                  declare
                     Found : constant Atom
                       := Decode_Base64 (Encode_Base64 (Expected));
                  begin
                     if Expected /= Found then
                        if Success then
                           Success := False;
                           Report.Item (Name, NT.Fail);
                        end if;

                        Test_Tools.Dump_Atom (Report, Found, "Found");
                        Test_Tools.Dump_Atom (Report, Expected, "Expected");
                     end if;
                  end;
               end loop;
            end loop;
         end loop;

         if Success then
            Report.Item (Name, NT.Success);
         end if;
      exception
         when Error : others => Report.Report_Exception (Name, Error);
      end;

      declare
         Name : constant String := "Decoding encoding of all octet duets";
         Success : Boolean := True;
         Expected : Atom (1 .. 2);
      begin
         for A in Octet loop
            Expected (1) := A;
            for B in Octet loop
               Expected (2) := B;

               declare
                  Found : constant Atom
                    := Decode_Base64 (Encode_Base64 (Expected));
               begin
                  if Expected /= Found then
                     if Success then
                        Success := False;
                        Report.Item (Name, NT.Fail);
                     end if;

                     Test_Tools.Dump_Atom (Report, Found, "Found");
                     Test_Tools.Dump_Atom (Report, Expected, "Expected");
                  end if;
               end;
            end loop;
         end loop;

         if Success then
            Report.Item (Name, NT.Success);
         end if;
      exception
         when Error : others => Report.Report_Exception (Name, Error);
      end;

      declare
         Name : constant String := "Decoding encoding of all single octets";
         Success : Boolean := True;
         Expected : Atom (1 .. 1);
      begin
         for A in Octet loop
            Expected (1) := A;

            declare
               Found : constant Atom
                 := Decode_Base64 (Encode_Base64 (Expected));
            begin
               if Expected /= Found then
                  if Success then
                     Success := False;
                     Report.Item (Name, NT.Fail);
                  end if;

                  Test_Tools.Dump_Atom (Report, Found, "Found");
                  Test_Tools.Dump_Atom (Report, Expected, "Expected");
               end if;
            end;
         end loop;

         if Success then
            Report.Item (Name, NT.Success);
         end if;
      exception
         when Error : others => Report.Report_Exception (Name, Error);
      end;
   end Base64_Test;

end Natools.S_Expressions.Encodings.Tests;