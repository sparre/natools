------------------------------------------------------------------------------
-- Copyright (c) 2014, Natacha Porté                                        --
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

package body Natools.S_Expressions.Atom_Buffers is

   procedure Preallocate (Buffer : in out Atom_Buffer; Length : in Count) is
      Old_Size, New_Size : Count := 0;
   begin
      if Buffer.Used + Length <= Buffer.Available then
         return;
      end if;

      Old_Size := Buffer.Available;
      New_Size := Buffer.Used + Length;

      if Buffer.Ref.Is_Empty then
         declare
            function Create return Atom;

            function Create return Atom is
            begin
               return Atom'(1 .. New_Size => <>);
            end Create;
         begin
            Buffer.Ref.Replace (Create'Access);
         end;
      else
         declare
            function Create return Atom;

            Old_Accessor : constant Atom_Refs.Accessor := Buffer.Ref.Query;

            function Create return Atom is
            begin
               return Result : Atom (1 .. New_Size) do
                  Result (1 .. Old_Size) := Old_Accessor.Data.all;
               end return;
            end Create;
         begin
            Buffer.Ref.Replace (Create'Access);
         end;
      end if;

      Buffer.Available := New_Size;
   end Preallocate;


   procedure Append (Buffer : in out Atom_Buffer; Data : in Atom) is
   begin
      Preallocate (Buffer, Data'Length);
      Buffer.Ref.Update.Data.all (Buffer.Used + 1 .. Buffer.Used + Data'Length)
        := Data;
      Buffer.Used := Buffer.Used + Data'Length;
   end Append;


   procedure Append (Buffer : in out Atom_Buffer; Data : in Octet) is
   begin
      Preallocate (Buffer, 1);
      Buffer.Ref.Update.Data.all (Buffer.Used + 1) := Data;
      Buffer.Used := Buffer.Used + 1;
   end Append;


   function Length (Buffer : Atom_Buffer) return Count is
   begin
      return Buffer.Used;
   end Length;


   function Query (Buffer : Atom_Buffer) return Atom is
   begin
      if Buffer.Ref.Is_Empty then
         pragma Assert (Buffer.Available = 0 and Buffer.Used = 0);
         return Null_Atom;
      else
         return Buffer.Ref.Query.Data.all;
      end if;
   end Query;


   function Query (Buffer : Atom_Buffer) return Atom_Refs.Accessor is
      function Create return Atom;

      function Create return Atom is
      begin
         return Null_Atom;
      end Create;
   begin
      if Buffer.Ref.Is_Empty then
         return Atom_Refs.Create (Create'Access).Query;
      else
         return Buffer.Ref.Query;
      end if;
   end Query;


   procedure Query
     (Buffer : in Atom_Buffer;
      Process : not null access procedure (Data : in Atom)) is
   begin
      if Buffer.Ref.Is_Empty then
         Process.all (Null_Atom);
      else
         Buffer.Ref.Query (Process);
      end if;
   end Query;


   procedure Query
     (Buffer : in Atom_Buffer;
      Data : out Atom;
      Length : out Count)
   is
      Transmit : constant Count := Count'Min (Data'Length, Buffer.Used);
   begin
      Length := Buffer.Used;

      if Buffer.Ref.Is_Empty then
         pragma Assert (Length = 0);
         null;
      else
         Data (Data'First .. Data'First + Transmit - 1)
           := Buffer.Ref.Query.Data.all (1 .. Transmit);
      end if;
   end Query;


   function Element (Buffer : Atom_Buffer; Position : Count) return Octet is
   begin
      return Buffer.Ref.Query.Data.all (Position);
   end Element;


   procedure Hard_Reset (Buffer : in out Atom_Buffer) is
   begin
      Buffer.Ref.Reset;
      Buffer.Available := 0;
      Buffer.Used := 0;
   end Hard_Reset;


   procedure Soft_Reset (Buffer : in out Atom_Buffer) is
   begin
      Buffer.Used := 0;
   end Soft_Reset;

end Natools.S_Expressions.Atom_Buffers;