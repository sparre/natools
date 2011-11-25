------------------------------------------------------------------------------
-- Copyright (c) 2011, Natacha Porté                                        --
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

with Ada.Strings.Fixed;
with Ada.Strings.Maps;

package body Natools.Getopt_Long is

   package Fixed renames Ada.Strings.Fixed;
   package Maps renames Ada.Strings.Maps;

   ----------------------------
   -- Option list management --
   ----------------------------

   procedure Add_Option
     (Options    : in out Option_Definitions;
      Long_Name  : String;
      Short_Name : Character;
      Has_Arg    : Argument_Requirement;
      Id         : Option_Id)
   is
      New_Option : constant Option
        := (Long_Name_Length => Long_Name'Length,
            Id               => Id,
            Has_Arg          => Has_Arg,
            Long_Name        => Long_Name,
            Short_Name       => Short_Name);
   begin
      if Long_Name = Null_Long_Name or Short_Name = Null_Short_Name then
         raise Constraint_Error;
      end if;
      Options.By_Long_Name.Insert (Long_Name, New_Option);
      Options.By_Short_Name.Insert (Short_Name, New_Option);
   end Add_Option;


   procedure Add_Option
     (Options    : in out Option_Definitions;
      Long_Name  : String;
      Has_Arg    : Argument_Requirement;
      Id         : Option_Id)
   is
      New_Option : constant Option
        := (Long_Name_Length => Long_Name'Length,
            Id               => Id,
            Has_Arg          => Has_Arg,
            Long_Name        => Long_Name,
            Short_Name       => Null_Short_Name);
   begin
      if Long_Name = Null_Long_Name then
         raise Constraint_Error;
      end if;
      Options.By_Long_Name.Insert (Long_Name, New_Option);
   end Add_Option;


   procedure Add_Option
     (Options    : in out Option_Definitions;
      Short_Name : Character;
      Has_Arg    : Argument_Requirement;
      Id         : Option_Id)
   is
      New_Option : constant Option
        := (Long_Name_Length => 0,
            Id               => Id,
            Has_Arg          => Has_Arg,
            Long_Name        => Null_Long_Name,
            Short_Name       => Short_Name);
   begin
      if Short_Name = Null_Short_Name then
         raise Constraint_Error;
      end if;
      Options.By_Short_Name.Insert (Short_Name, New_Option);
   end Add_Option;


   procedure Del_Option
     (Options    : in out Option_Definitions;
      Id         : Option_Id)
   is
      Short_Name_Cursor : Short_Option_Maps.Cursor
        := Options.By_Short_Name.First;
      Long_Name_Cursor : Long_Option_Maps.Cursor
        := Options.By_Long_Name.First;
   begin
      while Short_Option_Maps.Has_Element (Short_Name_Cursor) loop
         declare
            Next : constant Short_Option_Maps.Cursor
              := Short_Option_Maps.Next (Short_Name_Cursor);
         begin
            if Short_Option_Maps.Element (Short_Name_Cursor).Id = Id then
               Options.By_Short_Name.Delete (Short_Name_Cursor);
            end if;
            Short_Name_Cursor := Next;
         end;
      end loop;
      while Long_Option_Maps.Has_Element (Long_Name_Cursor) loop
         declare
            Next : constant Long_Option_Maps.Cursor
              := Long_Option_Maps.Next (Long_Name_Cursor);
         begin
            if Long_Option_Maps.Element (Long_Name_Cursor).Id = Id then
               Options.By_Long_Name.Delete (Long_Name_Cursor);
            end if;
            Long_Name_Cursor := Next;
         end;
      end loop;
   end Del_Option;


   procedure Del_Option
     (Options    : in out Option_Definitions;
      Long_Name  : String) is
   begin
      Options.By_Long_Name.Delete (Long_Name);
   end Del_Option;


   procedure Del_Option
     (Options    : in out Option_Definitions;
      Short_Name : Character) is
   begin
      Options.By_Short_Name.Delete (Short_Name);
   end Del_Option;



   ----------------------------
   -- Formatting subprograms --
   ----------------------------

   function Format_Long_Names
     (Options     : Option_Definitions;
      Id          : Option_Id;
      Separator   : String := ", ";
      Name_Prefix : String := "--")
      return String
   is
      Long_Name_Count : constant Natural := Get_Long_Name_Count (Options, Id);
      Space_Per_Name : constant Positive
        := Name_Prefix'Length + 1 + Separator'Length;
      Result : String (1 .. Long_Name_Count * Space_Per_Name);
   begin
      if Long_Name_Count = 0 then
         return "";
      end if;
      for J in 1 .. Long_Name_Count loop
         declare
            First : constant Positive
              := Result'First + (J - 1) * Space_Per_Name;
            Name : constant String := Get_Long_Name (Options, Id, J);
         begin
            Result (First .. First + Name_Prefix'Length - 1) := Name_Prefix;
            Result (First + Name_Prefix'Length ..
                    First + Name_Prefix'Length + Name'Length - 1)
              := Name;
            Result (First + Name_Prefix'Length + Name'Length ..
                    First + Space_Per_Name - 1)
              := Separator;
         end;
      end loop;
      return Result (1 .. Long_Name_Count * Space_Per_Name - Separator'Length);
   end Format_Long_Names;


   function Format_Names
     (Options           : Option_Definitions;
      Id                : Option_Id;
      Separator         : String := ", ";
      Long_Name_Prefix  : String := "--";
      Short_Name_Prefix : String := "-";
      Short_First       : Boolean := True)
      return String
   is
      Long_Names : constant String
        := Format_Long_Names (Options, Id, Separator, Long_Name_Prefix);
      Short_Names : constant String
        := Format_Short_Names (Options, Id, Separator, Short_Name_Prefix);
   begin
      if Long_Names = "" then
         return Short_Names;
      elsif Short_Names = "" then
         return Long_Names;
      elsif Short_First then
         return Short_Names & Separator & Long_Names;
      else
         return Long_Names & Separator & Short_Names;
      end if;
   end Format_Names;


   function Format_Short_Names
     (Options     : Option_Definitions;
      Id          : Option_Id;
      Separator   : String := ", ";
      Name_Prefix : String := "-")
      return String
   is
      Short_Names : constant String := Get_Short_Names (Options, Id);
      Space_Per_Name : constant Positive
        := Name_Prefix'Length + 1 + Separator'Length;
      Result : String (1 .. Short_Names'Length * Space_Per_Name);
   begin
      if Short_Names = "" then
         return "";
      end if;
      for J in Short_Names'Range loop
         declare
            First : constant Positive
              := Result'First + (J - Short_Names'First) * Space_Per_Name;
         begin
            Result (First .. First + Name_Prefix'Length - 1) := Name_Prefix;
            Result (First + Name_Prefix'Length) := Short_Names (J);
            Result (First + Name_Prefix'Length + 1 ..
                     First + Space_Per_Name - 1) := Separator;
         end;
      end loop;
      return Result (Result'First .. Result'Last - Separator'Length);
   end Format_Short_Names;



   function Get_Long_Name
     (Options    : Option_Definitions;
      Id         : Option_Id;
      Index      : Positive := 1)
      return String
   is
      Seen : Natural := 0;
      Cursor : Long_Option_Maps.Cursor := Options.By_Long_Name.First;
   begin
      while Long_Option_Maps.Has_Element (Cursor) loop
         declare
            Opt : constant Option := Long_Option_Maps.Element (Cursor);
         begin
            if Opt.Id = Id then
               Seen := Seen + 1;
               if Seen = Index then
                  return Opt.Long_Name;
               end if;
            end if;
         end;
         Long_Option_Maps.Next (Cursor);
      end loop;
      raise Constraint_Error;
   end Get_Long_Name;


   function Get_Long_Name_Count
     (Options    : Option_Definitions;
      Id         : Option_Id)
      return Natural
   is
      procedure Process (Key : String; Element : Option);
      procedure Process (Cursor : Long_Option_Maps.Cursor);

      Result : Natural := 0;

      procedure Process (Key : String; Element : Option) is
         pragma Unreferenced (Key);
      begin
         if Element.Id = Id then
            Result := Result + 1;
         end if;
      end Process;

      procedure Process (Cursor : Long_Option_Maps.Cursor) is
      begin
         Long_Option_Maps.Query_Element (Cursor, Process'Access);
      end Process;
   begin
      Options.By_Long_Name.Iterate (Process'Access);
      return Result;
   end Get_Long_Name_Count;


   function Get_Short_Name_Count
     (Options    : Option_Definitions;
      Id         : Option_Id)
      return Natural
   is
      procedure Process (Key : Character; Element : Option);
      procedure Process (Cursor : Short_Option_Maps.Cursor);

      Result : Natural := 0;

      procedure Process (Key : Character; Element : Option) is
         pragma Unreferenced (Key);
      begin
         if Element.Id = Id then
            Result := Result + 1;
         end if;
      end Process;

      procedure Process (Cursor : Short_Option_Maps.Cursor) is
      begin
         Short_Option_Maps.Query_Element (Cursor, Process'Access);
      end Process;
   begin
      Options.By_Short_Name.Iterate (Process'Access);
      return Result;
   end Get_Short_Name_Count;


   function Get_Short_Names
     (Options    : Option_Definitions;
      Id         : Option_Id)
      return String
   is
      procedure Process (Key : Character; Element : Option);
      procedure Process (Cursor : Short_Option_Maps.Cursor);

      Result : String (1 .. Options.Get_Short_Name_Count (Id));
      J : Positive := Result'First;

      procedure Process (Key : Character; Element : Option) is
      begin
         if Element.Id = Id then
            Result (J) := Key;
            J := J + 1;
         end if;
      end Process;

      procedure Process (Cursor : Short_Option_Maps.Cursor) is
      begin
         Short_Option_Maps.Query_Element (Cursor, Process'Access);
      end Process;
   begin
      Options.By_Short_Name.Iterate (Process'Access);
      return Result;
   end Get_Short_Names;


   procedure Iterate
     (Options : Option_Definitions;
      Process : not null access procedure (Id : Option_Id;
                                           Long_Name : String;
                                           Short_Name : Character;
                                           Has_Arg : Argument_Requirement))
   is
      procedure Long_Process (Key : String; Opt : Option);
      procedure Long_Query (C : Long_Option_Maps.Cursor);
      procedure Short_Process (Key : Character; Opt : Option);
      procedure Short_Query (C : Short_Option_Maps.Cursor);

      procedure Long_Process (Key : String; Opt : Option) is
         pragma Unreferenced (Key);
      begin
         if Opt.Short_Name = Null_Short_Name then
            Process (Opt.Id, Opt.Long_Name, Opt.Short_Name, Opt.Has_Arg);
         end if;
      end Long_Process;

      procedure Long_Query (C : Long_Option_Maps.Cursor) is
      begin
         Long_Option_Maps.Query_Element (C, Long_Process'Access);
      end Long_Query;

      procedure Short_Process (Key : Character; Opt : Option) is
         pragma Unreferenced (Key);
      begin
         Process (Opt.Id, Opt.Long_Name, Opt.Short_Name, Opt.Has_Arg);
      end Short_Process;

      procedure Short_Query (C : Short_Option_Maps.Cursor) is
      begin
         Short_Option_Maps.Query_Element (C, Short_Process'Access);
      end Short_Query;
   begin
      Options.By_Short_Name.Iterate (Short_Query'Access);
      Options.By_Long_Name.Iterate (Long_Query'Access);
   end Iterate;



   -----------------------------
   -- Command-line processing --
   -----------------------------

   procedure Process
     (Options : Option_Definitions;
      Top_Level_Argument : Option_Id;
      Callback : not null access procedure (Id : Option_Id;
                                            Argument : String);
      Missing_Argument : access procedure (Id : Option_Id) := null;
      Unexpected_Argument : access procedure (Id : Option_Id;
                                              Arg : String) := null;
      Unknown_Long_Option : access procedure (Name : String) := null;
      Unknown_Short_Option : access procedure (Name : Character) := null;
      Posixly_Correct : Boolean := True;
      Long_Only : Boolean := False;
      Argument_Count : not null access function return Natural
        := Ada.Command_Line.Argument_Count'Access;
      Argument : not null access function (Number : Positive) return String
        := Ada.Command_Line.Argument'Access)
   is
      procedure Process_Long_Option (Arg : String);

      Arg_Count : constant Natural := Argument_Count.all;
      Arg_N : Positive := 1;

      procedure Process_Long_Option (Arg : String) is
         function Has_Prefix (C : Long_Option_Maps.Cursor; Prefix : String)
            return Boolean;

         Equal : constant Natural := Fixed.Index (Arg, Maps.To_Set ('='));
         Cursor : Long_Option_Maps.Cursor;
         Arg_Name_Last : Natural := Arg'Last;

         function Has_Prefix (C : Long_Option_Maps.Cursor; Prefix : String)
            return Boolean
         is
            Key : constant String := Long_Option_Maps.Key (C);
         begin
            return Key'Length >= Prefix'Length and then
              Key (1 .. Prefix'Length) = Prefix;
         end Has_Prefix;
      begin
         if Equal /= 0 then
            Arg_Name_Last := Equal - 1;
         end if;
         declare
            Arg_Name : String renames Arg (Arg'First .. Arg_Name_Last);
         begin
            --  Looking for an exact match
            Cursor := Options.By_Long_Name.Find (Arg_Name);
            if not Long_Option_Maps.Has_Element (Cursor) then
               --  Looking for a unique partial match
               Cursor := Options.By_Long_Name.Ceiling (Arg_Name);
               if not Long_Option_Maps.Has_Element (Cursor) or else
                 not Has_Prefix (Cursor, Arg_Name) or else
                 Has_Prefix (Long_Option_Maps.Next (Cursor), Arg_Name)
               then
                  if Unknown_Long_Option = null then
                     raise Option_Error with "Unknown long option " & Arg_Name;
                  else
                     Unknown_Long_Option (Arg_Name);
                     return;
                  end if;
               end if;
            end if;
            --  At this point, Cursor points to the selected argument
            declare
               Opt : constant Option := Long_Option_Maps.Element (Cursor);
            begin
               case Opt.Has_Arg is
                  when No_Argument =>
                     if Equal = 0 then
                        Callback (Opt.Id, "");
                     else
                        if Unexpected_Argument = null then
                           raise Option_Error with "Unexpected argument """
                             & Arg (Equal + 1 .. Arg'Last) & """ to "
                             & Opt.Long_Name;
                        else
                           Unexpected_Argument (Opt.Id,
                                                Arg (Equal + 1 .. Arg'Last));
                        end if;
                     end if;
                  when Optional_Argument =>
                     if Equal = 0 then
                        Callback (Opt.Id, "");
                     else
                        Callback (Opt.Id, Arg (Equal + 1 .. Arg'Last));
                     end if;
                  when Required_Argument =>
                     if Equal = 0 then
                        if Arg_N = Arg_Count then
                           if Missing_Argument = null then
                              raise Option_Error with "Missing argument to "
                                                 & "option " & Opt.Long_Name;
                           else
                              Missing_Argument (Opt.Id);
                           end if;
                        else
                           Callback (Opt.Id, Argument (Arg_N + 1));
                           Arg_N := Arg_N + 1;
                        end if;
                     else
                        Callback (Opt.Id, Arg (Equal + 1 .. Arg'Last));
                     end if;
               end case;
            end;
         end;
      end Process_Long_Option;
   begin
      while Arg_N <= Arg_Count loop
         declare
            Arg : constant String := Argument (Arg_N);
         begin
            if Arg'Length <= 1 or else Arg (Arg'First) /= '-' then
               --  This is a non-flag argument, abort option processing if
               --    posixly correct.
               if Posixly_Correct then
                  exit;
               else
                  Callback (Top_Level_Argument, Arg);
                  Arg_N := Arg_N + 1;
               end if;
            elsif Arg (Arg'First + 1) = '-' then
               --  Argument starting with "--": long option.
               if Arg'Length = 2 then
                  Arg_N := Arg_N + 1;
                  exit;
               end if;
               Process_Long_Option (Arg (Arg'First + 2 .. Arg'Last));
               Arg_N := Arg_N + 1;
            elsif Long_Only then
               --  Force long option on a single dash prefix.
               Process_Long_Option (Arg (Arg'First + 1 .. Arg'Last));
               Arg_N := Arg_N + 1;
            else
               --  Process a list of short options, until one with required
               --    argument is encountered (and the rest is its argument).
               for Arg_I in Arg'First + 1 .. Arg'Last loop
                  declare
                     Cursor : constant Short_Option_Maps.Cursor
                       := Options.By_Short_Name.Find (Arg (Arg_I));
                  begin
                     if Short_Option_Maps.Has_Element (Cursor) then
                        declare
                           Opt : constant Option
                             := Short_Option_Maps.Element (Cursor);
                        begin
                           if Opt.Has_Arg = Required_Argument then
                              if Arg_I = Arg'Last then
                                 if Arg_N = Arg_Count then
                                    if Missing_Argument = null then
                                       raise Option_Error with "Missing "
                                         & "argument to option "
                                         & Opt.Short_Name;
                                    else
                                       Missing_Argument (Opt.Id);
                                    end if;
                                 else
                                    Callback (Opt.Id, Argument (Arg_N + 1));
                                    Arg_N := Arg_N + 1;
                                    exit;
                                 end if;
                              else
                                 Callback (Opt.Id,
                                           Arg (Arg_I + 1 .. Arg'Last));
                                 exit;
                              end if;
                           else
                              Callback (Opt.Id, "");
                           end if;
                        end;
                     else
                        if Unknown_Short_Option = null then
                           raise Option_Error with "Unknown short option "
                                                 & Arg (Arg_I);
                        else
                           Unknown_Short_Option (Arg (Arg_I));
                        end if;
                     end if;
                  end;
               end loop;
               Arg_N := Arg_N + 1;
            end if;
         end;
      end loop;

      --  Only non-flag arguments remain
      while Arg_N <= Arg_Count loop
         Callback (Top_Level_Argument, Argument (Arg_N));
         Arg_N := Arg_N + 1;
      end loop;
   end Process;

end Natools.Getopt_Long;
