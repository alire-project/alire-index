with GNAT.IO; -- To keep preelaborable

package body Alire.Releases is

   use all type Properties.Labeled.Labels;

   ------------
   -- Values --
   ------------

   function Values (Props : Properties.Vector; Label : Properties.Labeled.Labels) return Utils.String_Vector is
   --  Extract values of a particular label
   begin
      return Strs : Utils.String_Vector do
         for P of Props loop
            if P in Properties.Labeled.Label'Class then
               declare
                  LP : Properties.Labeled.Label renames Properties.Labeled.Label (P);
               begin
                  if LP.Name = Label then
                     Strs.Append (LP.Value);
                  end if;
               end;
            end if;
         end loop;
      end return;
   end Values;

   -----------------
   -- Executables --
   ----------------

   function Executables (R : Release) return Utils.String_Vector is
   begin
      return Exes : Utils.String_Vector := Values (R.Properties.All_Values, Executable) do
         if OS_Lib.Exe_Suffix /= "" then
            for I in Exes.Iterate loop
               Exes (I) := Exes (I) & OS_Lib.Exe_Suffix;
            end loop;
         end if;
      end return;
   end Executables;

   ---------------
   -- GPR_Files --
   ---------------

   function GPR_Files (R : Release) return Utils.String_Vector is
   begin
      return Files : Utils.String_Vector := Values (R.Properties.All_Values, GPR_File) do
         if Files.Is_Empty then
            Files.Append (R.Project & ".gpr");
         end if;
      end return;
   end GPR_Files;

   --------------------------------
   -- Print_Conditional_Property --
   --------------------------------

   procedure Print_Conditional_Property (Cond : Conditions.For_Properties.Conditional_Value) is
      use GNAT.IO;
   begin
      if Cond.Is_Unconditional then
         Cond.True_Value.Print (Prefix => "   ");
      else
         if Cond.True_Value.Is_Empty then
            Put_Line ("   when not (" & Cond.Condition.Image & "):");
            Cond.False_Value.Print (Prefix => "      ");
         else
            Put_Line ("   when " & Cond.Condition.Image & ":");
            Cond.True_Value.Print (Prefix => "      ");
            if not Cond.False_Value.Is_Empty then
               Put_Line ("   else:");
               Cond.False_Value.Print (Prefix => "      ");
            end if;
         end if;
      end if;
   end Print_Conditional_Property;

   -----------
   -- Print --
   -----------

   procedure Print (R : Release) is
      use GNAT.IO;
   begin
      --  MILESTONE
      Put_Line (R.Milestone.Image & ": " & R.Description);

      --  ORIGIN
      Put_Line ("Origin: " & R.Origin.Image);

      --  AVAILABILITY
      if not R.Available.Is_Empty then
         Put_Line ("Available when: " & R.Available.Image);
      end if;

      --  PROPERTIES
      if not R.Properties.Is_Empty then
         Put_Line ("Properties:");
         for Cond of R.Properties loop
            Print_Conditional_Property (Cond);
         end loop;
      end if;

      --  DEPENDENCIES
      if not R.Depends.Is_Empty then
         Put_Line ("Dependencies (direct):");
         for Dep of R.Depends loop
            Put_Line ("   " & Dep.Image);
         end loop;
      end if;
   end Print;

   -----------------------
   -- Property_Contains --
   -----------------------

   function Property_Contains (R : Release; Str : String) return Boolean is
      use Utils;

      Search : constant String := To_Lower_Case (Str);
   begin
      for P of R.Properties.All_Values loop
         declare
            Text : constant String :=
                     To_Lower_Case
                       ((if Utils.Contains (P.Image, ":")
                        then Utils.Tail (P.Image, ':')
                        else P.Image));
         begin
            if Utils.Contains (Text, Search) then
               return True;
            end if;
         end;
      end loop;

      return False;
   end Property_Contains;

   --------------
   -- Whenever --
   --------------

   function Whenever (R : Release; P : Properties.Vector) return Release is
   begin
      return Solid : constant Release (R.Name_Len, R.Descr_Len) :=
        (R.Name_Len, R.Descr_Len,
         R.Name,
         R.Description,
         R.Version,
         R.Origin,
         R.Dependencies.Evaluate (P),
         R.Properties.Evaluate (P),
         R.Available)
      do
         null;
      end return;
   end Whenever;

end Alire.Releases;
