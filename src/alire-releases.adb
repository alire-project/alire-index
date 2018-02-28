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
      return Exes : Utils.String_Vector := Values (R.Props, Executable) do
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
      return Files : Utils.String_Vector := Values (R.Props, GPR_File) do
         if Files.Is_Empty then
            Files.Append (R.Project & ".gpr");
         end if;
      end return;
   end GPR_Files;

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

      --  REQUISITES
      if not R.Reqs.Is_Empty then
         Put ("Requisites: ");
         R.Reqs.Print_Skeleton;
      end if;

      --  PROPERTIES
      if not R.Props.Is_Empty then
         Put_Line ("Properties:");
         for Prop of R.Props loop
            Put_Line ("   " & Prop.Image);
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
      for P of R.Props loop
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

end Alire.Releases;
