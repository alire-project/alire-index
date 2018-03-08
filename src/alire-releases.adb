with Alire.Conditional_Values;
with Alire.Platforms;
with Alire.Requisites.Booleans;

with GNAT.IO; -- To keep preelaborable

with Table_IO;

package body Alire.Releases is

   use all type Properties.Labeled.Labels;

   function All_Properties (R : Release) return Conditional.Properties is
      (R.Properties and R.Priv_Props);

   ----------------------------
   -- On_Platform_Properties --
   ----------------------------

   function On_Platform_Properties (R : Release; P : Properties.Vector) return Properties.Vector is
      (R.Properties.Evaluate (P) and R.Priv_Props.Evaluate (P));

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

   function Executables (R : Release;
                         P : Properties.Vector)
                         return Utils.String_Vector
   is
   begin
      return Exes : Utils.String_Vector := Values (R.All_Properties.Evaluate (P), Executable) do
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

   function GPR_Files (R : Release;
                       P : Properties.Vector)
                       return Utils.String_Vector is
   begin
      return Files : Utils.String_Vector := Values (R.All_Properties.Evaluate (P), GPR_File) do
         if Files.Is_Empty then
            Files.Append (R.Project & ".gpr");
         end if;
      end return;
   end GPR_Files;

   -------------------
   -- Project_Files --
   -------------------

   function Project_Files (R : Release;
                           P : Properties.Vector)
                           return Utils.String_Vector is
   begin
      return Files : Utils.String_Vector := Values (R.All_Properties.Evaluate (P), Project_File) do
         if Files.Is_Empty then
            Files.Append (R.Project & ".gpr");
         end if;
      end return;
   end Project_Files;

   ------------------------
   -- Labeled_Properties --
   ------------------------

   function Labeled_Properties (R     : Release;
                                P     : Properties.Vector;
                                Label : Properties.Labeled.Labels)
                                return Utils.String_Vector
   is
   begin
      return Values (R.All_Properties.Evaluate (P), Label);
   end Labeled_Properties;

   -----------------------
   -- Print_Conditional --
   -----------------------

   generic
      with package Cond is new Conditional_Values (<>);
      with procedure Print (Prefix : String; V : Cond.Values);
   procedure Print_Conditional (Prefix : String; This : Cond.Conditional_Value);

   procedure Print_Conditional (Prefix : String; This : Cond.Conditional_Value) is
      use GNAT.IO;

      procedure Visit (This : Cond.Conditional_Value) is
      begin
         case This.Kind is
            when Cond.Value =>
               Print (Prefix, This.Value);
            when Cond.Condition =>
               if This.True_Value.Is_Empty then
                  Put_Line (Prefix & "when not (" & This.Condition.Image & "):");
                  Print_Conditional (Prefix & "   ", This.False_Value);
               else
                  Put_Line (Prefix & "when " & This.Condition.Image & ":");
                  Print_Conditional (Prefix & "   ", This.True_Value);
                  if not This.False_Value.Is_Empty then
                     Put_Line (Prefix & "else:");
                     Print_Conditional (Prefix & "   ", This.False_Value);
                  end if;
               end if;
            when Cond.Vector =>
               raise Program_Error with "Shouldn't happen";
         end case;
      end Visit;

   begin
      This.Iterate_Children (Visit'Access);
   end Print_Conditional;

   -----------
   -- Print --
   -----------

   procedure Print (R : Release; Private_Too : Boolean := False) is
      use GNAT.IO;

      procedure Print_Propvec (Prefix : String; V : Properties.Vector) is
      begin
         Properties.Print (V, Prefix);
      end Print_Propvec;

      procedure Print_Depvec (Prefix : String; V : Dependencies.Vectors.Vector) is
      begin
         for Dep of V loop
            Put_Line (Prefix & Dep.Image);
         end loop;
      end Print_Depvec;

      procedure Print_Properties   is new Print_Conditional (Conditional.For_Properties,   Print_Propvec);
      procedure Print_Dependencies is new Print_Conditional (Conditional.For_Dependencies, Print_Depvec);
   begin
      --  MILESTONE
      Put_Line (R.Milestone.Image & ": " & R.Description);

      --  ORIGIN
      if R.Origin.Is_Native then
         Put_Line ("Origin (native package):");
         declare
            Table : Table_IO.Table;
         begin
            for Dist in Platforms.Distributions loop
               if R.Origin.Package_Name (Dist) /= Origins.Unavailable.Image then
                  Table.New_Row;
                  Table.Append ("   ");
                  Table.Append (Utils.To_Mixed_Case (Dist'Img) & ":");
                  Table.Append (R.Origin.Package_Name (Dist));
               end if;
            end loop;
            Table.Print;
         end;
      else
         Put_Line ("Origin: " & R.Origin.Image);
      end if;

      --  AVAILABILITY
      if not R.Available.Is_Empty then
         Put_Line ("Available when: " & R.Available.Image);
      end if;

      --  PROPERTIES
      if not R.Properties.Is_Empty then
         Put_Line ("Properties:");
         Print_Properties ("   ", R.Properties);
      end if;

      --  PRIVATE PROPERTIES
      if Private_Too and then not R.Properties.Is_Empty then
         Put_Line ("Private properties:");
         Print_Properties ("   ", R.Priv_Props);
      end if;

      --  DEPENDENCIES
      if not R.Dependencies.Is_Empty then
         Put_Line ("Dependencies (direct):");
         Print_Dependencies ("   ", R.Dependencies);
      end if;
   end Print;

   -----------------------
   -- Property_Contains --
   -----------------------

   function Property_Contains (R : Release; Str : String) return Boolean is
      use Utils;

      Search : constant String := To_Lower_Case (Str);
   begin
      for P of R.All_Properties.All_Values loop
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
         R.Priv_Props.Evaluate (P),
         (if R.Available.Check (P)
          then Requisites.Booleans.Always_True
          else Requisites.Booleans.Always_False))
      do
         null;
      end return;
   end Whenever;

end Alire.Releases;
