with Alire.Conditional_Values;
with Alire.Dependencies.Vectors;
with Alire.Platform;
with Alire.Platforms;
with Alire.Projects;
with Alire.Requisites.Booleans;

with GNAT.IO; -- To keep preelaborable

with Table_IO;

package body Alire.Releases is

   use all type Properties.Labeled.Labels;

   --------------------
   -- All_Properties --
   --------------------

   function All_Properties (R : Release) return Conditional.Properties is
      (R.Properties and R.Priv_Props);


   ---------------
   -- Extending --
   ---------------

   function Extending (Base               : Release;
                       Dependencies       : Conditional.Dependencies := Conditional.For_Dependencies.Empty;
                       Properties         : Conditional.Properties   := Conditional.For_Properties.Empty;
                       Private_Properties : Conditional.Properties   := Conditional.For_Properties.Empty;
                       Available          : Alire.Requisites.Tree    := Requisites.Trees.Empty_Tree)
                       return Release
   is
      use all type Conditional.Dependencies;
      use all type Requisites.Tree;
   begin
      return Extended : Release := Base do
         Extended.Dependencies := Base.Dependencies and Dependencies;
         Extended.Properties   := Base.Properties   and Properties;
         Extended.Priv_Props   := Base.Priv_Props   and Private_Properties;
         Extended.Available    := Base.Available    and Available;
      end return;
   end Extending;

   function Replacing (Base   : Release;
                       Origin : Origins.Origin) return Release is
   begin
      return Replaced : Release := Base do
         Replaced.Origin := Origin;
      end return;
   end Replacing;

   ---------------
   -- Replacing --
   ---------------

   function Replacing (Base               : Release;
                       Project            : Alire.Project      := "";
                       Notes              : Description_String := "") return Release
   is
      New_Project : constant Alire.Project := (if Project = ""
                                               then Base.Project
                                               else Project);
      New_Notes   : constant Description_String := (if Notes = ""
                                                    then Base.Notes
                                                    else Notes);
   begin

      return Replacement : constant Release (New_Project'Length, New_Notes'Length) :=
        (Prj_Len   => New_Project'Length,
         Notes_Len => New_Notes'Length,
         Project   => New_Project,
         Notes     => New_Notes,

         Version      => Base.Version,
         Origin       => Base.Origin,
         Dependencies => Base.Dependencies,
         Properties   => Base.Properties,
         Priv_Props   => Base.Priv_Props,
         Available    => Base.Available)
      do
         null;
      end return;
   end Replacing;

   ---------------
   -- Upgrading --
   ---------------

   function Upgrading (Base    : Release;
                       Version : Semantic_Versioning.Version;
                       Origin  : Origins.Origin) return Release is
   begin
      return Upgraded : Release := Base do
         Upgraded.Version := Version;
         Upgraded.Origin  := Origin;
      end return;
   end Upgrading;

   -----------------
   -- New_Release --
   -----------------

   function New_Release (Project            : Alire.Project;
                         Version            : Semantic_Versioning.Version;
                         Origin             : Origins.Origin;
                         Notes              : Description_String;
                         Dependencies       : Conditional.Dependencies;
                         Properties         : Conditional.Properties;
                         Private_Properties : Conditional.Properties;
                         Available          : Alire.Requisites.Tree) return Release is
     (Prj_Len      => Project'Length,
      Notes_Len    => Notes'Length,
      Project      => Project,
      Version      => Version,
      Origin       => Origin,
      Notes        => Notes,
      Dependencies => Dependencies,
      Properties   => Properties,
      Priv_Props   => Private_Properties,
      Available    => Available);

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

   -------------------
   -- Project_Files --
   -------------------

   function Project_Files (R         : Release;
                           P         : Properties.Vector;
                           With_Path : Boolean)
                           return Utils.String_Vector
   is
      use Utils;

      With_Paths : Utils.String_Vector := Values (R.All_Properties.Evaluate (P), Project_File);
      Without    : Utils.String_Vector;
   begin
      if With_Paths.Is_Empty then
         With_Paths.Append (String'((+R.Project) & ".gpr"));
      end if;

      if With_Path then
         return With_Paths;
      else
         for File of With_Paths loop
            --  Has path or not
            if Tail (File, '/') = "" then
               Without.Append (File); -- As is
            else
               Without.Append (Tail (File, '/'));
            end if;
         end loop;

         return Without;
      end if;
   end Project_Files;

   -------------------
   -- Project_Paths --
   -------------------

   function Project_Paths (R         : Release;
                           P         : Properties.Vector) return Utils.String_Set
   is
      use Utils;
      Files : constant String_Vector := Project_Files (R, P, With_Path => True);
   begin
      return Paths : String_Set do
         for File of Files loop
            if Contains (File, "/") then
               Paths.Include (Head (File, '/'));
            end if;
         end loop;
      end return;
   end Project_Paths;

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
      Put_Line (R.Milestone.Image & ": " & Projects.Descriptions (R.Project));

      if R.Notes /= "" then
         Put_Line ("Notes: " & R.Notes);
      end if;

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

   -------------
   -- Version --
   -------------

   function Version (R : Release) return Semantic_Versioning.Version is
   begin
      if R.Origin.Is_Native then
         declare
            Native_Version : constant String := Platform.Current.Package_Version (R.Origin);
         begin
            if Native_Version /= "" then
               return New_Version (Image (R.Version) & "+" & Native_Version);
            else
               return R.Version;
            end if;
         end;
      else
         return R.Version;
      end if;
   end Version;

   --------------
   -- Whenever --
   --------------

   function Whenever (R : Release; P : Properties.Vector) return Release is
   begin
      return Solid : constant Release (R.Prj_Len, R.Notes_Len) :=
        (Prj_Len      => R.Prj_Len,
         Notes_Len    => R.Notes_Len,
         Project      => R.Project,
         Version      => R.Version,
         Origin       => R.Origin,
         Notes        => R.Notes,
         Dependencies => R.Dependencies.Evaluate (P),
         Properties   => R.Properties.Evaluate (P),
         Priv_Props   => R.Priv_Props.Evaluate (P),
         Available    => (if R.Available.Check (P)
                          then Requisites.Booleans.Always_True
                          else Requisites.Booleans.Always_False))
      do
         null;
      end return;
   end Whenever;

end Alire.Releases;
