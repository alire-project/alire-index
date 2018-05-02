with Ada.Containers.Indefinite_Ordered_Maps;

with Alire.Projects;

with GNAT.Source_Info;

package body Alire.Index is

   use all type Version;

   package Name_Entry_Maps is new Ada.Containers.Indefinite_Ordered_Maps (Alire.Project,
                                                                          Catalog_Entry);

   Master_Entries : Name_Entry_Maps.Map;

   type Reflected_Info (Pack_Len, Id_Len : Positive) is record
      Package_Name : String (1 .. Pack_Len);
      Identifier   : String (1 .. Id_Len);
   end record;

   --------------
   -- Identify --
   --------------

   function Identify (Enclosing : String) return Reflected_Info is
      use Utils;
      Identifier : constant String := -- Portion after last dot
                     Split (Enclosing, '.', Side => Tail, From => Tail);
      Full_Name  : constant String := -- Portion after Alire.Index.
                     Split (Enclosing, '.', Side => Tail, From => Head, Count => 2);
      Pack_Name  : constant String := -- Portion between Alire.Index. and .Identifier
                     Split (Full_Name, '.', Side => Head, From => Tail);
   begin
      return (Pack_Name'Length, Identifier'Length, Pack_Name, Identifier);
   end Identify;

   ------------------------
   -- Catalogued_Project --
   ------------------------

   function Catalogued_Project return Catalog_Entry is
      use Utils;
      Enclosing : constant String := GNAT.Source_Info.Enclosing_Entity;
      Self_Name : constant String := Split (Enclosing, '.', Side => Tail, From => Tail);
      Full_Name : constant String := Split (Enclosing, '.', Side => Tail, From => Head, Count => 2);
      Pack_Name : constant String := Split (Full_Name, '.', Side => Head, From => Tail);
   begin
      return C : constant Catalog_Entry := (Name_Len  => Pack_Name'Length,
                                            Descr_Len => Description'Length,
                                            Pack_Len  => Pack_Name'Length,
                                            Self_Len  => Self_Name'Length,

                                            Project      => +To_Lower_Case (Pack_Name),
                                            Description  => Description,
                                            Package_Name => Pack_Name,
                                            Self_Name    => Self_Name)
      do
         if First_Use.all then
            First_Use.all := False;

            Master_Entries.Insert (C.Project, C);
            Projects.Descriptions.Insert (C.Project, Description);
         end if;
      end return;
   end Catalogued_Project;

   ---------------
   -- Extension --
   ---------------

   function Extension return Catalog_Entry is
      use Utils;
      Enclosing : constant String := GNAT.Source_Info.Enclosing_Entity;
      Self_Name : constant String := Split (Enclosing, '.', Side => Tail, From => Tail);
      Full_Name : constant String := Split (Enclosing, '.', Side => Tail, From => Head, Count => 2);
      Pack_Name : constant String := Split (Full_Name, '.', Side => Head, From => Tail);
   begin
--        Trace.Always ("Encl: " & GNAT.Source_Info.Enclosing_Entity);
--        Trace.Always ("self: " & Self_Name);
--        Trace.Always ("full: " & Full_Name);
--        Trace.Always ("pack: " & Pack_Name);
      return C : constant Catalog_Entry := (Name_Len  => Self_Name'Length + Base.Project'Length + 1,
                                            Descr_Len => Description'Length,
                                            Pack_Len  => Pack_Name'Length,
                                            Self_Len  => Self_Name'Length,

                                            Project      =>
                                              +To_Lower_Case ((+Base.Project) & Extension_Separator & Self_Name),
                                            Description  => Description,
                                            Package_Name => Pack_Name,
                                            Self_Name    => Self_Name)
      do
         if First_Use.all then
            First_Use.all := False;

            Master_Entries.Insert (C.Project, C);
            Projects.Descriptions.Insert (C.Project, Description);
         end if;
      end return;
   end Extension;

   -------------
   -- Current --
   -------------

   function Current (C : Catalog_Entry) return Release is
   begin
      for R of reverse Catalog loop
         if R.Project = C.Project then
            return R;
         end if;
      end loop;

      raise Program_Error with "Catalog entry without releases: " & (+C.Project);
   end Current;

   ---------
   -- Get --
   ---------

   function Get (Name : Alire.Project) return Catalog_Entry is
     (Master_Entries.Element (Name));

   --------------------------
   -- Is_Currently_Indexed --
   --------------------------

   function Is_Currently_Indexed (Name : Alire.Project) return Boolean is
      (Master_Entries.Contains (Name));

   ------------
   -- Exists --
   ------------

   function Exists (Project : Alire.Project;
                    Version : Semantic_Versioning.Version)
                    return Boolean is
   begin
      for R of Catalog loop
         if R.Project = Project and then R.Version = Version then
            return True;
         end if;
      end loop;

      return False;
   end Exists;

   ----------
   -- Find --
   ----------

   function Find (Project : Alire.Project;
                  Version : Semantic_Versioning.Version) return Release is
   begin
      for R of Catalog loop
         if R.Project = Project and then R.Version = Version then
            return R;
         end if;
      end loop;

      raise Constraint_Error with "Not in index: " & (+Project) & "=" & Semantic_Versioning.Image (Version);
   end Find;

   -------------------
   -- Register_Real --
   -------------------

   function Register_Real (R : Release) return Release is
   begin
      if Catalog.Contains (R) then
         Trace.Error ("Attempt to register duplicate versions: " & R.Milestone.Image);
      else
         Catalog.Insert (R);
      end if;

      return R;
   end Register_Real;

   --------------
   -- Register --
   --------------

   function Register (--  Mandatory
                      This               : Catalog_Entry;
                      Version            : Semantic_Versioning.Version;
                      Origin             : Origins.Origin;
                      -- we force naming beyond this point with this ugly guard:
                      XXXXXXXXXXXXXX     : Utils.XXX_XXX         := Utils.XXX_XXX_XXX;
                      --  Optional
                      Notes              : Description_String    := "";
                      Dependencies       : Release_Dependencies  := No_Dependencies;
                      Properties         : Release_Properties    := No_Properties;
                      Private_Properties : Release_Properties    := No_Properties;
                      Available_When     : Release_Requisites    := No_Requisites)
                      return Release
   is
      pragma Unreferenced (XXXXXXXXXXXXXX);
   begin
      return Register_Real
        (Alire.Releases.New_Release
           (Project            => This.Project,
            Version            => Version,
            Origin             => Origin,
            Notes              => Notes,
            Dependencies       => Dependencies,
            Properties         => Properties,
            Private_Properties => Private_Properties,
            Available          => Available_When));
   end Register;

   --------------
   -- Register --
   --------------

   function Register (Extension          : Catalog_Entry;
                      Extended_Release   : Release)
                      return Release
   is
   begin
      return Register_Real (Extended_Release.Replacing
                            (Project => Extension.Project));
   end Register;

   function Base_Release return Release is (raise Program_Error);

   function Derived_Release return Release is (raise Program_Error);

   ------------
   -- Bypass --
   ------------

   function Bypass (--  Mandatory
                    This               : Catalog_Entry;
                    Version            : Semantic_Versioning.Version;
                    Origin             : Origins.Origin;
                    -- we force naming beyond this point with this ugly guard:
                    XXXXXXXXXXXXXX     : Utils.XXX_XXX         := Utils.XXX_XXX_XXX;
                    --  Optional
                    Notes              : Description_String    := "";
                    Dependencies       : Release_Dependencies  := No_Dependencies;
                    Properties         : Release_Properties    := No_Properties;
                    Private_Properties : Release_Properties    := No_Properties;
                    Available_When     : Release_Requisites    := No_Requisites)
                    return Release
   is
      pragma Unreferenced (XXXXXXXXXXXXXX);
   begin
      return
        Alire.Releases.New_Release (Project            => This.Project,
                                    Version            => Version,
                                    Origin             => Origin,
                                    Notes              => Notes,
                                    Dependencies       => Dependencies,
                                    Properties         => Properties,
                                    Private_Properties => Private_Properties,
                                    Available          => Available_When);
   end Bypass;

end Alire.Index;
