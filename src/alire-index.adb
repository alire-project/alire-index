with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Strings.Maps;

with Alire.Projects;

with Gnat.OS_Lib;

package body Alire.Index is

   use all type Version;

   package Name_Entry_Maps is new Ada.Containers.Indefinite_Ordered_Maps (Alire.Project,
                                                                          Catalog_Entry);

   Master_Entries : Name_Entry_Maps.Map;

   ------------------------
   -- Catalogued_Project --
   ------------------------

   function Catalogued_Project return Catalog_Entry is
   begin
      return C : constant Catalog_Entry := (Name_Len  => Project'Length,
                                            Descr_Len => Description'Length,
                                            Pack_Len  => Package_Name'Length,
                                            Self_Len  => String'("Project")'Length,

                                            Project      => Project,
                                            Description  => Description,
                                            Package_Name => Package_Name,
                                            Self_Name    => "Project")
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
   begin
      return C : constant Catalog_Entry := (Name_Len => Name'Length + Base.Project'Length + 1,
                                            Descr_Len => Description'Length,
                                            Pack_Len => Base.Package_Name'Length,
                                            Self_Len => Ada_Identifier'Length,

                                            Project      => Base.Project & Extension_Separator & Name,
                                            Description  => Description,
                                            Package_Name => Base.Package_Name,
                                            Self_Name    => Ada_Identifier)
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

   ---------------
   -- To_Native --
   ---------------

   Dir_Seps : constant Ada.Strings.Maps.Character_Set := Ada.Strings.Maps.To_Set ("/\");

   function To_Native (Path : Platform_Independent_Path) return String is
      use Ada.Strings.Maps;
   begin
      return Native : String := Path do
         for I in Native'Range loop
            if Is_In (Path (I), Dir_Seps) then
               Native (I) := GNAT.OS_Lib.Directory_Separator;
            end if;
         end loop;
      end return;
   end To_Native;

end Alire.Index;
