with Ada.Containers.Ordered_Maps;
with Ada.Strings.Maps;

with Gnat.OS_Lib;

package body Alire.Index is

   use all type Version;

   package Name_Entry_Maps is new Ada.Containers.Ordered_Maps (Projects.Names,
                                                               Catalog_Entry);

   Master_Entries : Name_Entry_Maps.Map;

   ------------------------
   -- Catalogued_Project --
   ------------------------

   function Catalogued_Project return Catalog_Entry is (Name, Parent);

   -------------
   -- Current --
   -------------

   function Current (C : Catalog_Entry) return Release is
   begin
      for R of reverse Catalog loop
         if R.Name = C.Name then
            return R;
         end if;
      end loop;

      raise Program_Error with "Catalog entry without releases: " & Image (C.Name);
   end Current;

   ---------
   -- Get --
   ---------

   function Get (Name : Projects.Names) return Catalog_Entry is
     (Master_Entries.Element (Name));

   --------------------------
   -- Is_Currently_Indexed --
   --------------------------

   function Is_Currently_Indexed (Name : Projects.Names) return Boolean is
      (Master_Entries.Contains (Name));

   ------------
   -- Exists --
   ------------

   function Exists (Project : Name_String) return Boolean is
   begin
      return Names'Value (Project) = Projects.Alire or else True;
   exception
      when others =>
         return False;
   end Exists;

   ------------
   -- Exists --
   ------------

   function Exists (Project : Name_String;
                    Version : Semantic_Versioning.Version)
                    return Boolean is
   begin
      for R of Catalog loop
         if R.Variant = Project and then R.Version = Version then
            return True;
         end if;
      end loop;

      return False;
   end Exists;

   ----------
   -- Find --
   ----------

   function Find (Project : Name_String;
                  Version : Semantic_Versioning.Version) return Release is
   begin
      for R of Catalog loop
         if R.Variant = Project and then R.Version = Version then
            return R;
         end if;
      end loop;

      raise Constraint_Error with "Not in index: " & Project & "=" & Semantic_Versioning.Image (Version);
   end Find;

   --------------
   -- Register --
   --------------

   function Register (C : Catalog_Entry; R : Release) return Release is
   begin
      Master_Entries.Include (R.Name, C);
      --  Only once would be optimal, but we cannot do that any other way I can think of

      if Catalog.Contains (R) then
         Trace.Error ("Attempt to register duplicate versions: " & R.Milestone.Image);
      else
         Catalog.Insert (R);
      end if;

      return R;
   end Register;

   --------------
   -- Register --
   --------------

   function Register (--  Mandatory
                      Project            : Catalog_Entry;
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
      return Register
        (Project,
         Alire.Releases.New_Release
           (Project.Name,
            Version,
            Origin,
            Notes,
            Dependencies,
            Properties         => Properties,
            Private_Properties => Private_Properties,
            Available          => Available_When));
   end Register;

   --------------
   -- Register --
   --------------

   function Register (--  Mandatory
                      Project            : Catalog_Entry;
                      -- we force naming beyond this point with this ugly guard:
                      XXXXXXXXXXXXXX     : Utils.XXX_XXX         := Utils.XXX_XXX_XXX;
                      Parent             : Release;
                      Variant            : Name_String;
                      Notes              : Description_String; -- Mandatory for subrelease
                      Dependencies       : Release_Dependencies  := No_Dependencies;
                      Properties         : Release_Properties    := No_Properties;
                      Private_Properties : Release_Properties    := No_Properties;
                      Available_When     : Release_Requisites    := No_Requisites)
                      return Release
   is
      pragma Unreferenced (XXXXXXXXXXXXXX);
   begin
      return Register (Project,
                       Parent.New_Child (Variant            => Parent.Variant & ":" & Variant,
                                         Notes              => Notes,
                                         Dependencies       => Dependencies,
                                         Properties         => Properties,
                                         Private_Properties => Private_Properties,
                                         Available          => Available_When));
   end Register;

   ------------
   -- Bypass --
   ------------

   function Bypass (--  Mandatory
                    Project            : Catalog_Entry;
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
        Alire.Releases.New_Release (Project.Name,
                                    Version,
                                    Origin,
                                    Notes,
                                    Dependencies,
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

   -----------
   -- Value --
   -----------

   function Value (Project : Name_String) return Names is (Names'Value (Project));

end Alire.Index;
