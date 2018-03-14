with Ada.Containers.Ordered_Maps;
with Ada.Directories;
with Ada.Strings.Maps;

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
         if R.Project = Project and then R.Version = Version then
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
         if R.Project = Project and then R.Version = Version then
            return R;
         end if;
      end loop;

      raise Constraint_Error with "Not in index: " & Project & "=" & Semantic_Versioning.Image (Version);
   end Find;

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
      use all type Alire.Properties.Labeled.Labels;
   begin
      Master_Entries.Include (Project.Name, Project);
      --  Only once would be optimal, but we cannot do that any other way I can think of

      return Rel : constant Alire.Releases.Release :=
        Alire.Releases.New_Release (Project.Name,
                                    Version,
                                    Origin,
                                    Notes,
                                    Dependencies,
                                    Properties         => Properties,
                                    Private_Properties => Private_Properties,
                                    Available          => Available_When)
      do
         if Catalog.Contains (Rel) then
            Trace.Error ("Attempt to register duplicate versions: " & Rel.Milestone.Image);
         else
            Catalog.Insert (Rel);
         end if;
      end return;
   end Register;

   ---------------
   -- To_Native --
   ---------------

   Dir_Seps : constant Ada.Strings.Maps.Character_Set := Ada.Strings.Maps.To_Set ("/\");

   function To_Native (Path : Platform_Independent_Path) return String is
      use Ada.Strings.Maps;
   begin
      for I in Path'Range loop
         if Is_In (Path (I), Dir_Seps) then
            return Ada.Directories.Compose
              (Path (Path'First .. I - 1),
               To_Native (Path (I + 1 .. Path'Last)));
         end if;
      end loop;

      return Path;
   end To_Native;

   -----------
   -- Value --
   -----------

   function Value (Project : Name_String) return Names is (Names'Value (Project));

end Alire.Index;
