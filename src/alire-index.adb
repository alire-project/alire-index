with Ada.Directories;
with Ada.Strings.Maps;

package body Alire.Index is

   use all type Version;

   ------------
   -- Exists --
   ------------

   function Exists (Project : Project_Name;
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

   function Find (Project : Project_Name;
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
                      Project              : Project_Name;
                      Version              : Semantic_Versioning.Version;
                      Description          : Project_Description;
                      Origin               : Origins.Origin;
                      --  Barrier
                      XXXXXXXXXXXXXX       : Utils.XXX_XXX         := Utils.XXX_XXX_XXX;
                      --  Optional
                      Dependencies         : Release_Dependencies  := No_Dependencies;
                      Properties           : Release_Properties    := No_Properties;
                      Private_Properties   : Release_Properties    := No_Properties;
                      Available_When       : Alire.Requisites.Tree := No_Requisites)
                      return Release
   is
      pragma Unreferenced (XXXXXXXXXXXXXX);
      use all type Alire.Properties.Labeled.Labels;
   begin
      return Rel : constant Alire.Releases.Release :=
        Alire.Releases.New_Release (Project,
                                    Description,
                                    Version,
                                    Origin,
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

end Alire.Index;
