package body Alire.Index is

   ------------
   -- Exists --
   ------------

   function Exists (Project : Project_Name;
                    Version : Semantic_Versioning.Version)
                    return Boolean is
   begin
      for R of Releases loop
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
      for R of Releases loop
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
                      Project        : Project_Name;
                      Version        : Semantic_Versioning.Version;
                      Description    : Project_Description;
                      Origin         : Origins.Origin;
                      --  Optional
                      Depends_On     : Release_Dependencies  := No_Dependencies;
                      Properties     : Release_Properties    := No_Properties;
                      Available_When : Alire.Requisites.Tree := No_Requisites)
                      return Release
   is
   begin
      return Rel : constant Alire.Releases.Release :=
        Alire.Releases.New_Release (Project,
                                    Description,
                                    Version,
                                    Origin,
                                    Depends_On,
                                    Properties => Properties,
                                    Available  => Available_When)
      do
         if Releases.Contains (Rel) then
            Log ("Attempt to register duplicate versions: " & Rel.Milestone.Image, Warning);
         else
            Releases.Insert (Rel);
         end if;
      end return;
   end Register;

end Alire.Index;
