package body Alire.Index is

   --------------
   -- Register --
   --------------

   function Register (Project     : Project_Name;
                      Version     : Semantic_Versioning.Version;
                      Hosting     : Repositories.Repository'Class;
                      Id          : Repositories.Release_Id;
                      Depends_On  : Dependencies := Nothing;
                      License     : Licenses := Unknown) return Release
   is
      pragma Unreferenced (License);
   begin
      Log ("Registering " & Project & ": " & Semantic_Versioning.Image (Version));

     -- Milestones.Include (New_Milestone (Project, Version));

      return Rel : constant Alire.Releases.Release :=
        Alire.Releases.New_Release (Project,
                                    Version,
                                    Hosting,
                                    Id,
                                    Depends_On)
      do
         Releases.Insert (Rel);
      end return;
   end Register;

end Alire.Index;
