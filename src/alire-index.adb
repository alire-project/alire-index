package body Alire.Index is

   --------------
   -- Register --
   --------------

   function Register (Project        : Project_Name;
                      Version        : Semantic_Versioning.Version;
                      Description    : Project_Description;
                      Origin         : Origins.Origin;
                      Depends_On     : Dependencies            := No_Dependencies;
                      Properties     : Alire.Properties.Vector := No_Properties;
                      Requisites     : Alire.Requisites.Tree   := No_Requisites;
                      Available_When : Alire.Requisites.Tree   := No_Requisites) return Release
   is
   begin
      return Rel : constant Alire.Releases.Release :=
        Alire.Releases.New_Release (Project,
                                    Description,
                                    Version,
                                    Origin,
                                    Depends_On,
                                    Properties => Properties,
                                    Requisites => Requisites,
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
