package body Alire.Index is

   --------------
   -- Register --
   --------------

   function Register (Project     : Project_Name;
                      Version     : Semantic_Versioning.Version;
                      Hosting     : Repositories.Repository'Class;
                      Id          : Repositories.Release_Id;
                      Depends_On  : Dependencies := Depends.Nothing;
                      Properties  : Alire.Properties.Vector := Alire.Properties.Vectors.Empty_Vector;
                      Requisites  : Alire.Requisites.Tree   := Alire.Requisites.No_Requisites;
                      Native      : Boolean                 := False) return Release
   is
   begin
      if not Requisites.Is_Empty then
         Alire.Requisites.Trees.Print_Skeleton (Requisites);
      end if;

      return Rel : constant Alire.Releases.Release :=
        Alire.Releases.New_Release (Project,
                                    Version,
                                    Hosting,
                                    Id,
                                    Depends_On,
                                    Properties => Properties,
                                    Requisites => Requisites,
                                    Native     => Native)
      do
         if Releases.Contains (Rel) then
            Log ("Attempt to register duplicate versions: " & Rel.Milestone_Image, Warning);
         else
            Releases.Insert (Rel);
         end if;
      end return;
   end Register;

end Alire.Index;
