with Alire.Properties.Platform;

package body Alire.Index is

   Platform_Properties : constant Properties.Vector :=
                           Properties.Platform.Current;

   --------------
   -- Register --
   --------------

   function Register (Project     : Project_Name;
                      Version     : Semantic_Versioning.Version;
                      Hosting     : Repositories.Repository'Class;
                      Id          : Repositories.Release_Id;
                      Depends_On  : Dependencies := Depends.Nothing;
                      Properties     : Alire.Properties.Vector := Alire.Properties.Vectors.Empty_Vector;
                      Requisites     : Alire.Requisites.Tree   := Alire.Requisites.No_Requisites;
                      Available_When : Alire.Requisites.Tree   := Alire.Requisites.No_Requisites;
                      Native      : Boolean                 := False) return Release
   is
   begin
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
         if not Available_When.Is_Empty and Then not Available_When.Check (Platform_Properties)
         then
            Trace.Debug ("Release " & Rel.Milestone_Image & " requisites not met by platform");
            return;
         end if;

         if Releases.Contains (Rel) then
            Log ("Attempt to register duplicate versions: " & Rel.Milestone_Image, Warning);
         else
            Releases.Insert (Rel);
         end if;
      end return;
   end Register;

end Alire.Index;
