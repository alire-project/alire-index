package body Alire.Index is

   function Register (Project     : Project_Name;
                      Version     : Semantic_Versioning.Version;
                      Hosting     : Repositories.Repository'Class;
                      Id          : Repositories.Release_Id;
                      Depends_On  : Dependencies := Nothing;
                      License     : Licenses := Unknown) return Release is
   begin
      return (null record);
   end Register;

end Alire.Index;
