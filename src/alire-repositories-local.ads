package Alire.Repositories.Local is
   
   --  Special repository meant to be used for the current project, which is not checked out
   --  For this reason, its checkout is never useful and raises Program_Error
   
   type Repository (<>) is new Repositories.Repository with private;
   
   Repo : constant Repository;  
   
   overriding function Image (Repo : Repository) return String is 
     (raise Program_Error);
   
   overriding procedure Checkout (R : Repository; Id : Release_Id; Folder : String) is null 
     with Pre'Class => (raise Program_Error);
   
private
   
   type Repository is new Repositories.Repository with null record;
      
   Repo : constant Repository := (Repositories.Repository with null record); 

end Alire.Repositories.Local;
