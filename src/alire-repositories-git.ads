package Alire.Repositories.Git is

   subtype Commit_ID is String (1 .. 40);   
   
   type Repository (<>) is new Repositories.Repository with private;
   
   not overriding function New_Repository (URL : String) return Repository;
   
   overriding function Image (Repo : Repository) return String;
   
   overriding procedure Checkout (R : Repository; Id : Release_Id; Folder : String)
     with Pre => Id in Commit_Id;
   
private
   
   type Repository (URL_Length : Natural) is new Repositories.Repository with record
      URL : String (1 .. URL_Length);
   end record;
   
   function New_Repository (URL : String) return Repository
   is (URL_Length => Url'Length,
       URL        => URL);
   
   function Image (Repo : Repository) return String is (Repo.URL);

end Alire.Repositories.Git;
