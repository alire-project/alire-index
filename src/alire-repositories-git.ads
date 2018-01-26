package Alire.Repositories.Git with Preelaborate is

   type Repository (<>) is new Repositories.Repository with private;
   
   subtype Commit_ID is String (1 .. 40);
   
   function New_Git_Repo (URL : String) return Repository;   
   
private
   
   type Repository (URL_Length : Natural) is new Repositories.Repository with record
      URL : String (1 .. URL_Length);
   end record;

end Alire.Repositories.Git;
