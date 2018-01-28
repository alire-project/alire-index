package Alire.Repositories.Git with Preelaborate is

   type Repository (<>) is new Repositories.Repository with private;
   
   function New_Repository(URL : String) return Repository;
   
   subtype Commit_ID is String (1 .. 40);   
   
private
   
   type Repository (URL_Length : Natural) is new Repositories.Repository with record
      URL : String (1 .. URL_Length);
   end record;
   
   function New_Repository (URL : String) return Repository
   is (URL_Length => Url'Length,
       URL        => URL);

end Alire.Repositories.Git;
