package Alire.Repositories.Apt is

   type Repository (<>) is new Repositories.Repository with private;

   Repo : constant Repository;

   overriding function Image (Repo : Repository) return String;

   overriding procedure Checkout (R : Repository; Id : Release_Id; Folder : String);

private

   type Repository is new Repositories.Repository with null record;

   Repo : constant Repository := (Repositories.Repository with null record);

   function Image (Repo : Repository) return String is ("aptlocal");

end Alire.Repositories.Apt;
