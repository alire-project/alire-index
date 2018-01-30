with Ada.Containers.Indefinite_Holders;

package Alire.Repositories with Preelaborate is
       
   type Repository is interface;
   
   function Image (Repo : Repository) return String is abstract;
   
   function "<" (L, R : Repository'Class) return Boolean is (L.Image < R.Image);
   function "=" (L, R : Repository'Class) return Boolean is (L.Image = R.Image);
   
   type Release_Id is new String;   
   
   
   package Repository_Holders is new Ada.Containers.Indefinite_Holders (Repository'Class);
   
   type Repository_H is new Repository_Holders.Holder with null record;

end Alire.Repositories;
