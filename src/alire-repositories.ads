with Ada.Containers.Indefinite_Holders;

package Alire.Repositories with Preelaborate is
   
   subtype Release_Id is String
     with Dynamic_Predicate => Release_Id'Length > 0;
   -- Uniquely identifies a particular release within a repository
   -- E.g., git/hg hashes, a zip file within a file server...
   
   type Repository is interface;
   
   function Image (Repo : Repository) return String is abstract;
   
   function "<" (L, R : Repository'Class) return Boolean is (L.Image < R.Image);
   function "=" (L, R : Repository'Class) return Boolean is (L.Image = R.Image);
   
   procedure Checkout (R : Repository; Id : Release_Id; Folder : String) 
   is abstract
     --  Minimum defence against improper command spawning
     with Pre'Class => (for all C of Folder => C /= ' ') and 
		       (for all C of Id => C /= ' ') and
                       (for all C of R.Image => C /= ' ');
   --  Folder must afterwards contain the checked-out repository     
   
   
   package Repository_Holders is new Ada.Containers.Indefinite_Holders (Repository'Class);
   
   type Repository_H is new Repository_Holders.Holder with null record;

end Alire.Repositories;
