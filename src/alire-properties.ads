private with Ada.Containers.Indefinite_Holders;

package Alire.Properties with Preelaborate is

   --  Properties are the general mechanism used to store all info about a release.
   --  They can be specialized (e.g. in version, platform, compiler) but that can be transparent to the user.

   type Property is tagged null record;



   type Checker is tagged private;
   --  A checker verifies against some internally stored data that a property is satisfied.
   --  Here we provide the basic storage of values but the actual checking function must be overridden
   --    for particular checks.

   function New_Check (Using : Property'Class) return Checker'Class;

   function Value (Check : Checker'Class) return Property'Class;

   function Check (This : Checker'Class; Prop : Property'Class) return Boolean;
   --  This classwide helper will match the stored property tag against the one given.
   --  If the match the actual checking function is used, otherwise False is returned

private

   package Property_Holders is new Ada.Containers.Indefinite_Holders (Property'Class);

   type Checker is tagged record
      Property : Property_Holders.Holder;
   end record;

end Alire.Properties;
