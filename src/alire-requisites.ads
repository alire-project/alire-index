with Ada.Containers.Indefinite_Multiway_Trees;

with Alire.Properties;

package Alire.Requisites with Preelaborate is

   use Properties;

   type Requisite is tagged null record;
   --  A Requisite verifies against some internally stored data that a property is satisfied.
   --  Here we provide the basic storage of values but the actual checking function must be overridden
   --    for particular checks.

   function Is_Applicable (R : Requisite; P : Property'Class) return Boolean is (False);
   --  Initially there is no compatibility. See helper package below

   package Requisite_Trees is new Ada.Containers.Indefinite_Multiway_Trees (Requisite'Class);

   subtype Tree is Requisite_Trees.Tree;

   --  The following package is the building block to be used to define new compatibility checks.
   --  Here we tie a class of properties and requisites (e.g., versions and version sets) that make sense.
   --  A release has a list of properties, and a tree of requisites to be applied to potential dependencies.

   generic
      type Compatible_Property is new Property with private;
   package Property_Checker is

      type Requisite is new Requisites.Requisite with null record;

      function Is_Satisfied (R : Requisite; P : Compatible_Property) return Boolean;
      --  This is the important function to override by Requisite implementations

      --  The remainder methods are utilities that do not require modifications by the client.

      overriding function Is_Applicable (R : Requisite; P : Property'Class) return Boolean is
        (P in Compatible_Property);
      --  Convenience for the evaluator to determine which properties might satisfy a requisite

      function Cast (R : Requisite; P : Property'Class) return Compatible_Property'Class is
        (Compatible_Property'Class (P))
      with Pre => R.Is_Applicable (P);
      --  Convenience cast that can be done inside the package, but not outside, so it must be available here!

   end Property_Checker;

end Alire.Requisites;
