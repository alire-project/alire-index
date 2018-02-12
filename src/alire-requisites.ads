with Alire.Properties;

with Condtrees;

package Alire.Requisites with Preelaborate is

   use Properties;

   type Requisite is abstract tagged null record;
   --  A Requisite verifies against some internally stored data that a property is satisfied.
   --  Here we provide the basic storage of values but the actual checking function must be overridden
   --    for particular checks.

   function Is_Applicable (R : Requisite; P : Property'Class) return Boolean is (False);
   --  Initially there is no compatibility. See helper package below

   --  The following package is the building block to be used to define new compatibility checks.
   --  Here we tie a class of properties and requisites (e.g., versions and version sets) that make sense.
   --  A release has a list of properties, and a tree of requisites to be applied to potential dependencies.

   function Satisfies (R : Requisite; P : Property'Class) return Boolean is abstract;
   --  This function is used later in the generic implementation to automatically downcast,
   --  so requisite implementations do not need to deal with this MI-mess

   generic
      type Compatible_Property (<>) is new Property with private;
   package Typed_Requisites is

      type Requisite is abstract new Requisites.Requisite with null record;

      not overriding
      function Is_Satisfied (R : Requisite; P : Compatible_Property) return Boolean is abstract;
      --  This is the important function to override by Requisite implementations

      --  The remainder methods are utilities that do not require modifications by the client.

      overriding function Is_Applicable (R : Requisite; P : Property'Class) return Boolean is
        (P in Compatible_Property);
      --  Convenience for the evaluator to determine which properties might satisfy a requisite

      overriding
      function Satisfies (R : Requisite; P : Property'Class) return Boolean is
        (if R.Is_Applicable (P)
         then Requisite'Class (R).Is_Satisfied (Compatible_Property (P))
         else False);

   end Typed_Requisites;

   --  Trees of requisites to be matched against a list of properties in a release

   function Satisfies (R : Requisite'Class; P : Properties.Vector) return Boolean;
   --  True if any of the properties in the vector satisfies the requisite

   package Trees is new Condtrees (Properties.Vector,
                                   Requisite'Class,
                                   Satisfies);

   subtype Tree is Trees.Tree;

   function No_Requisites return Trees.Tree is (Trees.Empty_Tree);
   --  Function instead of constant to keep Preelaborate

end Alire.Requisites;
