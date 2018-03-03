with Alire.Boolean_Trees;
with Alire.Properties;
with Alire.Utils;

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

   function Image (R : Requisite) return String is abstract;
   --  A necessary pain to be able to report

   --  Trees of requisites to be matched against a list of properties in a release

   function Satisfies (R : Requisite'Class; P : Properties.Vector) return Boolean;
   --  True if any of the properties in the vector satisfies the requisite

   function Image_Class (R : Requisite'Class) return String is (R.Image);

   package Trees is new Boolean_Trees (Properties.Vector,
                                       Requisite'Class,
                                       Satisfies,
                                       Image_Class);

   subtype Tree is Trees.Tree;

   function No_Requisites return Trees.Tree is (Trees.Empty_Tree);
   --  Function instead of constant to keep Preelaborate

   ----------------------
   -- Typed_Requisites --
   ----------------------
   -- Using these we get free matching of properties to requisites
   -- It is in essence a work around MI

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

      --  For free we get a requisite that is equality for value properties

   end Typed_Requisites;

   --------------
   --  EXTRAS  --
   --------------

   --  This following requisite is a typed_requisite for equality with the property
   --  Concevably, this could be expanded to offer >=, <, <=...

   generic
      with package Values is new Properties.Values (<>);
      -- The property that encapsulates the requiiste value

      Name : String; -- used for image "Name is Mixed_Case (Image (Value))"
   package Typed_Value_Requisites is

      package Value_Requisites is new Typed_Requisites (Values.Property);

      type Equality is new Value_Requisites.Requisite with private;

      function New_Equality (V : Values.Value) return Tree;

      overriding function Is_Satisfied (R : Equality;
                                        P : Values.Property)
                                        return Boolean;

      overriding function Image        (R : Equality)
                                        return String;

   private

      function Mix (S : String) return String renames Utils.To_Mixed_Case;
      use all type Values.Value;

      type Equality is new Value_Requisites.Requisite with record
         Value : Values.Value;
      end record;

      function New_Equality (V : Values.Value) return Tree is
         (Trees.Leaf (Equality'(Value => V)));

      overriding function Is_Satisfied (R : Equality; P : Values.Property) return Boolean is
        (R.Value = P.Element);

      overriding function Image (R : Equality) return String is
        (Name & " is " & Mix (Values.Image (R.Value)));

   end Typed_Value_Requisites;

end Alire.Requisites;
