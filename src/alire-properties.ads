with Ada.Containers.Indefinite_Vectors;

package Alire.Properties with Preelaborate is

   --  Properties are the general mechanism used to store all info about a release.
   --  They can be specialized (e.g. in version, platform, compiler) but that can be transparent to the user.

   --  Since a property can be checked against a variety of conditions, this would require fully fledged
   --  multiple inheritance for the simplest design.
   --  Instead, a first check of matching tags is done and then the checks can proceed.

   type Property is interface;

   function Image (P : Property) return String is abstract;

   package Vectors is new Ada.Containers.Indefinite_Vectors (Positive, Property'Class);

   subtype Vector is Vectors.Vector;

   No_Properties : Vector renames Vectors.Empty_Vector;

   function "and" (L, R : Property'Class)          return Vector;
   function "and" (L : Vector; R : Property'Class) return Vector;

   --  A generic helper to simply store/retrieve e.g. an enumerated type
   generic
      type Value is private;
      with function Image (V : Value) return String is <>;
   package Values is

      type Property (<>) is new Properties.Property with private;

      function New_Property (V : Value) return Property;

      function Element (P : Property) return Value;

   private

      overriding function Image (P : Property) return String;

      type Property is new Properties.Property with record
         V : Value;
      end record;

      function New_Property (V : Value) return Property is (V => V);

      function Element (P : Property) return Value is (P.V);

      overriding function Image (P : Property) return String is (Image (P.V));

   end Values;

private

   use all type Vector;

   function "and" (L, R : Property'Class) return Vector is (L & R);

   function "and" (L : Vector; R : Property'Class) return Vector is (L & R);

end Alire.Properties;
