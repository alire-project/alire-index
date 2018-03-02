with Ada.Containers.Indefinite_Vectors;

generic
package Alire.Conditional.Vectors with Preelaborate is

   package Condition_Vectors is new Ada.Containers.Indefinite_Vectors (Positive, Conditional_Value);

   type Vector is new Condition_Vectors.Vector with null record;

   function All_Values (V : Vector) return Values;

   function Evaluate (V : Vector; On : Properties.Vector) return Values;
   --  Return the values that pass evaluation

   function Evaluate (V : Vector; On : Properties.Vector) return Vector;
   --  Take the values that pass evaluation and make a vector of inconditionals

   function New_Conditional (If_X   : Requisites.Tree;
                             Then_X : Values;
                             Else_X : Values) return Vector is
      (To_Vector (New_Conditional (If_X, Then_X, Else_X), 1));

   function New_Unconditional (V : Values) return Vector is (To_Vector (New_Unconditional (V), 1));

   function "+" (V : Conditional_Value) return Vector is (To_Vector (V, 1));
   function "and" (L, R : Vector) return Vector is (L & R);

   Empty_Vector : constant Vector := (Condition_Vectors.Empty_Vector with null record);

end Alire.Conditional.Vectors;
