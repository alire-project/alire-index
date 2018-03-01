with Alire.Properties;
with Alire.Requisites;

generic
   type Values is private;
   with function "&" (L, R : Values) return Values with Unreferenced; -- used in child vectors
   --  FIXME: we'll have to keep an eye on the overhead of this (append to be considered)
package Alire.Conditional with Preelaborate is

   type Conditional_Value (<>) is tagged private;

   function New_Conditional (If_X   : Requisites.Tree;
                             Then_X : Values;
                             Else_X : Values) return Conditional_Value;

   function New_Inconditional (V : Values) return Conditional_Value;

   function Evaluate (This : Conditional_Value; Against : Properties.Vector) return Values;

   function Condition (This : Conditional_Value) return Requisites.Tree;

   function Is_Inconditional (This : Conditional_Value) return Boolean;

   function True_Value (This : Conditional_Value) return Values;

   function False_Value (This : Conditional_Value) return Values;

private

   type Conditional_Value is tagged record
      Condition  : Requisites.Tree;
      Then_Value : Values;
      Else_Value : Values;
   end record;

   function Evaluate (This : Conditional_Value; Against : Properties.Vector) return Values is
     (if This.Condition.Check (Against)
      then This.Then_Value
      else This.Else_Value);

   function New_Conditional (If_X   : Requisites.Tree;
                             Then_X : Values;
                             Else_X : Values) return Conditional_Value is
     (Condition  => If_X,
      Then_Value => Then_X,
      Else_Value => Else_X);

   function New_Inconditional (V : Values) return Conditional_Value is
     (Condition => Requisites.No_Requisites,
      Then_Value => V,
      Else_Value => <>);

   function Condition (This : Conditional_Value) return Requisites.Tree is (This.Condition);

   function Is_Inconditional (This : Conditional_Value) return Boolean is (This.Condition.Is_Empty);

   function True_Value (This : Conditional_Value) return Values is (This.Then_Value);

   function False_Value (This : Conditional_Value) return Values is (This.Else_Value);

end Alire.Conditional;
