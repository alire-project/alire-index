with Alire.Properties;
with Alire.Requisites;

generic
   type Values is private;
package Alire.Conditional is

   type Conditional_Value (<>) is tagged private;

   function New_Conditional (If_X   : Requisites.Tree;
                             Then_X : Values;
                             Else_X : Values) return Conditional_Value;

   function Evaluate (This : Conditional_Value; Against : Properties.Vector) return Values;

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

end Alire.Conditional;
