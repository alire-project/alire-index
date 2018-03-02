with Alire.Properties;
with Alire.Requisites;

private with Ada.Containers.Indefinite_Holders;
private with Ada.Containers.Indefinite_Vectors;

generic
   type Values is private;
   with function "&" (L, R : Values) return Values;
package Alire.Conditional_Values with Preelaborate is

   type Kinds is (Condition, Value, Vector);

   type Conditional_Value is tagged private;
   --  Recursive type that stores conditions (requisites) and values/further conditions if they are met or not

   function Evaluate (This : Conditional_Value; Against : Properties.Vector) return Values;
   --  Materialize against the given properties

   function Evaluate (This : Conditional_Value; Against : Properties.Vector) return Conditional_Value;
   --  Materialize against the given properties, returning values as an unconditional vector

   function Kind (This : Conditional_Value) return Kinds;

   function Is_Empty (This : Conditional_Value) return Boolean;

   function Empty return Conditional_Value;

   ---------------
   --  SINGLES  --
   ---------------

   function New_Value (V : Values) return Conditional_Value; -- when we don't really need a condition

   function Value (This : Conditional_Value) return Values
     with Pre => This.Kind = Value;

   ---------------
   --  VECTORS  --
   ---------------

   function "and" (L, R : Conditional_Value) return Conditional_Value;
   --  Concatenation

   procedure Iterate_Children (This    : Conditional_Value;
                               Visitor : access procedure (CV : Conditional_Value))
     with Pre => This.Kind = Vector;

   --------------------
   --  CONDITIONALS  --
   --------------------

   function New_Conditional (If_X   : Requisites.Tree;
                             Then_X : Conditional_Value;
                             Else_X : Conditional_Value) return Conditional_Value;

   function Condition (This : Conditional_Value) return Requisites.Tree
     with Pre => This.Kind = Condition;

   function True_Value (This : Conditional_Value) return Conditional_Value
     with Pre => This.Kind = Condition;

   function False_Value (This : Conditional_Value) return Conditional_Value
     with Pre => This.Kind = Condition;

private

   type Inner_Value is abstract tagged null record;

   function Kind (This : Inner_Value'Class) return Kinds;

   package Holders is new Ada.Containers.Indefinite_Holders (Inner_Value'Class);
   package Vectors is new Ada.Containers.Indefinite_Vectors (Positive, Inner_Value'Class);

   type Conditional_Value is new Holders.Holder with null record;
   --  Instead of dealing with pointers and finalization, we use this class-wide container

   type Value_Inner is new Inner_Value with record
      Value : Values;
   end record;

   type Vector_Inner is new Inner_Value with record
      Values : Vectors.Vector;
   end record;

   type Conditional_Inner is new Inner_Value with record
      Condition  : Requisites.Tree;
      Then_Value : Conditional_Value;
      Else_Value : Conditional_Value;
   end record;

   --------------
   -- As_Value --
   --------------

   function As_Value (This : Conditional_Value) return Values
   is
     (Value_Inner (This.Constant_Reference.Element.all).Value)
   with Pre => This.Kind = Value;

   --------------------
   -- As_Conditional --
   --------------------

   function As_Conditional (This : Conditional_Value) return Conditional_Inner'Class is
     (Conditional_Inner'Class (This.Element))
   with Pre => This.Kind = Condition;

   ---------------
   -- As_Vector --
   ---------------

   function As_Vector (This : Conditional_Value) return Vectors.Vector is
     (Vector_Inner'Class (This.Element).Values)
       with Pre => This.Kind = Vector;

   ---------------------
   -- New_Conditional --
   ---------------------

   function New_Conditional (If_X   : Requisites.Tree;
                             Then_X : Conditional_Value;
                             Else_X : Conditional_Value) return Conditional_Value is
     (To_Holder (Conditional_Inner'(Condition  => If_X,
                                    Then_Value => Then_X,
                                    Else_Value => Else_X)));

   ---------------
   -- New_Value --
   ---------------

   function New_Value (V : Values) return Conditional_Value is
     (To_Holder (Value_Inner'(Value => V)));

   ---------------
   -- Condition --
   ---------------

   function Condition (This : Conditional_Value) return Requisites.Tree is
     (This.As_Conditional.Condition);

   -----------
   -- Value --
   -----------

   function Value (This : Conditional_Value) return Values renames As_Value;

   ----------------
   -- True_Value --
   ----------------

   function True_Value (This : Conditional_Value) return Conditional_Value is
      (This.As_Conditional.Then_Value);

   -----------------
   -- False_Value --
   -----------------

   function False_Value (This : Conditional_Value) return Conditional_Value is
      (This.As_Conditional.Else_Value);

   -----------
   -- Empty --
   -----------

   function Empty return Conditional_Value is
      (Holders.Empty_Holder with null record);

   --------------
   -- Is_Empty --
   --------------

   overriding function Is_Empty (This : Conditional_Value) return Boolean is
     (Holders.Holder (This).Is_Empty);

   ----------
   -- Kind --
   ----------

   function Kind (This : Inner_Value'Class) return Kinds is
     (if This in Value_Inner'Class
      then Value
      else (if This in Vector_Inner'Class
            then Vector
            else Condition));

   function Kind (This : Conditional_Value) return Kinds is
      (This.Constant_Reference.Kind);

   --  The price of doing this without pointers is this manual dispatching...

end Alire.Conditional_Values;
