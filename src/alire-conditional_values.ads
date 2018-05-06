with Ada.Containers; use Ada.Containers;
with Ada.Iterator_Interfaces;

with Alire.Properties;
with Alire.Requisites;
with Alire.Utils;

private with Ada.Containers.Indefinite_Holders;
private with Ada.Containers.Indefinite_Vectors;

generic
   type Values (<>) is private;
   with function Image (V : Values) return String;
package Alire.Conditional_Values with Preelaborate is

   type Kinds is (Condition, Value, Vector);

   type Conditional_Value is tagged private with
     Default_Iterator => Iterate,
     Iterator_Element => Conditional_Value,
     Constant_Indexing => Indexed_Element;
   --  Recursive type that stores conditions (requisites) and values/further conditions if they are met or not

   function Leaf_Count (This : Conditional_Value) return Natural;

   generic
      type Collection is private;
      with procedure Append (C : in out Collection; V : Values; Count : Count_Type := 1);
   function Materialize (This : Conditional_Value; Against : Properties.Vector) return Collection;
   --  Materialize against the given properties, and return as list
   --  NOTE: this presumes there are no OR conditions along the tree
   --  In Alire context, this is always true for properties and
   --    potentially never for dependencies

   generic
      type Collection is private;
      with procedure Append (C : in out Collection; V : Values; Count : Count_Type := 1);
   function Enumerate (This : Conditional_Value) return Collection;
   --  Return all value nodes, regardless of dependencies/conjunctions
   --  This is used for textual search and has no semantic trascendence

   function Evaluate (This : Conditional_Value; Against : Properties.Vector) return Conditional_Value;
   --  Materialize against the given properties, returning values as an unconditional tree
   --  NOTE: the result is unconditional but can still contain a mix of AND/OR subtrees

   function Kind (This : Conditional_Value) return Kinds;

   function Is_Empty (This : Conditional_Value) return Boolean;

   function Empty return Conditional_Value;

   function Image_One_Line (This : Conditional_Value) return String;

   function Is_Unconditional (This : Conditional_Value) return Boolean;
   -- Recursively!

   function Contains_ORs (This : Conditional_Value) return Boolean;

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

   function "or" (L, R : Conditional_Value) return Conditional_Value;

   type Conjunctions is (Anded, Ored);

   function Conjunction (This : Conditional_Value) return Conjunctions
     with Pre => This.Kind = Vector;

   procedure Iterate_Children (This    : Conditional_Value;
                               Visitor : access procedure (CV : Conditional_Value));
   --  There is "of" notation too, but that bugs out when using this package as generic formal

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

   generic
      type Enum is (<>);
      with function Requisite_Equal (V : Enum) return Requisites.Tree;
      --  Function which creates an equality requisite on V
   package Case_Statements is

      type Arrays is array (Enum) of Conditional_Value;

      function Case_Is (Arr : Arrays) return Conditional_Value;

   end Case_Statements;

   -----------
   -- Print --
   -----------

   procedure Print (This   : Conditional_Value;
                    Prefix : String := "";
                    And_Or : Boolean := True);
   --  And_Or is false if only And can appear, thus no necessity to distinguish

   -----------------
   --  ITERATORS  --
   -----------------

   type Cursor is private;

   function Has_Element (This : Cursor) return Boolean;

   function Next (This : Cursor) return Cursor;

   package Iterators is new Ada.Iterator_Interfaces (Cursor, Has_Element);

   function Iterate (Container : Conditional_Value)
      return Iterators.Forward_Iterator'Class;
   -- Returns our own iterator, which in general will be defined in the
   -- private part or the body.

   function Indexed_Element (Container : Conditional_Value; Pos : Cursor)
      return Conditional_Value;

private

   type Inner_Node is abstract tagged null record;

   function Image (Node : Inner_Node) return String is abstract;

   function Image_Classwide (Node : Inner_Node'Class) return String is (Node.Image);

   function Kind (This : Inner_Node'Class) return Kinds;

   package Holders is new Ada.Containers.Indefinite_Holders (Inner_Node'Class);
   package Vectors is new Ada.Containers.Indefinite_Vectors (Positive, Inner_Node'Class);

   type Cursor is new Vectors.Cursor;

   type Conditional_Value is new Holders.Holder with null record;
   --  Instead of dealing with pointers and finalization, we use this class-wide container

   package Definite_Values is new Ada.Containers.Indefinite_Holders (Values);

   type Value_Inner is new Inner_Node with record
      Value : Definite_Values.Holder;
   end record;

   overriding function Image (V : Value_Inner) return String is
      (Image (V.Value.Constant_Reference));

   type Vector_Inner is new Inner_Node with record
      Conjunction : Conjunctions;
      Values      : Vectors.Vector;
   end record;

   function Conjunction (This : Vector_Inner) return Conjunctions is
      (This.Conjunction);

   package Non_Primitive is
      function One_Liner_And is new Utils.Image_One_Line
        (Vectors,
         Vectors.Vector,
         Image_Classwide,
         " and ",
         "(empty condition)");

      function One_Liner_Or is new Utils.Image_One_Line
        (Vectors,
         Vectors.Vector,
         Image_Classwide,
         " or ",
         "(empty condition)");
   end Non_Primitive;

   overriding function Image (V : Vector_Inner) return String is
     (if V.Conjunction = Anded
      then Non_Primitive.One_Liner_And (V.Values)
      else Non_Primitive.One_Liner_Or (V.Values));

   type Conditional_Inner is new Inner_Node with record
      Condition  : Requisites.Tree;
      Then_Value : Conditional_Value;
      Else_Value : Conditional_Value;
   end record;

   overriding function Image (V : Conditional_Inner) return String is
     ("if " & V.Condition.Image &
        " then " & V.Then_Value.Image_One_Line &
        " else " & V.Else_Value.Image_One_Line);

   --------------
   -- As_Value --
   --------------

   function As_Value (This : Conditional_Value) return Values
   is
     (Value_Inner (This.Element).Value.Element)
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

   -----------------
   -- Conjunction --
   -----------------

   function Conjunction (This : Conditional_Value) return Conjunctions is
      (Vector_Inner'Class (This.Element).Conjunction);

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
     (To_Holder (Value_Inner'(Value => Definite_Values.To_Holder (V))));

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

   function Kind (This : Inner_Node'Class) return Kinds is
     (if This in Value_Inner'Class
      then Value
      else (if This in Vector_Inner'Class
            then Vector
            else Condition));

   function Kind (This : Conditional_Value) return Kinds is
     (This.Constant_Reference.Kind);

   --------------------
   -- Image_One_Line --
   --------------------

   function Image_One_Line (This : Conditional_Value) return String is
     (if This.Is_Empty
      then "(empty condition)"
      else This.Constant_Reference.Image);

end Alire.Conditional_Values;
