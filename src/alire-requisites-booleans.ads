package Alire.Requisites.Booleans with Preelaborate is

   type Requisite_True is new Requisite with null record;

   type Requisite_False is new Requisite with null record;

   function Satisfies (R : Requisite_True; P : Property'Class) return Boolean is (True);
   function Image     (R : Requisite_True) return String is ("True");

   function Satisfies (R : Requisite_False; P : Property'Class) return Boolean is (False);
   function Image     (R : Requisite_False) return String is ("False");

   function Always_True  return Tree is (Trees.Leaf (Requisite_True'(null record)));
   function Always_False return Tree is (Trees.Leaf (Requisite_False'(null record)));

end Alire.Requisites.Booleans;
