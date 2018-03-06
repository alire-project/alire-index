with Ada.Containers.Indefinite_Vectors;

package Alire.Utils with Preelaborate is

   function To_Lower_Case (S : String) return String;
   function To_Mixed_Case (S : String) return String;

   function Contains (Text : String; Sub : String) return Boolean;

   function Head (Str : String; Separator : Character) return String;
   --  if Str contains Separator, the lhs is returned
   --  Otherwise Str is returned

   function Tail (Str : String; Separator : Character) return String;
   --  If Str contains Separator, the rhs is returned
   --  Otherwise ""

   generic
      with package Vectors is new Ada.Containers.Indefinite_Vectors (<>);
      type Vector is new Vectors.Vector with private;
      with function Image (Item : Vectors.Element_Type) return String is <>;
      Separator  : String := " ";
      When_Empty : String := "(empty)";
   function Image_One_Line (V : Vector) return String;

   --------------------
   -- String_Vectors --
   --------------------

   --  To simplify somewhat managing lists of strigns

   package String_Vectors is new Ada.Containers.Indefinite_Vectors (Positive, String);
   type String_Vector is new String_Vectors.Vector with null record;

   Empty_Vector : constant String_Vector;

   function Count (V : String_Vector) return Natural;
   --  FSM do I hate the Containers.Count_Type...

   function Flatten (V : String_Vector; Separator : String := " ") return String;
   --  Concatenate all elements

   -----------------
   -- XXX_XXX_XXX --
   -----------------

   type XXX_XXX (<>) is limited private;
   function XXX_XXX_XXX return XXX_XXX;


private

   Empty_Vector : constant String_Vector := (String_Vectors.Empty_Vector with null record);

   function Count (V : String_Vector) return Natural is (Natural (String_Vectors.Vector (V).Length));

   type XXX_XXX is limited null record;
   function XXX_XXX_XXX return XXX_XXX is (null record);

end Alire.Utils;
