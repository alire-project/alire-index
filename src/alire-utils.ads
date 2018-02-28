with Ada.Containers.Indefinite_Vectors;

package Alire.Utils with Preelaborate is

   function To_Lower_Case (S : String) return String;
   function To_Mixed_Case (S : String) return String;

   function Contains (Text : String; Sub : String) return Boolean;

   package String_Vectors is new Ada.Containers.Indefinite_Vectors (Positive, String);
   subtype String_Vector is String_Vectors.Vector;

   function Head (Str : String; Separator : Character) return String;
   --  if Str contains Separator, the lhs is returned
   --  Otherwise Str is returned

   function Tail (Str : String; Separator : Character) return String;
   --  If Str contains Separator, the rhs is returned
   --  Otherwise ""

end Alire.Utils;
