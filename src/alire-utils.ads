with Ada.Containers.Indefinite_Vectors;

package Alire.Utils with Preelaborate is

   function To_Mixed_Case (S : String) return String;

   package String_Vectors is new Ada.Containers.Indefinite_Vectors (Positive, String);
   subtype String_Vector is String_Vectors.Vector;

end Alire.Utils;
