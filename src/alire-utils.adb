with Ada.Strings.Fixed;

with GNAT.Case_Util;

package body Alire.Utils is

   --------------
   -- Contains --
   --------------

   function Contains (Text : String; Sub : String) return Boolean is
     (Ada.Strings.Fixed.Count (Text, Sub) > 0);

   ----------
   -- Tail --
   ----------

   function Tail (Str : String; Separator : Character) return String is
   begin
      for I in Str'Range loop
         if Str (I) = Separator then
            return Str (I + 1 .. Str'Last);
         end if;
      end loop;

      return "";
   end Tail;

   -------------------
   -- To_Lower_Case --
   -------------------

   function To_Lower_Case (S : String) return String is
   begin
      return SLC : String := S do
         GNAT.Case_Util.To_Lower (SLC);
      end return;
   end To_Lower_Case;

   -------------------
   -- To_Mixed_Case --
   -------------------

   function To_Mixed_Case (S : String) return String is
   begin
      return SMC : String := S do
         GNAT.Case_Util.To_Mixed (SMC);
      end return;
   end To_Mixed_Case;

end Alire.Utils;
