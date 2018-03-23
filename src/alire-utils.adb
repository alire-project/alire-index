with Ada.Strings.Fixed;

with GNAT.Case_Util;

package body Alire.Utils is

   --------------
   -- Contains --
   --------------

   function Contains (Text : String; Sub : String) return Boolean is
     (Ada.Strings.Fixed.Count (Text, Sub) > 0);

   ----------
   -- Head --
   ----------

   function Head (Str : String; Separator : Character) return String is
   begin
      for I in Str'Range loop
         if Str (I) = Separator then
            return Str (Str'First .. I - 1);
         end if;
      end loop;

      return Str;
   end Head;

   -------------
   -- Flatten --
   -------------

   function Flatten (V : String_Vector; Separator : String := " ") return String is

      function Flatten (Pos : Positive; V : String_Vector) return String is
        (if Pos > V.Count
         then ""
         else V (Pos) & Separator & Flatten (Pos + 1, V));

   begin
      return Flatten (1, V);
   end Flatten;

   -------------
   -- Replace --
   -------------

   function Replace (Text : String; Match : String; Subst : String) return String is
      use Ada.Strings.Fixed;
      First : Natural;
   begin
      First := Index (Text, Match);
      if First = 0 then
         return Text;
      else
         return Replace (Replace_Slice (Text, First, First + Match'Length - 1, Subst), Match, Subst);
      end if;
   end Replace;

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

   --------------------
   -- Image_One_Line --
   --------------------

   function Image_One_Line (V : Vector) return String is

      use all type Vectors.Index_Type;

      function Image (V : Vector; Pos : Vectors.Index_Type) return String is
        (Image (V.Element (Pos)) &
         (if Pos = V.Last_Index
          then ""
          else Separator & Image (V, Pos + 1)));

   begin
      if V.Is_Empty then
         return When_Empty;
      else
         return Image (V, V.First_Index);
      end if;
   end Image_One_Line;

end Alire.Utils;
