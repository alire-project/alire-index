with Ada.Containers;
with Ada.Strings.Fixed;

with GNAT.IO;

package body Table_IO is

   use all type Ada.Containers.Count_Type;

   ------------
   -- Append --
   ------------

   procedure Append (T : in out Table; Cell : String) is
   begin
      if T.Rows.Is_Empty then
         T.New_Row;
      end if;

      if Natural (T.Max_Widths.Length) < T.Next_Column then
         T.Max_Widths.Append (Cell'Length);
      else
         T.Max_Widths (T.Next_Column) :=
           Natural'Max (Cell'Length, T.Max_Widths (T.Next_Column));
      end if;

      T.Rows (Natural (T.Rows.Length)).Append (Cell);
      T.Next_Column := T.Next_Column + 1;
   end Append;

   -------------
   -- New_Row --
   -------------

   procedure New_Row (T : in out Table) is
   begin
      T.Next_Column := 1;
      T.Rows.Append (String_Vectors.Empty_Vector);
   end New_Row;

   ----------------
   -- Put_Padded --
   ----------------

   procedure Put_Padded (T     : Table;
                         Col   : Positive;
                         Text  : String;
                         Align : Ada.Strings.Alignment)
   is
      Field : String (1 .. T.Max_Widths (Col));
   begin
      Ada.Strings.Fixed.Move (Text,
                              Field,
                              Drop    => Ada.Strings.Error,
                              Justify => Align);
      GNAT.IO.Put (Field);
   end Put_Padded;

   -----------
   -- Print --
   -----------

   procedure Print  (T         : Table;
                    Separator : String := " ";
                     Align     : Alignments := (1 .. 0 => <>))
   is
      use GNAT.IO;
   begin
      for Row of T.Rows loop
         for I in 1 .. Natural (Row.Length) loop
            Put_Padded (T,
                        I,
                        Row (I),
                        (if Align'Length >= I
                         then Align (I)
                         else Ada.Strings.Left));

            if I < Natural (Row.Length) then
               Put (Separator);
            else
               New_Line;
            end if;
         end loop;
      end loop;
   end Print;

end Table_IO;
