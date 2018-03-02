package body Alire.Conditional.Vectors is

   ----------------
   -- All_Values --
   ----------------

   function All_Values (V : Vector) return Values is
   begin
      return Result : Values do
         for Cond of V loop
            Result := Result & Cond.Then_Value & Cond.Else_Value;
         end loop;
      end return;
   end All_Values;

   --------------
   -- Evaluate --
   --------------

   function Evaluate (V : Vector; On : Properties.Vector) return Values is
   begin
      return Result : Values do
         for Cond of V loop
            Result := Result & Cond.Evaluate (On);
         end loop;
      end return;
   end Evaluate;

   --------------
   -- Evaluate --
   --------------

   function Evaluate (V : Vector; On : Properties.Vector) return Vector is
   begin
      return Result : Vector do
         for Cond of V loop
            Result.Append (Conditional_Value'(New_Unconditional (Values'(Cond.Evaluate (On)))));
         end loop;
      end return;
   end Evaluate;

end Alire.Conditional.Vectors;
