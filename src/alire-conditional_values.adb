package body Alire.Conditional_Values is

   -----------
   -- "and" --
   -----------

   function "and" (L, R : Conditional_Value) return Conditional_Value is
   begin
      return Result : Conditional_Value do
         if L.Is_Empty and then R.Is_Empty then
            null; -- nothing to do nor return
         else
            declare
               Inner : Vector_Inner;
            begin
               if not L.Is_Empty then
                  Inner.Values.Append (L.Constant_Reference);
               end if;

               if not R.Is_Empty then
                  Inner.Values.Append (R.Constant_Reference);
               end if;

               Result.Replace_Element (Inner);
            end;
         end if;
      end return;
   end "and";

   --------------
   -- Evaluate --
   --------------

   function Evaluate (This : Conditional_Value; Against : Properties.Vector) return Values is

      function Evaluate (This : Inner_Value'Class) return Values is
      begin
         case This.Kind is
            when Condition =>
               declare
                  Cond : Conditional_Inner renames Conditional_Inner (This);
               begin
                  if Cond.Condition.Check (Against) then
                     return Cond.Then_Value.Evaluate (Against);
                  else
                     return Cond.Else_Value.Evaluate (Against);
                  end if;
               end;
            when Value =>
               return Value_Inner (This).Value;
            when Vector =>
               return Result : Values do
                  for Cond of Vector_Inner (This).Values loop
                     Result := Result & Evaluate (Cond);
                  end loop;
               end return;
         end case;
      end Evaluate;

      Empty_Value : Values;
   begin
      if This.Is_Empty then
         return Empty_Value;
      else
         return Evaluate (This.Constant_Reference);
      end if;
   end Evaluate;

   --------------
   -- Evaluate --
   --------------

   function Evaluate (This : Conditional_Value; Against : Properties.Vector) return Conditional_Value is
     (New_Value (This.Evaluate (Against)));


end Alire.Conditional_Values;
