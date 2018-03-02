package body Alire.Conditional_Values is

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

   begin
      return Evaluate (This.Constant_Reference);
   end Evaluate;

   --------------
   -- Evaluate --
   --------------

   function Evaluate (This : Conditional_Value; Against : Properties.Vector) return Conditional_Value is
     (New_Value (This.Evaluate (Against)));


end Alire.Conditional_Values;
