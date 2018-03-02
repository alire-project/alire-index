package body Alire.Conditional_Values is

   -----------
   -- "and" --
   -----------

   function "and" (L, R : Conditional_Value) return Conditional_Value is
   --  FIXME: we could do an effort to flatten this binary tree that's forming here in longer vectors
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

      Empty_Value : Values with Warnings => Off;
      -- Default value should made sense; in our case it will be an empty vector...
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

   -------------
   -- Iterate --
   -------------

   procedure Iterate_Children (This    : Conditional_Value;
                               Visitor : access procedure (CV : Conditional_Value))
   is
   begin
      for Inner of Vector_Inner (This.Constant_Reference.Element.all).Values loop
         case Inner.Kind is
            when others => BANG
         end case;
      end loop;
   end Iterate_Children;

end Alire.Conditional_Values;
