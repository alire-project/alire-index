package body Alire.Conditional_Values is

   -----------
   -- "and" --
   -----------

   function "and" (L, R : Conditional_Value) return Conditional_Value is
      Inner : Vector_Inner;

      -------------
      -- Flatten --
      -------------

      procedure Flatten (This : Inner_Node'Class) is
      begin
         case This.Kind is
            when Value | Condition =>
               Inner.Values.Append (This);
            when Vector =>
               for Child of Vector_Inner (This).Values loop
                  Flatten (Child);
               end loop;
         end case;
      end Flatten;

   begin
      if not L.Is_Empty then
         Flatten (L.Constant_Reference);
      end if;

      if not R.Is_Empty then
         Flatten (R.Constant_Reference);
      end if;

      if Inner.Values.Is_Empty then
         return Empty;
      else
         return (To_Holder (Inner));
      end if;
   end "and";

   --------------
   -- Evaluate --
   --------------

   function Evaluate (This : Conditional_Value; Against : Properties.Vector) return Values is

      function Evaluate (This : Inner_Node'Class) return Values is
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

      procedure Iterate (This : Inner_Node'Class) is
      begin
         case This.Kind is
            when Value | Condition =>
               Visitor (To_Holder (This));
            when Vector =>
               for Inner of Vector_Inner (This).Values loop
                  case Inner.Kind is
                     when Value =>
                        Visitor (New_Value (Value_Inner (Inner).Value));
                     when Condition =>
                        declare
                           Cond : Conditional_Inner renames Conditional_Inner (Inner);
                        begin
                           Visitor (New_Conditional (Cond.Condition, Cond.Then_Value, Cond.Else_Value));
                        end;
                     when Vector =>
                        Iterate (Inner);
                  end case;
               end loop;
         end case;
      end Iterate;

   begin
      if not This.Is_Empty then
         Iterate (This.Constant_Reference);
      end if;
   end Iterate_Children;

end Alire.Conditional_Values;
