with GNAT.IO;

package body Alire.Conditional_Values is

   function All_But_First_Children (This : Conditional_Value) return Conditional_Value is
      Children : Vectors.Vector := This.As_Vector;
   begin
      Children.Delete_First;
      return To_Holder (Vector_Inner'(This.Conjunction, Children));
   end All_But_First_Children;

   -------------
   -- Flatten --
   -------------

   procedure Flatten (Inner : in out Vector_Inner; -- The resulting vector
                      This  : Inner_Node'Class;    -- The next node to flatten
                      Conj  : Conjunctions) is      -- To prevent mixing
      begin
      case This.Kind is
         when Value | Condition =>
            Inner.Values.Append (This);
         when Vector =>
            --  Flatten ofly if conjunction matches, otherwise just append subtree
            if Vector_Inner (This).Conjunction = Conj then
               for Child of Vector_Inner (This).Values loop
                  Flatten (Inner, Child, Conj);
               end loop;
            else
               Inner.Values.Append (This);
            end if;
      end case;
   end Flatten;

   -----------
   -- "and" --
   -----------

   function "and" (L, R : Conditional_Value) return Conditional_Value is
      Inner : Vector_Inner := (Conjunction => Anded, Values => <>);

   begin
      if not L.Is_Empty then
         Flatten (Inner, L.Constant_Reference, Anded);
      end if;

      if not R.Is_Empty then
         Flatten (Inner, R.Constant_Reference, Anded);
      end if;

      if Inner.Values.Is_Empty then
         return Empty;
      else
         return (To_Holder (Inner));
      end if;
   end "and";

   ----------
   -- "or" --
   ----------

   function "or" (L, R : Conditional_Value) return Conditional_Value is
      Inner : Vector_Inner := (Conjunction => Ored, Values => <>);

   begin
      if not L.Is_Empty then
         Flatten (Inner, L.Constant_Reference, Ored);
      end if;

      if not R.Is_Empty then
         Flatten (Inner, R.Constant_Reference, Ored);
      end if;

      if Inner.Values.Is_Empty then
         return Empty;
      else
         return (To_Holder (Inner));
      end if;
   end "or";

   ----------------
   -- Leaf_Count --
   ----------------

   function Leaf_Count (This : Conditional_Value) return Natural is
      Count : Natural := 0;
   begin
      if This.Is_Empty then
         return 0;
      else
         case This.Kind is
            when Value =>
               return 1;
            when Condition =>
               return This.True_Value.Leaf_Count + This.False_Value.Leaf_Count;
            when Vector =>
               for Child of This loop
                  Count := Count + Child.Leaf_Count;
               end loop;
               return Count;
         end case;
      end if;
   end Leaf_Count;

   -----------------
   -- Materialize --
   -----------------

   function Materialize (This : Conditional_Value; Against : Properties.Vector) return Collection is
      Col : Collection with Warnings => Off;
      Pre : constant Conditional_Value := This.Evaluate (Against);

      procedure Visit (Inner : Inner_Node'Class) is
      begin
         case Inner.Kind is
            when Value =>
               Append (Col, Value_Inner (Inner).Value.Constant_Reference);
            when Condition =>
               raise Program_Error with "Should not appear in evaluated CV";
            when Vector =>
               if Vector_Inner (Inner).Conjunction = Anded then
                  for Child of Vector_Inner (Inner).Values loop
                     Visit (Child);
                  end loop;
               else
                  raise Constraint_Error with "OR trees cannot be materialized as list";
               end if;
         end case;
      end Visit;

   begin
      if not This.Is_Empty then
         Visit (Pre.Constant_Reference);
      end if;
      return Col;
   end Materialize;

   ---------------
   -- Enumerate --
   ---------------

   function Enumerate (This : Conditional_Value) return Collection is
      Col : Collection with Warnings => Off;

      procedure Visit (Inner : Inner_Node'Class) is
      begin
         case Inner.Kind is
            when Value =>
               Append (Col, Value_Inner (Inner).Value.Constant_Reference);
            when Condition =>
               Visit (Conditional_Inner (Inner).Then_Value.Constant_Reference);
               Visit (Conditional_Inner (Inner).Else_Value.Constant_Reference);
            when Vector =>
               if Vector_Inner (Inner).Conjunction = Anded then
                  for Child of Vector_Inner (Inner).Values loop
                     Visit (Child);
                  end loop;
               else
                  raise Constraint_Error with "OR trees cannot be materialized as list";
               end if;
         end case;
      end Visit;

   begin
      if not This.Is_Empty then
         Visit (This.Constant_Reference);
      end if;
      return Col;
   end Enumerate;

   --------------
   -- Evaluate --
   --------------

   function Evaluate (This : Conditional_Value; Against : Properties.Vector) return Conditional_Value is

      function Evaluate (This : Inner_Node'Class) return Conditional_Value is
      begin
         case This.Kind is
            when Condition =>
               declare
                  Cond : Conditional_Inner renames Conditional_Inner (This);
               begin
                  if Cond.Condition.Check (Against) then
                     return Evaluate (Cond.Then_Value.Element);
                  elsif not Cond.Else_Value.Is_Empty then
                     return Evaluate (Cond.Else_Value.Element);
                  else
                     return Empty;
                  end if;
               end;
            when Value =>
               return Conditional_Value'(To_Holder (This));
            when Vector =>
               return Result : Conditional_Value := Empty do
                  for Cond of Vector_Inner (This).Values loop
                     if Vector_Inner (This).Conjunction = Anded then
                        Result := Result and Evaluate (Cond);
                     else
                        Result := Result or Evaluate (Cond);
                     end if;
                  end loop;
               end return;
         end case;
      end Evaluate;

   begin
      if This.Is_Empty then
         return This;
      else
         return Evaluate (This.Element);
      end if;
   end Evaluate;

   ------------------
   -- Contains_ORs --
   ------------------

   function Contains_ORs (This : Conditional_Value) return Boolean is

      function Verify (This : Conditional_Value) return Boolean is
         Contains : Boolean := False;
      begin
         case This.Kind is
            when Value =>
               return False;
            when Condition =>
               Return
                 This.True_Value.Contains_ORs or Else
                 This.False_Value.Contains_ORs;
            when Vector =>
               if This.Conjunction = Ored then
                  return True;
               else
                  for Child of This loop
                     Contains := Contains or else Verify (Child);
                  end loop;
                  return Contains;
               end if;
         end case;
      end Verify;

   begin
      if This.Is_Empty then
         return False;
      else
         return Verify (This);
      end if;
   end Contains_ORs;

   ----------------------
   -- Is_Unconditional --
   ----------------------

   function Is_Unconditional (This : Conditional_Value) return Boolean is

      function Verify (This : Conditional_Value) return Boolean is
         Pass : Boolean := True;
      begin
         case This.Kind is
            when Value =>
               return True;
            when Condition =>
               return False;
            when Vector =>
               for Child of This loop
                  Pass := Pass and then Verify (Child);
               end loop;
               return Pass;
         end case;
      end Verify;

   begin
      return This.Is_Empty or else Verify (This);
   end Is_Unconditional;

   ----------------------
   -- Iterate_Children --
   ----------------------

   procedure Iterate_Children (This    : Conditional_Value;
                               Visitor : access procedure (CV : Conditional_Value))
   is

      procedure Iterate (This : Inner_Node'Class) is
      begin
         case This.Kind is
            when Value | Condition =>
               raise Constraint_Error with "Conditional value is not a vector";
            when Vector =>
               for Inner of Vector_Inner (This).Values loop
                  Visitor (Conditional_Value'(To_Holder (Inner)));
               end loop;
         end case;
      end Iterate;

   begin
      if not This.Is_Empty then
         Iterate (This.Constant_Reference);
      end if;
   end Iterate_Children;

   ---------------------
   -- Case_Statements --
   ---------------------

   package body Case_Statements is

      function Case_Is (Arr : Arrays) return Conditional_Value is
         Case_Is : Conditional_Value := Arr (Arr'Last);
         --  Since we get the whole array,
         --    by exhaustion at worst the last must be true
      begin
         for I in reverse Arr'First .. Enum'Pred (Arr'Last) loop
            Case_Is := New_Conditional (If_X   => Requisite_Equal (I),
                                        Then_X => Arr (I),
                                        Else_X => Case_Is);
         end loop;

         return Case_Is;
      end Case_Is;

   end Case_Statements;

   -----------
   -- Print --
   -----------

   procedure Print (This   : Conditional_Value;
                    Prefix : String := "";
                    And_Or : Boolean := True) is
      use GNAT.IO;
      Tab : constant String := "   ";

--        function Image (C : Conjunctions) return String is
--          (case C is
--              when Anded => "and",
--              when Ored  => "or");

   begin
      if This.Is_Empty then
         Put_Line (Prefix & "(empty)");
         return;
      end if;

      case This.Kind is
         when Value =>
            Put_Line (Prefix & Image (This.Value));
         when Condition =>
            Put_Line (Prefix & "when " & This.Condition.Image & ":");
            Print (This.True_Value, Prefix & Tab);
            if not This.False_Value.Is_Empty then
               Put_Line (Prefix & "else:");
               Print (This.False_Value, Prefix & Tab);
            end if;
         when Vector =>
            if And_Or then
               case This.Conjunction is
                  when Anded => Put_Line (Prefix & "All of:");
                  when Ored  => Put_Line (Prefix & "First available of:");
               end case;
            end if;

            for I in This.Iterate loop
               Print (This (I),
                      (if And_Or then Prefix else "") & "   ");
            end loop;
      end case;
   end Print;

   -----------------
   --  ITERATORS  --
   -----------------

   type Forward_Iterator is new Iterators.Forward_Iterator with record
      Children : Vectors.Vector;
   end record;

   -----------
   -- First --
   -----------

   overriding function First (Object : Forward_Iterator) return Cursor is
     (Cursor (Object.Children.First));

   ----------
   -- Next --
   ----------

   function Next (This : Cursor) return Cursor is
      (Cursor (Vectors.Next (Vectors.Cursor (This))));

   ----------
   -- Next --
   ----------

   overriding function Next (Object   : Forward_Iterator;
                             Position : Cursor) return Cursor is
     (Next (Position));

   -----------------
   -- Has_Element --
   -----------------

   function Has_Element (This : Cursor) return Boolean is
      (Vectors.Has_Element (Vectors.Cursor (This)));

   -------------
   -- Iterate --
   -------------

   function Iterate (Container : Conditional_Value)
                     return Iterators.Forward_Iterator'Class is
   begin
      if Container.Kind /= Vector then
         raise Constraint_Error
           with "Cannot iterate over non-vector conditional value";
      end if;

      return Forward_Iterator'
        (Children =>
           Vector_Inner (Container.Constant_Reference.Element.all).Values);
   end Iterate;

   ---------------------
   -- Indexed_Element --
   ---------------------

   function Indexed_Element (Container : Conditional_Value;
                             Pos       : Cursor)
                             return Conditional_Value is
     (Conditional_Value'(To_Holder (Element (Pos))));

end Alire.Conditional_Values;
