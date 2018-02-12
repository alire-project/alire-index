package body Alire.Query is

   ------------
   -- Exists --
   ------------

   function Exists (Project : Project_Name) return Boolean is
   begin
      for R of Releases loop
         if R.Project = Project then
            return True;
         end if;
      end loop;

      return False;
   end Exists;

   --------------------
   -- Print_Solution --
   --------------------

   procedure Print_Solution (I : Instance) is
      use Containers.Project_Release_Maps;
   begin
      for Rel of I loop
         Log ("  " & Rel.Milestone_Image, Detail);
      end loop;
   end Print_Solution;

   ----------
   -- Fail --
   ----------

   function Fail return Instance is (Containers.Project_Release_Maps.Empty_Map);

   -------------
   -- Resolve --
   -------------

   function Resolve (Unresolved : Dependencies;
                     Frozen     : Instance;
                     Success    : out Boolean) return Instance
   is
   --  FIXME: since this is depth-first, Frozen can be passed in-out and updated on the spot,
   --  thus saving copies. Probably the same applies to Unresolved.
      Dep    : constant Dependency   := Unresolved.First_Element;
      Remain :          Dependencies := Unresolved;

      ---------------
      -- Go_Deeper --
      ---------------

      function Go_Deeper (Unresolved : Dependencies;
                          Frozen     : Instance) return Instance
      is
      begin
         if Unresolved.Is_Empty then
            Log ("Dependencies resolved");
            Print_Solution (Frozen);
            return Frozen;
         else
            return Resolve (Unresolved, Frozen, Success);
         end if;
      end Go_Deeper;

   begin

      Remain.Delete_First;

      if Frozen.Contains (Dep.Project) then
         if Satisfies (Frozen.Element (Dep.Project).Version, Dep.Versions) then
            --  Dependency already met, simply go down...
            return Go_Deeper (Remain, Frozen);
         else
            --  Failure because an already frozen version is incompatible
            return Fail;
         end if;
      else
         -- Need to check all versions for the first one...
         -- FIXME: complexity can be improved not visiting blindly all releases to match by project
         for R of reverse Index.Releases loop
            if Dep.Project = R.Project and then Satisfies (R.Version, Dep.Versions) then
               declare
                  New_Frozen : Instance     := Frozen;
                  New_Remain : Dependencies := Remain;

                  Solution   : Instance;
               begin
                  New_Frozen.Insert (R.Project, R);
                  New_Remain.Append (R.Depends);

                  Solution := Go_Deeper (New_Remain, New_Frozen);

                  if not Solution.Is_Empty then
                     Success := True;
                     return Solution; -- Success!
                  end if;
               end;
            end if;
         end loop;

         --  We found no milestone compatible with the first unresolved dependency...
         return Fail;
      end if;
   end Resolve;

   -------------
   -- Resolve --
   -------------

   function Resolve (Deps    : Dependencies;
                     Success : out Boolean) return Instance is
   begin
      Success := False;

      return I : constant Instance := Resolve (Deps, Containers.Project_Release_Maps.Empty_Map, Success) do
         if not Success then
            Log ("Dependency resolution failed");
         end if;
      end return;
   end Resolve;

end Alire.Query;
