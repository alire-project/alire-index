package body Alire.Index.Query is

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

   procedure Print_Solution (Solution : Containers.Version_Map) is
      use Containers.Project_Version_Maps;
   begin
      for I in Solution.Iterate loop
         Log ("  " & Key (I) & "=" & Image (Element (I)));
      end loop;
   end Print_Solution;

   -------------
   -- Resolve --
   -------------

   function Fail return Containers.Version_Map is (Containers.Project_Version_Maps.Empty_Map);

   function Resolve (Unresolved : Dependencies;
                     Frozen     : Containers.Version_Map) return Containers.Version_Map
   is
   --  FIXME: since this is depth-first, Frozen can be passed in-out and updated on the spot,
   --  thus saving copies. Probably the same applies to Unresolved.
      Dep    : constant Dependency   := Unresolved.First_Element;
      Remain :          Dependencies := Unresolved;

      function Go_Deeper (Unresolved : Dependencies;
                          Frozen     : Containers.Version_Map) return Containers.Version_Map
      is
      begin
         if Unresolved.Is_Empty then
            Log ("Dependency solution found.");
            Print_Solution (Frozen);
            return Frozen;
         else
            return Resolve (Unresolved, Frozen);
         end if;
      end Go_Deeper;

   begin
      Remain.Delete_First;

      if Frozen.Contains (Dep.Project) then
         if Satisfies (Frozen.Element (Dep.Project), Dep.Versions) then
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
                  New_Frozen : Containers.Version_Map := Frozen;
                  New_Remain : Dependencies           := Remain;

                  Solution : Containers.Version_Map;
               begin
                  New_Frozen.Insert (R.Project, R.Version);
                  New_Remain.Append (R.Depends);

                  Solution := Go_Deeper (New_Remain, New_Frozen);

                  if not Solution.Is_Empty then
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

   function Resolve (Deps : Dependencies) return Containers.Version_Map is
   begin
      return Resolve (Deps, Containers.Project_Version_Maps.Empty_Map);
   end Resolve;

end Alire.Index.Query;
