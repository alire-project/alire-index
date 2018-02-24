with Alire.Dependencies;
with Alire.Utils;

package body Alire.Query is

   package Semver renames Semantic_Versioning;

   use all type Semver.Version_Set;

   ----------------------
   -- Dependency_Image --
   ----------------------

   function Dependency_Image (Project  : Project_Name;
                              Versions : Semantic_Versioning.Version_Set;
                              Policy   : Policies := Newest) return String is
      (Project &
       (if Versions /= Semver.Any
        then " version " & Semver.Image (Versions)
        else " with " & Utils.To_Mixed_Case (Policy'Img) & " version"));

   ------------
   -- Exists --
   ------------

   function Exists (Project : Project_Name;
                    Allowed : Semantic_Versioning.Version_Set := Semantic_Versioning.Any)
                    return Boolean
   is
      use Semver;
   begin
      for R of Releases loop
         if R.Project = Project and then Satisfies (R.Version, Allowed) then
            return True;
         end if;
      end loop;

      return False;
   end Exists;

   ----------
   -- Find --
   ----------

   function Find (Project : Project_Name;
                  Allowed : Semantic_Versioning.Version_Set := Semantic_Versioning.Any;
                  Policy  : Policies := Newest) return Release
   is
      use Semantic_Versioning;
   begin
      for R of reverse Index.Releases loop
         if R.Project = Project then
            if Satisfies (R.Version, Allowed) then
               return R;
            else
               Trace.Debug ("Skipping unsatisfactory version: " & Image (R.Version));
            end if;
         end if;
      end loop;

      raise Query_Unsuccessful with "Release not found: " & Project;
   end Find;

   ------------
   -- Exists --
   ------------

   function Exists (Project : Project_Name;
                    Version : Semantic_Versioning.Version)
                    return Boolean is
      (Exists (Project, Semver.Exactly (Version)));

   ----------
   -- Find --
   ----------

   function Find (Project : Project_Name;
                  Version : Semantic_Versioning.Version) return Release is
      (Find (Project, Semver.Exactly (Version)));

   --------------------
   -- Print_Solution --
   --------------------

   procedure Print_Solution (I : Instance) is
      use Containers.Project_Release_Maps;
   begin
      for Rel of I loop
         Log ("  " & Rel.Milestone_Image, Debug);
      end loop;
   end Print_Solution;

   ----------
   -- Fail --
   ----------

   function Fail return Instance is (Containers.Project_Release_Maps.Empty_Map);

   -------------
   -- Resolve --
   -------------

   function Resolve (Unresolved :     Index.Dependencies;
                     Frozen     :     Instance;
                     Success    : out Boolean) return Instance
   is
   --  FIXME: since this is depth-first, Frozen can be passed in-out and updated on the spot,
   --  thus saving copies. Probably the same applies to Unresolved.
      Dep    : constant Alire.Dependencies.Dependency := Unresolved.First_Element;
      Remain :          Index.Dependencies            := Unresolved;

      ---------------
      -- Go_Deeper --
      ---------------

      function Go_Deeper (Unresolved : Index.Dependencies;
                          Frozen     : Instance) return Instance
      is
      begin
         if Unresolved.Is_Empty then
            Log ("Dependencies resolved", Detail);
            Print_Solution (Frozen);
            return Frozen;
         else
            return Resolve (Unresolved, Frozen, Success);
         end if;
      end Go_Deeper;

   begin

      Remain.Delete_First;

      if Frozen.Contains (Dep.Project) then
         if Semver.Satisfies (Frozen.Element (Dep.Project).Version, Dep.Versions) then
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
            if Dep.Project = R.Project and then Semver.Satisfies (R.Version, Dep.Versions) then
               declare
                  New_Frozen : Instance           := Frozen;
                  New_Remain : Index.Dependencies := Remain;

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

   function Resolve (Deps    :     Index.Dependencies;
                     Success : out Boolean;
                     Policy  :     Policies := Newest) return Instance is
   begin
      Success := False;

      if Deps.Is_Empty then
         Success := True;
         return Empty_Instance;
      end if;

      return I : constant Instance := Resolve (Deps, Containers.Project_Release_Maps.Empty_Map, Success) do
         if not Success then
            Log ("Dependency resolution failed");
         end if;
      end return;
   end Resolve;

end Alire.Query;
