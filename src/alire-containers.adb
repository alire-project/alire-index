package body Alire.Containers is

   ------------
   -- Insert --
   ------------

   procedure Insert (Dst : in out Release_Map; Src : Release_Map) is
   begin
      for E of Src loop
         Dst.Insert (E.Project, E);
      end loop;
   end Insert;

   ---------------
   -- Inserting --
   ---------------

   function Inserting (Dst : Release_Map; Src : Release_Map) return Release_Map is
   begin
      return Result : Release_Map := Dst do
         for E of Src loop
            Result.Insert (E.Project, E);
         end loop;
      end return;
   end Inserting;

   function Inserting (Dst : Release_Map; Src : Releases.Release) return Release_Map is
      (Dst.Inserting (To_Map (Src)));

   ---------------
   -- Excluding --
   ---------------

   function Excluding (Map : Release_Map; Name : Alire.Project) return Release_Map is
   begin
      return Filtered : Release_Map := Map do
         Filtered.Exclude (Name);
      end return;
   end Excluding;

   ---------------
   -- Including --
   ---------------

   function Including (Map : Release_Map; Release : Releases.Release) return Release_Map is
   begin
      return New_Map : Release_Map := Map do
         New_Map.Include (Release.Project, Release);
      end return;
   end Including;

   ---------------------
   -- To_Dependencies --
   ---------------------

   function To_Dependencies (Map : Release_Map) return Conditional.Dependencies is
      use Conditional.For_Dependencies;
   begin
      return Deps : Conditional.Dependencies do
         for R of Map loop
            Deps :=
              Deps and
              Conditional.New_Dependency
                (R.Project,
                 Semantic_Versioning.Exactly (R.Version));
         end loop;
      end return;
   end To_Dependencies;

   ------------
   -- To_Map --
   ------------

   function To_Map (R : Releases.Release) return Release_Map is
   begin
      return M : Release_Map do
         M.Include (R.Project, R);
      end return;
   end To_Map;

end Alire.Containers;
