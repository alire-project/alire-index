with Alire.Index;

package body Alire.Root is

   Root : access Roots.Root;
   --  Root dependency (the working project). If Is_Empty we know we must recompile,
   --  unless the hash already matches. In this case, we know the project file is
   --  missing the Set call

   -------------
   -- Current --
   -------------

   function Current return Roots.Root is (Root.all);

   ------------
   -- Is_Set --
   ------------

   function Is_Set return Boolean is (Root /= null);

   ---------
   -- Set --
   ---------

   function Set (Project      : Projects.Names;
                 Version      : Semantic_Versioning.Version)
                 return Roots.Root
   is
   begin
      Root := new Roots.Root'(Roots.New_Root (Index.Find (Projects.Image (Project), Version)));
      Trace.Debug ("Storing indexed release as root: " & Root.Release.Milestone.Image);
      return Root.all;
   end Set;

   ---------
   -- Set --
   ---------

   function Set (Project      : Name_String;
                 Dependencies : Conditional.Dependencies)
                 return Roots.Root
   is
   begin
      Trace.Debug ("Storing unindexed project as root:" & Project);
      Root := new Roots.Root'(Roots.New_Root (Project, Dependencies));
      return Root.all;
   end Set;

end Alire.Root;
