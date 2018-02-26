
with Ada.Directories;

with Alire.Containers;
with Alire.Origins;
with Alire.Properties;
with Alire.Query;
with Alire.Requisites;

package body Alire.Root_Project is

   Root : Alire.Containers.Release_H;
   --  Root dependency (the working project). If Is_Empty we know we must recompile,
   --  unless the hash already matches. In this case, we know the project file is
   --  missing the Set_Root_Project call

   -------------
   -- Current --
   -------------

   function Current return Releases.Release is (Root.Element);

   ------------
   -- Is_Set --
   ------------

   function Is_Set return Boolean is
      (not Root.Is_Empty);

   ----------------------
   -- Set_Root_Project --
   ----------------------

   function Set (Project    : Project_Name;
                 Version    : Semantic_Versioning.Version;
                 Depends_On : Dependencies.Vectors.Vector := Dependencies.Vectors.No_Dependencies)
                 return Releases.Release
   is
      use Origins;

      Descr : constant String := "working copy of " & Project;
      Rel : constant Releases.Release :=
              Alire.Releases.New_Release (Project,
                                          Descr (Descr'First .. Descr'First - 1 +
                                              Natural'Min (Descr'Length, Max_Description_Length)),
                                          Version,
                                          New_Filesystem (Ada.Directories.Current_Directory),
                                          Depends_On,
                                          Properties => Properties.No_Properties,
                                          Requisites => Requisites.No_Requisites,
                                          Native     => False);
   begin
      if Query.Exists (Project, Version) then
         --  This is done to ensure that properties are all available
         Trace.Debug ("Storing pre-indexed release of root project");
         Root.Replace_Element (Query.Find (Project, Version));
      else
         Trace.Debug ("Storing unindexed release of root project");
         Root.Replace_Element (Rel);
      end if;

      return Rel;
   end Set;

end Alire.Root_Project;
