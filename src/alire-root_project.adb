
with Ada.Directories;

with Alire.Containers;
with Alire.Origins;
with Alire.Properties;
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

      Rel : constant Releases.Release :=
              Alire.Releases.New_Release (Project,
                                          "working copy of " & Project, -- FIXME might be too long
                                          Version,
                                          New_Filesystem (Ada.Directories.Current_Directory),
                                          Depends_On,
                                          Properties => Properties.No_Properties,
                                          Requisites => Requisites.No_Requisites,
                                          Native     => False);
   begin
      Root.Replace_Element (Rel);

      return Rel;
   end Set;

end Alire.Root_Project;
