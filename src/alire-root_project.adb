with Alire.Properties;
with Alire.Requisites;
with Alire.Repositories.Local;

package body Alire.Root_Project is

   ----------------------
   -- Set_Root_Project --
   ----------------------

   function Set (Project    : Project_Name;
                 Version    : Semantic_Versioning.Version;
                 Depends_On : Dependencies.Vectors.Vector := Dependencies.Vectors.No_Dependencies)
                 return Releases.Release
   is
      Rel : constant Releases.Release :=
              Alire.Releases.New_Release (Project,
                                          "working copy of " & Project, -- FIXME might be too long
                                          Version,
                                          Alire.Repositories.Local.Repo,
                                          "filesystem",
                                          Depends_On,
                                          Properties => Properties.No_Properties,
                                          Requisites => Requisites.No_Requisites,
                                          Native     => False);
   begin
      Root_Project.Current.Replace_Element (Rel);

      return Rel;
   end Set;

end Alire.Root_Project;
