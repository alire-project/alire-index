package Alire.Index.Semantic_Versioning is

   Name : constant Project_Name := "semantic_versioning";
   Repo : constant URL          := "https://bitbucket.org/aleteolabs/semver.git";

   Desc : constant Project_Description := "Semantic Versioning for Ada";

   Latest : constant Release :=
              Register (Name,
                        V ("0.1.0"),
                        Desc,
                        Git (Repo, "9f35b00a31861ea96085ee553fb6335d74831f5c"));

end Alire.Index.Semantic_Versioning;
