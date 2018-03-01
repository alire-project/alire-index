package Alire.Index.Semantic_Versioning is

   Name : constant Project_Name := "semantic_versioning";
   Repo : constant URL          := "https://bitbucket.org/aleteolabs/semver.git";

   Desc : constant Project_Description := "Semantic Versioning for Ada";

   V_0_1 : constant Release :=
              Register (Name,
                        V ("0.1.2"),
                        Desc,
                        Git (Repo, "09774d80fac62ea3a09d46b22d4807da530387e2"));

end Alire.Index.Semantic_Versioning;
