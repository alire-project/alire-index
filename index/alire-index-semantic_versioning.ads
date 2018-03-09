package Alire.Index.Semantic_Versioning is

   Repo : constant URL := "https://bitbucket.org/aleteolabs/semver.git";

   V_0_1_2 : constant Release :=
              Register (Projects.Semantic_Versioning,
                        V ("0.1.2"),
                        Git (Repo, "09774d80fac62ea3a09d46b22d4807da530387e2"));

end Alire.Index.Semantic_Versioning;
