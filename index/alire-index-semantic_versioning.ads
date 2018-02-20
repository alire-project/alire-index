package Alire.Index.Semantic_Versioning is

   Name : constant Project_Name := "semantic_versioning";
   Repo : constant URL          := "https://bitbucket.org/aleteolabs/semver.git";

   Desc : constant Project_Description := "Semantic Versioning for Ada";

   Latest : constant Release :=
              Register (Name,
                        V ("0.1.1"),
                        Desc,
                        Git (Repo, "c25fb63017b098e8c696f9530e1128fb33948fd5"));

end Alire.Index.Semantic_Versioning;
