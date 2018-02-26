with Alire.Index.Semantic_Versioning;
with Alire.Index.Simple_Logging;

package Alire.Index.Alire is

   Name : constant Project_Name := "alire";
   Repo : constant URL          := "https://bitbucket.org/aleteolabs/alire.git";

   Desc : constant Project_Description := "Alire project catalog and support files";

   V_0_1 : constant Release :=
              Register (Name,
                        V ("0.1.2"),
                        Desc,
                        Git (Repo, "e2dee2e147ae9e4d666567b53b108cbe61bc06e8"),
                        Depends_On =>
                          Within_Minor (Semantic_Versioning.V_0_1) and
                          Within_Major (Simple_Logging.V_1));

end Alire.Index.Alire;
