with Alire.Index.Semantic_Versioning;
with Alire.Index.Simple_Logging;

package Alire.Index.Alire is

   Name : constant Project_Name := "alire";
   Repo : constant URL          := "https://bitbucket.org/aleteolabs/alire.git";

   Desc : constant Project_Description := "Alire project catalog and support files";

   Latest : constant Release :=
              Register (Name,
                        V ("0.1.1"),
                        Desc,
                        Git (Repo, "cc3aaa2010a5a4a0033479bd4385e990c125f813"),
                        Depends_On =>
                          Within_Major (Semantic_Versioning.Latest) and
                          Within_Major (Simple_Logging.V_1));

end Alire.Index.Alire;
