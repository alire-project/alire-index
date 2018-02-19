with Alire.Index.Semantic_Versioning;
with Alire.Index.Simple_Logging;

package Alire.Index.Alire is

   Name : constant Project_Name := "alire";
   Repo : constant URL          := "https://bitbucket.org/aleteolabs/alire.git";

   Desc : constant Project_Description := "Alire project catalog and support files";

   Latest : constant Release :=
               Register_Git (Name,
                             V ("0.1.0"),
                             Desc,
                             Repo,
                             "ce78e7706c9d3f97605df48d8befca5407f8d328",
                             Depends_On =>
                               Within_Major (Semantic_Versioning.Latest) and
                               Within_Major (Simple_Logging.V_1_0_0));

end Alire.Index.Alire;
