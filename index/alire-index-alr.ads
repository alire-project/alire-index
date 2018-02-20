with Alire.Index.Alire;
with Alire.Index.Simple_Logging;

package Alire.Index.Alr is

   Name : constant Project_Name := "alr";
   Repo : constant URL          := "https://bitbucket.org/aleteolabs/alr.git";

   Desc : constant Project_Description := "Command-line tool from the Alire project";

   Latest : constant Release :=
               Register_Git (Name,
                             V ("0.1.0"),
                             Desc,
                             Repo,
                             "10a98e9d14ea129271e5de9a3a4e46a14610fbc2",
                             Depends_On =>
                               Within_Major (Alire.Latest) and
                               Within_Major (Simple_Logging.V_1));

end Alire.Index.Alr;
