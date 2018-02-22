with Alire.Index.Alire;
with Alire.Index.Simple_Logging;

package Alire.Index.Alr is

   Name : constant Project_Name := "alr";
   Repo : constant URL          := "https://bitbucket.org/aleteolabs/alr.git";

   Desc : constant Project_Description := "Command-line tool from the Alire project";

   Latest : constant Release :=
              Register (Name,
                        V ("0.1.1"),
                        Desc,
                        Git (Repo, "4002536beea8aee12b455077df4dd144b409bde4"),
                        Depends_On =>
                          Within_Major (Alire.Latest) and
                          Within_Major (Simple_Logging.V_1));

end Alire.Index.Alr;
