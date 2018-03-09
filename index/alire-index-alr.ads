with Alire.Index.Alire;
with Alire.Index.Simple_Logging;

package Alire.Index.Alr is

   Repo : constant URL          := "https://bitbucket.org/aleteolabs/alr.git";

   V_0_1_2 : constant Release :=
              Register (Projects.Alr,
                        V ("0.1.2"),
                        Git (Repo, "4002536beea8aee12b455077df4dd144b409bde4"),
                        Dependencies =>
                          Within_Major (Alire.V_0_1_2) and
                          Within_Major (Simple_Logging.V_1_0_0));

end Alire.Index.Alr;
