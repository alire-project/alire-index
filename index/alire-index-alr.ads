with Alire.Index.Alire;
with Alire.Index.Simple_Logging;

package Alire.Index.Alr is

   function Project is new Catalogued_Project ("alr",
                                               "Command-line tool from the Alire project");

   Repo : constant URL := "https://github.com/alire-project/alr.git";

   V_0_1_2 : constant Release :=
               Project.Register
                 (V ("0.1.2"),
                  Git (Repo, "4002536beea8aee12b455077df4dd144b409bde4"),
                  Dependencies =>
                    Alire.V_0_1_2.Within_Minor and
                    Simple_Logging.V_1_0.Within_Major,

                  Properties   =>
                    Author ("Alejandro R. Mosteo") and
                    License (GPL_3_0));

   V_0_2   : constant Release :=
               Project.Register
                 (V_0_1_2.Upgrading
                    (V ("0.2"),
                     Git (Repo, "481a22aceb07242cabaefedbb41b2d6fe7a8bd1e")));

end Alire.Index.Alr;
