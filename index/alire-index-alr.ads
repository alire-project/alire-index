with Alire.Index.Alire;
with Alire.Index.Simple_Logging;

package Alire.Index.Alr is

   function Project is new Catalogued_Project (Projects.Alr);

   Repo : constant URL := "https://github.com/alire-project/alr.git";

   V_0_2 : constant Release :=
               Project.Register
                 (V ("0.2"),
                  Git (Repo, "77f7f2fc3f55e6f4ce3425c70c208420ca4db373"),
                  Dependencies =>
                    Alire.V_0_2.Within_Minor and
                    Simple_Logging.V_1_0.Within_Major);

   V_0_1_2 : constant Release :=
               Project.Register
                 (V ("0.1.2"),
                  Git (Repo, "4002536beea8aee12b455077df4dd144b409bde4"),
                  Dependencies =>
                    Alire.V_0_1_2.Within_Minor and
                    Simple_Logging.V_1_0.Within_Major);

end Alire.Index.Alr;
