with Alire.Index.Alire;
with Alire.Index.Simple_Logging;
with Alire.Index.XML_EZ_Out;

package Alire.Index.Alr is

   function Project is
     new Catalogued_Project ("Command-line tool from the Alire project");

   Repo : constant URL := "https://github.com/alire-project/alr.git";

   V_0 : constant Release :=
               Project.Bypass
                 (V ("0.0"),
                  No_Origin,
                  Dependencies =>
                      Simple_Logging.V_1_0.Within_Major,

                  Properties   =>
                    Author ("Alejandro R. Mosteo") and
                    License (GPL_3_0));

   V_0_4 : constant Release :=
               Project.Register
                 (V_0
                  .Upgrading
                    (V ("0.4"),
                     Git (Repo, "721d111225cf30b9c298ff23587664510f4c4ea1"))
                  .Extending
                    (Dependencies =>
                       Alire.V_0_4.Within_Minor and
                         XML_EZ_Out.V_1_6.Within_Minor));

   V_0_2 : constant Release :=
               Project.Register
                 (V_0
                  .Upgrading
                    (V ("0.2"),
                     Git (Repo, "481a22aceb07242cabaefedbb41b2d6fe7a8bd1e"))
                  .Extending
                    (Dependencies =>
                       Alire.V_0_2.Within_Minor));

   V_0_1_2 : constant Release :=
               Project.Register
                 (V_0
                  .Upgrading
                    (V ("0.1.2"),
                     Git (Repo, "4002536beea8aee12b455077df4dd144b409bde4"))
                  .Extending
                    (Dependencies =>
                       Alire.V_0_1_2.Within_Minor));

end Alire.Index.Alr;
