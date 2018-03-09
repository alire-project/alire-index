with Alire.Index.GtkAda;

package Alire.Index.Eagle_Lander is

   Prj_Repo : constant URL                 := "https://github.com/alire-project/eagle-lander.git";

   Prj_Author     : constant String := "Fabien Chouteau";
   Prj_Website    : constant URL    := "https://blog.adacore.com/make-with-ada-the-eagle-has-landed";

   V_1_0 : constant Release :=
             Register (Projects.Eagle_Lander,
                       V ("1.0"),
                       Git (Prj_Repo, "5a3bcc61eff4d60d2b741add7841410ce539d0b8"),

                       Dependencies       =>
                         Within_Major (Alire.Index.GtkAda.V_16_1),

                       Properties         =>
                         Project_File ("lunar_lander.gpr") and

                         Author     (Prj_Author) and
                         Website    (Prj_Website) and
                         License    (GPL_3_0),

                       Private_Properties =>
                         GPR_File ("lunar_lander.gpr")
                      );

end Alire.Index.Eagle_Lander;
