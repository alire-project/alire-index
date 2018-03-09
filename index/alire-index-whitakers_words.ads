package Alire.Index.Whitakers_Words is

   Prj_Repo       : constant URL    := "https://github.com/mk270/whitakers-words.git";
   Prj_Author     : constant String := "William A. Whitaker";
   Prj_Maintainer : constant String := "Martin Keegan";
   Prj_Website    : constant URL    := "http://mk270.github.io/whitakers-words/";

   V_2017_09_10 : constant Release :=
                    Register (Projects.Whitakers_Words,
                              V ("2017.09.10"),
                              Git (Prj_Repo, "27be95b8a06d7b22c0600c824cf929ab43efcf25"),
                              Properties         =>
                                Project_File ("words.gpr") and

                                Executable ("words") and

                                Author     (Prj_Author) and
                                Maintainer (Prj_Maintainer) and
                                Website    (Prj_Website) and
                                License    (Public_Domain) and

                                Comment ("This package builds the binary but additional steps are needed") and
                                Comment ("See the README file for further instructions"),

                              Private_Properties =>
                                GPR_File ("words.gpr"),

                              Available_When     =>
                                Compiler > GNAT_FSF_7_2 -- bug with SAL library failing binding
                             );

end Alire.Index.Whitakers_Words;
