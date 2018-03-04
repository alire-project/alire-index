package Alire.Index.Whitakers_Words is

   --  A Template with common fields ready to save-as

   Prj_Name : constant Project_Name        := "whitakers_words";
   Prj_Desc : constant Project_Description := "William Whitaker's WORDS, a Latin dictionary";
   Prj_Repo : constant URL                 := "https://github.com/mk270/whitakers-words.git";

   Prj_Author     : constant String := "William A. Whitaker";
   Prj_Maintainer : constant String := "Martin Keegan";
   Prj_Website    : constant URL    := "http://mk270.github.io/whitakers-words/";

   V_0_0_0 : constant Release :=
               Register (Prj_Name,
                         V ("2017.09.10"),
                         Prj_Desc,
                         Git (Prj_Repo, "27be95b8a06d7b22c0600c824cf929ab43efcf25"),
                         Properties =>
                           GPR_File ("words.gpr") and

                           Executable ("words") and

                           Author     (Prj_Author) and
                           Maintainer (Prj_Maintainer) and
                           Website    (Prj_Website) and
                           License    (Public_Domain) and

                           Comment ("This package builds the binary but additional steps are needed") and
                           Comment ("See the README file for further instructions")
                        );

end Alire.Index.Whitakers_Words;
