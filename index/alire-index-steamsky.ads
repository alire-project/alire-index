with Alire.Index.Adacurses;
with Alire.Index.GtkAda;

package Alire.Index.Steamsky is

   Prj_Repo : constant URL := "https://github.com/thindil/steamsky.git";

   Prj_Author     : constant String := "Bartek Jasicki";
   Prj_Website    : constant URL    := "https://thindil.github.io/steamsky/";

   V_2_1_Dev : constant Release :=
               Register (Projects.Steamsky,
                         V ("2.1-dev"),
                         Git (Prj_Repo, "faee42a72506f2522d3db9c714d0b0a001c0032d"),

                         Notes => "GtkAda version",

                         Dependencies =>
                           Within_Major (Alire.Index.GtkAda.V_16_1),

                         Properties   =>
                           Author     (Prj_Author) and
                           Website    (Prj_Website) and
                           License    (GPL_3_0)
                        );

   V_2_0_2 : constant Release :=
               Register (Projects.Steamsky,
                         V ("2.0.2"),
                         Git (Prj_Repo, "8b4dd319c3199f4b770e39de13f9ef3d9020266f"),

                         Notes => "Console (ncurses) version",

                         Dependencies =>
                           Within_Major (Alire.Index.Adacurses.V_6),

                         Properties =>
                           Author     (Prj_Author) and
                           Website    (Prj_Website) and
                           License    (GPL_3_0)
                        );

end Alire.Index.Steamsky;
