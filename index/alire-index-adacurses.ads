with Alire.Index.NcursesAda;

package Alire.Index.Adacurses is

   Repo : constant String := "https://github.com/alire-project/adacurses-wrapper.git";

   Comments : constant Conditional.Properties :=
                Comment ("AdaCurses is the project name used by upstream, thus adacurses.gpr") and
                Comment ("However, some distros (e.g., Debian family) use ncursesada.gpr") and
                Comment ("This package wraps these differences so clients can always safely use adacurses");

   V_6 : constant Release :=
           Register (Projects.Adacurses,
                   V ("6.0+20170708-2"),
                   Git (Repo, "4ccb20409becb50c0b5fd29effb676b650608326"),

                   Dependencies =>
                     On_Condition
                       (Distribution = Debian or Distribution = Ubuntu,
                        When_True  => When_Available (Within_Major (NcursesAda.V_5)),
                        When_False => Unavailable),

                   Properties =>
                       Comments
                    );

   V_5_9 : constant Release :=
           Register (Projects.Adacurses,
                     V ("5.9+20140726-1build1"),
                     Git (Repo, "4ccb20409becb50c0b5fd29effb676b650608326"),

                     Dependencies =>
                       On_Condition
                         (Distribution = Debian or Distribution = Ubuntu,
                          When_True  => When_Available (Within_Major (NcursesAda.V_3)),
                          When_False => Unavailable),

                     Properties   =>
                       Comments
                    );

end Alire.Index.Adacurses;
