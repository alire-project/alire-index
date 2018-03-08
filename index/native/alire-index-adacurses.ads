with Alire.Index.NcursesAda;

package Alire.Index.Adacurses is

   Name : constant String := "adacurses";
   Desc : constant String := "Wrapper on different packagings of NcursesAda";
   Repo : constant String := "https://github.com/alire-project/adacurses-wrapper.git";

   V_6 : constant Release :=
           Register (Name,
                   V ("6.0+20170708-2"),
                   Desc,
                   Git (Repo, "4ccb20409becb50c0b5fd29effb676b650608326"),

                   Dependencies =>
                     On_Condition
                       (Distribution_Is (Debian) or Distribution_Is (Ubuntu),
                        When_True  => When_Available (Within_Major (NcursesAda.V_5)),
                        When_False => Unavailable),

                   Properties =>
                     Comment ("AdaCurses is the project name used by upstream, thus adacurses.gpr") and
                     Comment ("However, some distros (e.g., Debian family) use ncursesada.gpr") and
                     Comment ("This package wraps these differences so clients can always safely use adacurses")
                  );

end Alire.Index.Adacurses;
