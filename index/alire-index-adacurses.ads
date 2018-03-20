with Alire.Index.NcursesAda;

package Alire.Index.Adacurses is

   function Project is new Catalogued_Project (Projects.Adacurses);

   Repo : constant String := "https://github.com/alire-project/adacurses-wrapper.git";

   Comments : constant Conditional.Properties :=
                Comment ("AdaCurses is the project name used by upstream, thus adacurses.gpr") and
                Comment ("However, some distros (e.g., Debian family) use ncursesada.gpr") and
                Comment ("This package wraps these differences so clients can always safely use adacurses");

   V_6 : constant Release :=
           Project.Register
             (V ("6"),
              Git (Repo, "4ccb20409becb50c0b5fd29effb676b650608326"),

              Dependencies =>
                On_Condition
                  (Distribution = Debian or Distribution = Ubuntu,
                   When_True  => When_Available (NcursesAda.V_6.Within_Major),
                   When_False => Unavailable),

              Properties   =>
                Comments
             );

   V_5 : constant Release :=
             Project.Register
               (V ("5"),
                Git (Repo, "4ccb20409becb50c0b5fd29effb676b650608326"),

                Dependencies =>
                  On_Condition
                    (Distribution = Debian or Distribution = Ubuntu,
                     When_True  => When_Available (NcursesAda.V_5.Within_Major),
                     When_False => Unavailable),

                Properties   =>
                  Comments
               );

end Alire.Index.Adacurses;
