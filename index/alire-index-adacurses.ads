with Alire.Index.NcursesAda;

package Alire.Index.Adacurses is

   function Project is
     new Catalogued_Project ("Wrapper on different packagings of NcursesAda");

   Repo : constant String := "https://github.com/alire-project/adacurses-wrapper.git";

   Comments : constant Conditional.Properties :=
                Comment ("AdaCurses is the project name used by upstream, thus adacurses.gpr") and
                Comment ("However, some distros (e.g., Debian family) use ncursesada.gpr") and
                Comment ("This package wraps these differences so clients can always safely use adacurses");

   Base : constant Release := Project.Unreleased
     (Properties => Comments);

   package V_6 is new Released
     (Base
      .Replacing
        (Git (Repo, "4ccb20409becb50c0b5fd29effb676b650608326"))
      .Extending
        (Case_Distribution_Is
             ((Debian | Ubuntu => NcursesAda.V_6.Within_Major,
               others          => Unavailable))));

   package V_5 is new Released
     (Base
      .Replacing
        (Git (Repo, "4ccb20409becb50c0b5fd29effb676b650608326"))
      .Extending
        (Case_Distribution_Is
             ((Debian | Ubuntu => NcursesAda.V_5.Within_Major,
               others          => Unavailable))));

end Alire.Index.Adacurses;
