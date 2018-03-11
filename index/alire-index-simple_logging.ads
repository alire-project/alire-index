package Alire.Index.Simple_Logging is

   function Project is new Catalogued_Project (Projects.Simple_Logging);

   Repo : constant URL          := "https://github.com/mosteo/simple_logging.git";

   V_1_0_0 : constant Release :=
               Project.Register (V ("1.0.0"),
                                 Git (Repo, "d98242b8bd1c7f964cebc454e9b1206ffdbb0ca9"));

end Alire.Index.Simple_Logging;
