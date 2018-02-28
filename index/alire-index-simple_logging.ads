package Alire.Index.Simple_Logging is

   Name : constant Project_Name := "simple_logging";
   Repo : constant URL          := "https://github.com/mosteo/simple_logging.git";

   Desc : constant Project_Description := "Simple logging to console";

   V_1 : constant Release :=
           Register (Name,
                     V ("1.0.0"),
                     Desc,
                     Git (Repo, "d98242b8bd1c7f964cebc454e9b1206ffdbb0ca9"));

end Alire.Index.Simple_Logging;
