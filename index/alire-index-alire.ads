with Alire.Index.Semantic_Versioning;
with Alire.Index.Simple_Logging;

package Alire.Index.Alire is

   Name : constant Project_Name := "alire";
   Repo : constant URL          := "https://bitbucket.org/aleteolabs/alire.git";

   Desc : constant Project_Description := "Alire project catalog and support files";

   V_0_1_2 : constant Release :=
              Register (Name,
                        V ("0.1.2"),
                        Desc,
                        Git (Repo, "e2dee2e147ae9e4d666567b53b108cbe61bc06e8"),
                        Dependencies =>
                          Within_Minor (Semantic_Versioning.V_0_1_2) and
                          Within_Major (Simple_Logging.V_1_0_0)
                       );


   Syntax_Example : constant Release :=
                Register ("alire_example",
                          V ("1.0.0"),
                          "Release with all index syntax features",
                          Origins.New_Filesystem ("/fake"),

                          Dependencies =>
                            Current ("half_life_3") and -- unconditional
                            On_Condition                -- conditional
                              (System_Is (GNU_Linux),
                               When_True => At_Least ("elite_horizons", V ("2.0")) and
                                            At_Least ("star_citizen", V ("3.0")), -- Wish...
                               When_False => At_Least ("windows_100", V ("1.0"))) and
                            When_Available -- Chained preferences
                              (Preferred => Within_Major ("alire", V ("1.0")),
                               Otherwise => Within_Major ("alire", V ("0.0"))) and
                          When_Available -- Chained preferences
                            (Preferred => Within_Major ("alire", V ("2.0")),
                             Otherwise => When_Available -- Chained preferences multi-level
                                            (Preferred => Within_Major ("alire_alt", V ("1.0")),
                                             Otherwise => Within_Major ("alire", V ("0.5")))),

                          Alr_Properties => -- These are only interesting to alr, not users
                            GPR_External ("Profile", "False"),
                            --  Sample extra params for build

                          Properties   =>
                            GPR_Scenario ("Build", "Debug" or "Release") and
                            GPR_Free_Scenario ("Path_To_Something") and
                            --  Known scenario variables

                            GPR_File ("scenarios/catastrophical.gpr") and
                            --  Way to specify a project file not named like the project
                            --  Path separators are always "/" and internally converted to native ones

                            On_Condition
                              (System_Is (Windows),
                               GPR_File ("project_win.gpr")) and
                            On_Condition
                              (System_Is (GNU_Linux),
                               On_Condition (Distribution_Is (Ubuntu), -- Nested conditions
                                            GPR_File ("project_ubuntu.gpr"))) and
                            --  Conditional project file

                            On_Condition
                              (System_Is (GNU_Linux),
                               Comment ("Long life the penguin")) and
                            --  Conditions on operating system

                            On_Condition
                              (not Compiler_Is (GNAT_Unknown),
                               Comment ("Never saw that compiler") and Comment ("But I would like to")) and
                            --  Conditions on compiler version

                            On_Condition
                              (Version_Is (Ubuntu_Artful),
                               When_True  => Comment ("Living on the edge"),
                               When_False => Comment ("I am a rock")) and
                            --  Conditions on distribution release

                            Comment ("Tell me about your mother") and
                            Website ("http://www.www.www"),
                            --  Unconditional properties

                          Available_When => -- Impossible mix
                            (System_Is (Windows) and System_Is (GNU_Linux)) or
                            (Compiler_Is (GNAT_Unknown) and not Compiler_Is (GNAT_Unknown)));

end Alire.Index.Alire;
