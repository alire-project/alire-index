with Alire.Index.Semantic_Versioning;
with Alire.Index.Simple_Logging;

package Alire.Index.Alire is

   function Project is new Catalogued_Project (Projects.Alire);

   Repo : constant URL := "https://bitbucket.org/aleteolabs/alire.git";

   V_0_2 : constant Release :=
             Project.Register
               (V ("0.2"),
                Git (Repo, "dfddd0764a58a28bc06508f7b5454533f3fe6386"),
                Dependencies =>
                  Semantic_Versioning.V_0_1_2.Within_Minor and
                    Simple_Logging.V_1_0.Within_Major);

   V_0_1_2 : constant Release :=
               Project.Register
                 (V ("0.1.2"),
                  Git (Repo, "e2dee2e147ae9e4d666567b53b108cbe61bc06e8"),
                  Dependencies =>
                    Semantic_Versioning.V_0_2.Within_Minor and
                    Simple_Logging.V_1_0.Within_Major);

   function Example_Project is new Catalogued_Project (Projects.Alire_Reserved);

   Elite_Dangerous,
   Half_Life_3,
   Star_Citizen,
   Windows_100 : Catalog_Entry;
   --  A few fake release to spice descriptions a bit up.

   Syntax_Example : constant Release :=
                      Example_Project.Register
                        (V ("1.0.0"),
                         Origins.New_Filesystem ("/alire"),
                         Dependencies       =>
                           Half_Life_3.Current and -- unconditional
                             On_Condition            -- conditional
                           (Operating_System = GNU_Linux,
                            When_True  => Elite_Dangerous >= "2.0" and Star_Citizen >= V ("3.0"), -- Wish...
                            When_False => Windows_100 > V ("1.0")) and
                           When_Available -- Chained preferences
                             (Preferred => Within_Major (Alire.Project, V ("1.0")),    -- as function
                              Otherwise => Alire.Project.Within_Major (V ("0.0"))) and -- or dot notation
                           When_Available -- Chained preferences
                             (Preferred => Alire.Project.Within_Major ("2.0"),
                              Otherwise => When_Available -- Chained preferences multi-level
                                (Preferred => Within_Major (Alire.Project, V ("1.0")),
                                 Otherwise => Alire.Project.Within_Major ("0.5"))), -- V () is optional

                         Private_Properties => -- These are only interesting to alr, not users
                           GPR_External ("Profile", "False"),
                         --  Sample extra params for build

                         Properties         =>
                           GPR_Scenario ("Build", "Debug" or "Release") and
                           GPR_Free_Scenario ("Path_To_Something") and
                         --  Known scenario variables

                           GPR_File ("scenarios/catastrophical.gpr") and
                         --  Way to specify a project file not named like the project
                         --  Path separators are always "/" and internally converted to native ones

                           On_Condition
                             (Operating_System = Windows,
                              GPR_File ("project_win.gpr")) and
                           On_Condition
                             (Operating_System = GNU_Linux,
                              On_Condition (Distribution = Ubuntu, -- Nested conditions
                                GPR_File ("project_ubuntu.gpr"))) and
                         --  Conditional project file

                           On_Condition
                             (Operating_System = GNU_Linux,
                              Comment ("Long life the penguin")) and
                         --  Conditions on operating system

                           On_Condition
                             (Compiler = GNAT_Unknown, -- /= also works
                              Comment ("Never saw that compiler") and Comment ("But I would like to")) and
                         --  Conditions on compiler version

                           On_Condition
                             (Distro_Release = Ubuntu_Artful,
                              When_True  => Comment ("Living on the edge"),
                              When_False => Comment ("I am a rock")) and
                         --  Conditions on distribution release

                           Comment ("Tell me about your mother") and
                           Website ("http://www.www.www"),
                         --  Unconditional properties

                         Available_When     => -- Impossible mix
                           (Operating_System = Windows and Operating_System /= GNU_Linux) or
                           (Compiler = GNAT_Unknown and Compiler /= GNAT_Unknown));

end Alire.Index.Alire;
