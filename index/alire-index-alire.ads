with Alire.Index.Semantic_Versioning;
with Alire.Index.Simple_Logging;

package Alire.Index.Alire is

   function Project is
     new Catalogued_Project ("Alire project catalog and support files");

   Repo : constant URL := "https://github.com/alire-project/alire.git";

   V_0 : constant Release :=
           Project.Unreleased
             (V ("0.0"),
              No_Origin,
              Properties =>
                Author ("Alejandro R. Mosteo") and
                License (GPL_3_0));

   V_0_4 : constant Release :=
               Project.Register
                 (V_0
                  .Upgrading
                    (V ("0.4"),
                     Git (Repo, "219cdcbc5f26efca331400582026c6377ef0f794"))
                  .Extending
                    (Dependencies =>
                       Semantic_Versioning.V_0_3_1.Within_Minor and
                       Simple_Logging.V_1_0.Within_Major));

   V_0_2 : constant Release :=
               Project.Register
                 (V_0
                  .Upgrading
                    (V ("0.2"),
                     Git (Repo, "5ba81ba33dfeb184b2e644ef2996200b5fdd6ae4"))
                  .Extending
                    (Dependencies =>
                       Semantic_Versioning.V_0_3.Within_Minor and
                       Simple_Logging.V_1_0.Within_Major));

   V_0_1_2 : constant Release :=
               Project.Register
                 (V_0
                  .Upgrading
                    (V ("0.1.2"),
                     Git (Repo, "e2dee2e147ae9e4d666567b53b108cbe61bc06e8"))
                  .Extending
                    (Dependencies =>
                       Semantic_Versioning.V_0_1_2.Within_Minor and
                       Simple_Logging.V_1_0.Within_Major));

--     function Example_Project is new Catalogued_Project ("alire_indexing_example",
--                                                         "Demo of dependencies/properties/conditionals in alire-index-alire.ads");
--     function Elite_Dangerous is new Catalogued_Project ("elite_dangerous",
--                                                         "Elite: Dangerous");
--     function Half_Life       is new Catalogued_Project ("half_life",
--                                                         "Half-Life franchise");
--     function Star_Citizen    is new Catalogued_Project ("star_citizen",
--                                                         "Star Citizen and Squadron 42 humongousware");
--     function Windows_3000    is new Catalogued_Project ("windows_3000",
--                                                         "Next-gen operating system for the brainz");
--  A few fake projects to spice descriptions a bit up.

   function Example_Project return Catalog_Entry renames Project;
   function Elite_Dangerous return Catalog_Entry renames Project;
   function Half_Life       return Catalog_Entry renames Project;
   function Star_Citizen    return Catalog_Entry renames Project;
   function Windows_3000    return Catalog_Entry renames Project;

   Syntax_Example : constant Release :=
                      Example_Project.Unreleased
                        (V ("0.0.1"),
                         Origins.New_Filesystem ("/alire"),
                         Notes => "Mock release with examples of complex conditions",
                         Dependencies       =>
                           Half_Life >= "3.0" and -- unconditional
                             On_Condition            -- conditional
                           (Operating_System = GNU_Linux,
                            When_True  => Elite_Dangerous >= "2.0" and Star_Citizen >= V ("3.0"), -- Wish...
                            When_False => Windows_3000 > V ("1.0")) and
                           When_Available -- Chained preferences
                             (Preferred => Within_Major (Alire.Project, V ("1.0"))) and -- or dot notation
                           When_Available -- Chained preferences
                             (Preferred => Alire.Project.Within_Major ("2.0"),
                              Otherwise => When_Available -- Chained preferences multi-level
                                (Preferred => Within_Major (Alire.Project, V ("1.0")),
                                 Otherwise => Alire.Project.Within_Major ("0.5"))) and -- V () is optional
                           (Star_Citizen >= "4.0" or Half_Life >= "3.0"), -- Chained preferences, takes first

                         Private_Properties => -- These are only interesting to alr, not users
                           GPR_External ("Profile", "False"),
                         --  Sample extra params for build

                         Properties         =>
                           GPR_Scenario ("Build", "Debug" or "Release") and
                           GPR_Free_Scenario ("Path_To_Something") and
                         --  Known scenario variables

                           Project_File ("scenarios/catastrophical.gpr") and
                         --  Way to specify a project file not named like the project
                         --  Path separators are always "/" and internally converted to native ones

                           On_Condition
                             (Operating_System = Windows,
                              Project_File ("project_win.gpr")) and
                           On_Condition
                             (Operating_System = GNU_Linux,
                              On_Condition (Distribution = Ubuntu, -- Nested conditions
                                Project_File ("project_ubuntu.gpr"))) and
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
