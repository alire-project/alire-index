with Alire.Index.Semantic_Versioning;
with Alire.Index.Simple_Logging;

package Alire.Index.Alire is

   Name : constant Project_Name := "alire";
   Repo : constant URL          := "https://bitbucket.org/aleteolabs/alire.git";

   Desc : constant Project_Description := "Alire project catalog and support files";

   V_0_1 : constant Release :=
              Register (Name,
                        V ("0.1.2"),
                        Desc,
                        Git (Repo, "e2dee2e147ae9e4d666567b53b108cbe61bc06e8"),
                        Depends_On =>
                          Within_Minor (Semantic_Versioning.V_0_1) and
                          Within_Major (Simple_Logging.V_1)
                       );


   Syntax_Example : constant Release :=
                Register ("alire_example",
                          V ("1.0.0"),
                          "Release with all index syntax features",
                          Origins.New_Filesystem ("/fake"),
                          Properties =>
                            If_Platform
                              (System_Is (GNU_Linux),
                               Comment ("Long life the penguin")) and
                            If_Platform
                              (not Compiler_Is_At_Least (GNAT_Unknown),
                               Comment ("Never saw that compiler") and Comment ("But I would like to")) and
                            If_Platform
                              (Distribution_Is (Ubuntu_Artful),
                               When_True  => Comment ("Living on the edge"),
                               When_False => Comment ("I am a rock")) and
                            Comment ("Tell me about your mother"),
                          Available_When => -- Impossible mix
                            (System_Is (Windows) and System_Is (GNU_Linux)) or
                            (Compiler_Is_At_Least (GNAT_Unknown) and not Compiler_Is_At_Least (GNAT_Unknown)));

end Alire.Index.Alire;
