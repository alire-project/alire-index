package Alire.Index.DAK is

   --  Simple Components by Dmitry A. Kazakov
   --  This library is a good challenge since it has many subprojects
   --  It prompted the introduction of the GPR_File property

   --  Unsure if something should be done about -X arguments...

   --  Since most project names are common words, I've taken the liberty to prefix them with dak_
   --  But the original sources are unchanged.

   Base : constant Project_Name := "dak_";
   Repo : constant URL          := "https://github.com/alire-project/dak_simple_components.git";

   DAK_Author  : constant String := "Dmitry A. Kazakov";
   DAK_Website : constant String := "http://www.dmitry-kazakov.de/ada/components.htm";

   Desc_Pre  : constant String := "Simple Components ";
   Desc_Post : constant String := " by Dmitry A. Kazakov";

   Strings_Edit_V_4_27  : constant Release :=
                      Register (Base & "strings_edit",
                                V ("4.27"),
                                Desc_Pre & "(strings)" & Desc_Post,
                                Git (Repo, "b8b638e6694e92141f4c1b33ccad066678260c18"),
                                Properties =>

                                  GPR_File ("strings_edit.gpr") and
                                  GPR_File ("test_strings_edit" / "strings_edit-test.gpr") and

                                  Executable ("test_base64") and
                                  Executable ("test_strings_edit") and
                                  Executable ("test_string_streams") and

                                  License  (GMGPL_2_0) and
                                  Author   (DAK_Author) and
                                  Website  (DAK_Website)
                               );

   Tables_V_4_27  : constant Release :=
                      Register (Base & "tables",
                                V ("4.27"),
                                Desc_Pre & "(tables)" & Desc_Post,
                                Git (Repo, "93dfba26bf6251aefd76af0b5567fd43ec3c8fe2"),
                                Properties =>

                                  GPR_File ("tables.gpr") and
                                  GPR_File ("test_tables" / "tables-test.gpr") and

                                  Executable ("test_tables") and

                                  License  (GMGPL_2_0) and
                                  Author   (DAK_Author) and
                                  Website  (DAK_Website)
                               );



end Alire.Index.DAK;
