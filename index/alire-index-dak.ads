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
                                Git (Repo, "44ac8e0c817558b8641f746ce225b3d2fa90b7a1"),
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
                                Git (Repo, "19205e4981d72242daf72da7d59c5faf2b4c91fd"),
                                Properties =>

                                  GPR_File ("tables.gpr") and
                                  GPR_File ("test_tables" / "tables-test.gpr") and

                                  Executable ("test_tables") and

                                  License  (GMGPL_2_0) and
                                  Author   (DAK_Author) and
                                  Website  (DAK_Website)
                               );

   Components_V_4_27  : constant Release :=
                          Register (Base & "components",
                                    V ("4.27"),
                                    Desc_Pre & "(base components)" & Desc_Post,
                                    Git (Repo, "542f02c9be86693f759fcb784a8462bc4b25f1f2"),
                                    Depends_On =>
                                      Within_Major (Strings_Edit_V_4_27) and
                                      Within_Major (Tables_V_4_27),

                                    Properties =>
                                      GPR_File ("components.gpr") and
                                      GPR_File ("test_components" / "components-tests.gpr") and

                                      License  (GMGPL_2_0) and
                                      Author   (DAK_Author) and
                                      Website  (DAK_Website) and

                                      Executable ("test_approximations") and
                                      Executable ("test_association") and
                                      Executable ("test_blackboard") and
                                      Executable ("test_blackboard_performance") and
                                      Executable ("test_blocking_files") and
                                      Executable ("test_block_streams") and
                                      Executable ("test_b_trees") and
                                      Executable ("test_cubic_spline") and
                                      Executable ("test_dining_philosophers") and
                                      Executable ("test_fifo") and
                                      Executable ("test_generic_indefinite_sets") and
                                      Executable ("test_generic_maps") and
                                      Executable ("test_generic_sets") and
                                      Executable ("test_graphs") and
                                      Executable ("test_handles") and
                                      Executable ("test_ieee_754") and
                                      Executable ("test_linked_lists") and
                                      Executable ("test_linked_lists_scheduler_test") and
                                      Executable ("test_parser_stream_io") and
                                      Executable ("test_persistent_memory_pool") and
                                      Executable ("test_persistent_storage") and
                                      Executable ("test_sequencer") and
                                      Executable ("test_single_file_persistence") and
                                      Executable ("test_stack") and
                                      Executable ("test_storage_streams") and
                                      Executable ("test_string_streams") and
                                      Executable ("test_synchronization_events") and
                                      Executable ("test_transactional_blocking_files") and
                                      Executable ("test_utf8_tables")
                                   );

end Alire.Index.DAK;
