with Alire.Index.LibGNUTLS;
with Alire.Index.UnixODBC;

package Alire.Index.DAK_Components is

   function Project is new Catalogued_Project (Projects.DAK_Components);

   function Subproject_DAK_Strings_Edit is new Catalogued_Project (DAK_Strings_Edit,
                                                                   Projects.DAK_Components);
   function Subproject_DAK_Tables       is new Catalogued_Project (DAK_Tables,
                                                                   Projects.DAK_Components);
   function Subproject_DAK_Components_Connections is new Catalogued_Project (DAK_Components_Connections,
                                                                             Projects.DAK_Components);
   function Subproject_DAK_Components_Connections_Secure is new Catalogued_Project
     (DAK_Components_Connections_Secure, Projects.DAK_Components);

   function Subproject_DAK_Components_NTP is new Catalogued_Project (DAK_Components_NTP,
                                                                     Projects.DAK_Components);
   function Subproject_DAK_Components_ODBC is new Catalogued_Project (DAK_Components_ODBC,
                                                                      Projects.DAK_Components);
   function Subproject_DAK_Components_SQLite is new Catalogued_Project (DAK_Components_SQLite,
                                                                        Projects.DAK_Components);

   --  Simple Components by Dmitry A. Kazakov
   --  Since most project names are common words, I've taken the liberty to prefix them with dak_
   --     but the original sources are unchanged.

   Repo : constant URL          := "https://github.com/alire-project/dak_simple_components.git";

   DAK_Author  : constant String := "Dmitry A. Kazakov";
   DAK_Website : constant String := "http://www.dmitry-kazakov.de/ada/components.htm";

   Strings_Edit_V_4_27 : constant Release :=
                           Subproject_DAK_Strings_Edit.Register
                             (V ("4.27"),
                              Git (Repo, "8ac67a28d666dde3a0f96b5a08a7016e4f29d491"),
                              Properties         =>
                                Project_File ("strings_edit.gpr") and

                                GPR_Scenario ("Legacy", "Ada95" or "Ada2005" or "Ada2012") and
                                GPR_Scenario ("Development", "Debug" or "Release") and

                                Executable ("test_base64") and
                                Executable ("test_strings_edit") and
                                Executable ("test_string_streams") and

                                License  (GMGPL_2_0) and
                                Author   (DAK_Author) and
                                Website  (DAK_Website),

                              Private_Properties =>
                                GPR_File ("strings_edit.gpr") and
                                GPR_File ("test_strings_edit/strings_edit-test.gpr")
                             );

   Tables_V_4_27 : constant Release :=
                     Subproject_DAK_Tables.Register
                       (V ("4.27"),
                        Git (Repo, "19205e4981d72242daf72da7d59c5faf2b4c91fd"),
                        Properties         =>
                          Project_File ("tables.gpr") and

                          GPR_Scenario ("Legacy", "Ada95" or "Ada2005" or "Ada2012") and
                          GPR_Scenario ("Development", "Debug" or "Release") and

                          Executable ("test_tables") and

                          License  (GMGPL_2_0) and
                          Author   (DAK_Author) and
                          Website  (DAK_Website),

                        Private_Properties =>
                          GPR_File ("tables.gpr") and
                          GPR_File ("test_tables/tables-test.gpr")
                       );

   Components_V_4_27 : constant Release :=
                         Project.Register
                           (V ("4.27"),
                            Git (Repo, "542f02c9be86693f759fcb784a8462bc4b25f1f2"),
                            Dependencies       =>
                              Strings_Edit_V_4_27.Within_Major and
                              Tables_V_4_27.Within_Major,

                            Properties         =>
                              Project_File ("components.gpr") and

                              GPR_Scenario ("Atomic_Access", "Pragma-atomic" or "GCC-built-ins" or "GCC-long-offsets") and
                              GPR_Scenario ("Tasking", "Multiple" or "Single") and
                              GPR_Scenario ("Traced_objects", "Off" or "On") and

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
                              Executable ("test_utf8_tables") and

                              License  (GMGPL_2_0) and
                              Author   (DAK_Author) and
                              Website  (DAK_Website) and
                              Comment  ("Tasking=Single seems to be broken at persistent-single_file-text_io.adb"),

                            Private_Properties =>
                              GPR_File ("components.gpr") and
                              GPR_File ("test_components/components-tests.gpr")
                           );

   Components_NTP_V_4_27 : constant Release :=
                             Subproject_DAK_Components_NTP.Register
                               (V ("4.27"),
                                Git (Repo, "34fb305d6ef360cde5e272b51409097a5de72017"),
                                Dependencies       =>
                                  Components_V_4_27.Within_Major,

                                Properties         =>
                                  Project_File ("components-ntp.gpr") and

                                  Executable ("test_ntp") and

                                  License  (GMGPL_2_0) and
                                  Author   (DAK_Author) and
                                  Website  (DAK_Website),

                                Private_Properties =>
                                  GPR_File ("components-ntp.gpr") and
                                  GPR_File ("test_components/components-ntp-test_ntp.gpr")
                               );

   Components_Sqlite_V_4_27 : constant Release :=
                                Subproject_DAK_Components_SQLite.Register
                                  (V ("4.27"),
                                   Git (Repo, "6fda0f3f7494815c87b329f7411b9a49ff97b9ba"),
                                   Dependencies       =>
                                     Components_V_4_27.Within_Major,

                                   Properties         =>
                                     Project_File ("components-sqlite.gpr") and

                                     Executable ("test_sqlite_benchmark") and
                                     Executable ("test_sqlite_persistence") and

                                     License  (GMGPL_2_0) and
                                     Author   (DAK_Author) and
                                     Website  (DAK_Website),

                                   Private_Properties =>
                                     GPR_File ("components-sqlite.gpr") and
                                     GPR_File ("test_components/components-sqlite-benchmark_tests.gpr") and
                                     GPR_File ("test_components/components-sqlite-sqlite_persistence_tests.gpr")
                                  );

   Components_Connections_V_4_27 : constant Release :=
                                     Subproject_DAK_Components_Connections.Register
                                       (V ("4.27"),
                                        Git (Repo, "008935d5a89396cc0c39afb39f04bf6a89a92058"),
                                        Dependencies       =>
                                          Components_V_4_27.Within_Major and
                                          Components_Sqlite_V_4_27.Within_Major,

                                        Properties         =>
                                        --  Main projects
                                          Project_File ("components-connections_server.gpr") and
                                          Project_File ("components-connections_server-elv_max_cube.gpr") and
                                          Project_File ("components-connections_server-http_server.gpr") and
                                          Project_File ("components-connections_server-http_server-sqlite_browser.gpr") and
                                          Project_File ("components-connections_server-modbus.gpr") and
                                          Project_File ("components-connections_server-mqtt.gpr") and
                                          Project_File ("components-connections_server-smtp.gpr") and

                                          Executable ("test_data_server") and
                                          Executable ("test_echo_client") and
                                          Executable ("test_echo_client_async") and
                                          Executable ("test_echo_server") and
                                          Executable ("test_elv_max_cube_client") and
                                          Executable ("test_http_client") and
                                          Executable ("test_http_continuous_server") and
                                          Executable ("test_http_server") and
                                          Executable ("test_http_sqlite_browser") and
                                          Executable ("test_infinity_server") and
                                          Executable ("test_modbus_client") and
                                          Executable ("test_mqtt_client") and
                                          Executable ("test_mqtt_server") and
                                          Executable ("test_mqtt_webserver") and
                                          Executable ("test_websocket_duplex_server") and
                                          Executable ("test_websocket_server") and

                                          License  (GMGPL_2_0) and
                                          Author   (DAK_Author) and
                                          Website  (DAK_Website),

                                        Private_Properties =>
                                          GPR_File ("components-connections_server.gpr") and
                                          GPR_File ("components-connections_server-elv_max_cube.gpr") and
                                          GPR_File ("components-connections_server-http_server.gpr") and
                                          GPR_File ("components-connections_server-http_server-sqlite_browser.gpr") and
                                          GPR_File ("components-connections_server-modbus.gpr") and
                                          GPR_File ("components-connections_server-mqtt.gpr") and
                                          GPR_File ("components-connections_server-smtp.gpr") and
                                        --  Test projects
                                          GPR_File ("test_components/components-connections_server-elv_max_cube-test_elv_max_cube_client.gpr") and
                                          GPR_File ("test_components/components-connections_server-http_server-test_http_server.gpr") and
                                          GPR_File ("test_components/components-connections_server-modbus-test_modbus_client.gpr") and
                                          GPR_File ("test_components/components-connections_server-mqtt-test_mqtt.gpr") and
                                          GPR_File ("test_components/components-connections_server-test_data_server.gpr") and
                                          GPR_File ("test_components/components-connections_server-test_echo_client_async.gpr") and
                                          GPR_File ("test_components/components-connections_server-test_echo_client.gpr") and
                                          GPR_File ("test_components/components-connections_server-test_echo_server.gpr") and
                                          GPR_File ("test_components/components-connections_server-test_websockets_mqtt.gpr") and
                                          GPR_File ("test_components/components-test_sqlite_browser.gpr")
                                       );

   Components_Connections_Secure_V_4_27 : constant Release :=
                                            Subproject_DAK_Components_Connections_Secure.Register
                                              (V ("4.27"),
                                               Git (Repo, "ca72cf4150ae14ba6d40c3d2dd92c7846cb4cb5d"),
                                               Dependencies       =>
                                                 Components_Connections_V_4_27.Within_Major and
                                                 LibGNUTLS.V_3_5_8.Within_Major,

                                               Properties         =>
                                                 Project_File ("components-connections_server-secure.gpr") and
                                                 Project_File ("components-gnutls.gpr") and

                                                 Executable ("test_https_client") and
                                                 Executable ("test_https_server") and
                                                 Executable ("test_smtp_client") and

                                                 License  (GMGPL_2_0) and
                                                 Author   (DAK_Author) and
                                                 Website  (DAK_Website),

                                               Private_Properties =>
                                                 GPR_File ("components-connections_server-secure.gpr") and
                                                 GPR_File ("components-gnutls.gpr") and
                                                 GPR_File ("test_components/components-connections_server-http_server-test_https_server.gpr") and
                                                 GPR_File ("test_components/components-connections_server-smtp-test_smtp.gpr")
                                              );

   Components_ODBC_V_4_27 : constant Release :=
                              Subproject_DAK_Components_ODBC.Register
                                (V ("4.27"),
                                 Git (Repo, "47337f8a5dd69404087129d5cca79885d6e8cd3f"),
                                 Dependencies       =>
                                   Components_V_4_27.Within_Major and
                                   UnixODBC.V_2_3.Within_Major,

                                 Properties         =>
                                   Project_File ("components-odbc.gpr") and

                                   Executable ("test_odbc_bindings") and

                                   License  (GMGPL_2_0) and
                                   Author   (DAK_Author) and
                                   Website  (DAK_Website),

                                 Private_Properties =>
                                   GPR_File ("components-odbc.gpr") and
                                   GPR_File ("test_components/components-odbc-odbc_bindings_tests.gpr") and

                                   On_Condition
                                     (Operating_System = GNU_Linux,
                                      GPR_External ("odbc", "unixODBC")) and
                                   On_Condition
                                     (Operating_System = Windows,
                                      GPR_External ("odbc", "ODBC32")) and
                                   On_Condition
                                     (Word_Size = Bits_32,
                                      GPR_External ("arch", "i686")) and
                                   On_Condition
                                     (Word_Size = Bits_64,
                                      GPR_External ("arch", "x86_64"))
                                );

end Alire.Index.DAK_Components;
