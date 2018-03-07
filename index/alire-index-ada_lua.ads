with Alire.Index.Liblua;

package Alire.Index.Ada_Lua is

   Prj_Name : constant Project_Name        := "ada_lua";
   Prj_Desc : constant Project_Description := "An Ada binding for Lua";
   Prj_Repo : constant URL                 := "https://github.com/alire-project/ada-lua.git";

   Prj_Maintainer : constant String := "AdaCore";
   Prj_Website    : constant URL    := "https://github.com/AdaCore/ada-lua";

   V_0_0_0 : constant Release :=
               Register (Prj_Name,
                         V ("0.0.0-5.3"),
                         Prj_Desc,
                         Git (Prj_Repo, "ba2fcbf9f8d54d3f6362f20523deb4371371f658"),

                         Dependencies =>
                           Within_Major (Liblua.V_5_3),

                         Properties =>
                           Project_File ("lua.gpr") and

                           Executable ("main") and

                           Maintainer (Prj_Maintainer) and
                           Website    (Prj_Website) and
                           License    (GPL_3_0),

                         Private_Properties =>
                           GPR_File ("lua.gpr") and
                           GPR_File ("examples/example1/example1.gpr") and
                           GPR_File ("examples/example2/example2.gpr")
                        );

end Alire.Index.Ada_Lua;
