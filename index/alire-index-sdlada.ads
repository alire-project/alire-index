with Alire.Index.LibSDL2;

package Alire.Index.SDLAda is

   Prj_Name : constant Project_Name        := "sdlada";
   Prj_Desc : constant Project_Description := "Ada 2012 bindings to SDL 2";
   Prj_Repo : constant URL                 := "https://github.com/alire-project/sdlada.git";

   Prj_Author     : constant String        := "Luke A. Guest";

   V_2_3_1 : constant Release :=
               Register (Prj_Name,
                         V ("2.3.1"),
                         Prj_Desc,
                         Git (Prj_Repo, "570232193facb90a58f67aadac93df9dfae8bcd4"),

                         Dependencies =>
                           Within_Major (LibSDL2.SDL_V_2) and
                           Within_Major (LibSDL2.SDL_Image_V_2) and
                           Within_Major (LibSDL2.SDL_TTF_V_2),

                         Properties =>
                           Project_File ("sdlada.gpr") and

                           GPR_Scenario ("SDL_MODE", "debug" or "release") and
                           GPR_Scenario ("SDL_PLATFORM", "linux" or "bsd" or "windows" or "macosx" or "ios" or "android") and

                           Author     (Prj_Author) and
                           License    (Zlib),

                         Private_Properties =>
                           GPR_Path ("build/gnat") and

                           GPR_File ("build/gnat/sdlada.gpr") and
--                           GPR_File ("build/gnat/sdlada_image.gpr") and
                           GPR_File ("build/gnat/tests.gpr") and
--                           GPR_File ("build/gnat/tests_image.gpr") and
                           GPR_File ("build/gnat/test_maths_build.gpr") and
--                           GPR_File ("build/gnat/tools.gpr") and
--                           GPR_File ("build/gnat/unit_tests.gpr") and

                           Executable ("clipboard") and
                           Executable ("error") and
                           Executable ("libraries") and
                           Executable ("load_surface") and
                           Executable ("platform") and
                           Executable ("rwops") and
                           Executable ("stream") and
                           Executable ("stream2") and
                           Executable ("surface") and
                           Executable ("test") and
                           Executable ("version") and

                           GPR_External ("SDL_MODE", "release") and

                           On_Condition
                             (System_Is (GNU_Linux),
                              GPR_External ("SDL_PLATFORM", "linux")),

                         Available_When =>
                           System_Is (GNU_Linux)
                        );

end Alire.Index.SDLAda;
