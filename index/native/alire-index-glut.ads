package Alire.Index.GLUT is

   Name : constant String := "glut";
   Desc : constant String := "OpenGL Utility Toolkit";

   V_2_8_1 : constant Release :=
         Register (Name,
                   V ("2.8.1-3"),
                   Desc,
                   Native ((Debian | Ubuntu => Packaged_As ("freeglut3-dev"),
                            others          => Unavailable)));

end Alire.Index.GLUT;
