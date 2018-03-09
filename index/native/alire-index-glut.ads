package Alire.Index.GLUT is

   V_2_8_1 : constant Release :=
         Register (Projects.GLUT,
                   V ("2.8.1-3"),
                   Native ((Debian | Ubuntu => Packaged_As ("freeglut3-dev"),
                            others          => Unavailable)));

end Alire.Index.GLUT;
