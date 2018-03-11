package Alire.Index.LibSDL2 is

   function Project is new Catalogued_Project (Projects.LibSDL2);

   function Subproject_Image is new Catalogued_Project (Projects.LibSDL2_Image, Projects.LibSDL2);

   function Subproject_TTF is new Catalogued_Project (Projects.LibSDL2_TTF, Projects.LibSDL2);

   SDL_V_2 : constant Release :=
               Project.Register
                 (V ("2"),
                  Native ((Debian | Ubuntu => Packaged_As ("libsdl2-dev"),
                           others          => Unavailable)));

   SDL_Image_V_2 : constant Release :=
                     Subproject_Image.Register
                       (V ("2"),
                        Native ((Debian | Ubuntu => Packaged_As ("libsdl2-image-dev"),
                                 others          => Unavailable)));

   SDL_TTF_V_2 : constant Release :=
                   Subproject_TTF.Register
                     (V ("2"),
                      Native ((Debian | Ubuntu => Packaged_As ("libsdl2-ttf-dev"),
                               others          => Unavailable)));

end Alire.Index.LibSDL2;
