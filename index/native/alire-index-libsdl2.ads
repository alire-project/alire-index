package Alire.Index.LibSDL2 is

   SDL_V_2 : constant Release :=
               Register (Projects.LibSDL2,
                         V ("2"),
                         Native ((Debian | Ubuntu => Packaged_As ("libsdl2-dev"),
                                  others          => Unavailable)));

   SDL_Image_V_2 : constant Release :=
               Register (LibSDL2_Image,
                         V ("2"),
                         Native ((Debian | Ubuntu => Packaged_As ("libsdl2-image-dev"),
                                  others          => Unavailable)));

   SDL_TTF_V_2 : constant Release :=
               Register (LibSDL2_TTF,
                         V ("2"),
                         Native ((Debian | Ubuntu => Packaged_As ("libsdl2-ttf-dev"),
                                  others          => Unavailable)));

end Alire.Index.LibSDL2;
