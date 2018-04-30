package Alire.Index.LibSDL2 is

   function Project is new Catalogued_Project
     ("Simple DirectMedia Layer development files");

   function Image is new Extension
     (Project,
      "Image loading library for Simple DirectMedia Layer 2");

   function TTF is new Extension
     (Project,
      "TrueType Font library for Simple DirectMedia Layer 2");

   SDL_V_2 : constant Release :=
               Project.Register
                 (V ("2"),
                  Native ((Debian | Ubuntu => Packaged_As ("libsdl2-dev"),
                           others          => Unavailable)));

   SDL_Image_V_2 : constant Release :=
                     Image.Register
                       (V ("2"),
                        Native ((Debian | Ubuntu => Packaged_As ("libsdl2-image-dev"),
                                 others          => Unavailable)));

   SDL_TTF_V_2 : constant Release :=
                   TTF.Register
                     (V ("2"),
                      Native ((Debian | Ubuntu => Packaged_As ("libsdl2-ttf-dev"),
                               others          => Unavailable)));

end Alire.Index.LibSDL2;
