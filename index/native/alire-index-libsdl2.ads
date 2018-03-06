package Alire.Index.LibSDL2 is

   SDL_V_2 : constant Release :=
               Register ("libsdl2",
                         V ("2"),
                         "Simple DirectMedia Layer development files",
                         Native ((Debian | Ubuntu => Packaged_As ("libsdl2-dev"),
                                  others          => Unavailable)));
   --  Note: the minor version will change with versions of distributions.
   --  If this proved to be a problem several releases should be isolated using the Version property

   SDL_Image_V_2 : constant Release :=
               Register ("libsdl2_image",
                         V ("2"),
                         "Image loading library for Simple DirectMedia Layer 2",
                         Native ((Debian | Ubuntu => Packaged_As ("libsdl2-image-dev"),
                                  others          => Unavailable)));

   SDL_TTF_V_2 : constant Release :=
               Register ("libsdl2_ttf",
                         V ("2"),
                         "TrueType Font library for Simple DirectMedia Layer 2",
                         Native ((Debian | Ubuntu => Packaged_As ("libsdl2-ttf-dev"),
                                  others          => Unavailable)));

end Alire.Index.LibSDL2;
