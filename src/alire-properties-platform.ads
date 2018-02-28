with Alire.Platforms;

package Alire.Properties.Platform with Preelaborate is

   package Compilers is new Values (Platforms.Compilers,
                                    Platforms.Compilers'IMage);

   package Distributions is new Values (Platforms.Distributions,
                                        Platforms.Distributions'Image);

   package Operating_Systems is new Values (Platforms.Operating_Systems,
                                           Platforms.Operating_Systems'Image);

--     function Current return Vector;
   --  FIXME this will eventually have to go into Alr

private

   function System_Is (V : Platforms.Operating_Systems) return Vector is
     (+Operating_Systems.New_Property (V));

   function Compiler_Is (C : Platforms.Compilers) return Vector is
     (+Compilers.New_Property (C));

--     function Current return Vector is
--       (Compiler_Is (Alire.Compilers.Compiler) and
--          System_Is (Alire.Operating_Systems.Current));

end Alire.Properties.Platform;
