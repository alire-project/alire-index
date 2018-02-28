with Alire.Platforms;

package Alire.Properties.Platform with Preelaborate is

   function Compiler_Is (C : Platforms.Compilers) return Vector;

   function Distribution_Is (D : Platforms.Distributions) return Vector;

   function System_Is (V : Platforms.Operating_Systems) return Vector;

   --  The following packages declare types used elsewhere so they have to be public  --

   package Compilers is new Values (Platforms.Compilers,
                                    Platforms.Compilers'IMage);

   package Distributions is new Values (Platforms.Distributions,
                                        Platforms.Distributions'Image);

   package Operating_Systems is new Values (Platforms.Operating_Systems,
                                            Platforms.Operating_Systems'Image);

private

   function Compiler_Is (C : Platforms.Compilers) return Vector is
     (+Compilers.New_Property (C));

   function Distribution_Is (D : Platforms.Distributions) return Vector is
     (+Distributions.New_Property (D));

   function System_Is (V : Platforms.Operating_Systems) return Vector is
     (+Operating_Systems.New_Property (V));

end Alire.Properties.Platform;
