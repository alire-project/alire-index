with Alire.Conditional_Values;
with Alire.Dependencies.Vectors;
with Alire.Properties;
with Alire.Requisites;

package Alire.Conditional with Preelaborate is

   package For_Dependencies is new Conditional_Values (Dependencies.Vectors.Vector,
                                                       Dependencies.Vectors."and");
   subtype Dependencies is For_Dependencies.Conditional_Value;

   package For_Properties is new Conditional_Values (Properties.Vector,
                                                     Properties."and");
   subtype Properties is For_Properties.Conditional_Value;

end Alire.Conditional;
