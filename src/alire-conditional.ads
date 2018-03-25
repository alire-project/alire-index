with Alire.Conditional_Values;
with Alire.Dependencies.Vectors;
with Alire.Properties;
with Alire.Requisites;

with Semantic_Versioning;

package Alire.Conditional with Preelaborate is

   package For_Dependencies is new Conditional_Values (Dependencies.Vectors.Vector,
                                                       Dependencies.Vectors."and",
                                                       Dependencies.Vectors.Image_One_Line);
   subtype Dependencies is For_Dependencies.Conditional_Value;

   function New_Dependency (Name     : Alire.Project;
                            Versions : Semantic_Versioning.Version_Set)
                            return Dependencies;


   package For_Properties is new Conditional_Values (Properties.Vector,
                                                     Properties."and",
                                                     Properties.Image_One_Line);
   subtype Properties is For_Properties.Conditional_Value;

   function New_Property (Property : Alire.Properties.Property'Class)
                          return Properties;

private

   function New_Dependency (Name     : Alire.Project;
                            Versions : Semantic_Versioning.Version_Set)
                            return Dependencies is
     (For_Dependencies.New_Value
        (Alire.Dependencies.Vectors.New_Dependency (Name, Versions)));

   function New_Property (Property : Alire.Properties.Property'Class)
                          return Properties is
     (For_Properties.New_Value
        (Alire.Properties.To_Vector (Property, 1)));

end Alire.Conditional;
