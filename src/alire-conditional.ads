with Alire.Conditional_Trees;
with Alire.Dependencies;
with Alire.Properties;
with Alire.Requisites;

with Semantic_Versioning;

package Alire.Conditional with Preelaborate is

   package For_Dependencies is new Conditional_Trees (Dependencies.Dependency,
                                                       Dependencies.Image);
   subtype Dependencies is For_Dependencies.Conditional_Value;

   function New_Dependency (Name     : Alire.Project;
                            Versions : Semantic_Versioning.Version_Set)
                            return Dependencies;


   package For_Properties is new Conditional_Trees (Properties.Property'Class,
                                                     Properties.Image_Classwide);
   subtype Properties is For_Properties.Conditional_Value;

   function New_Property (Property : Alire.Properties.Property'Class)
                          return Properties;

private

   function New_Dependency (Name     : Alire.Project;
                            Versions : Semantic_Versioning.Version_Set)
                            return Dependencies is
     (For_Dependencies.New_Value
        (Alire.Dependencies.New_Dependency (Name, Versions)));

   function New_Property (Property : Alire.Properties.Property'Class)
                          return Properties is
     (For_Properties.New_Value (Property));

end Alire.Conditional;
