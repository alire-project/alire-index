with Alire.Conditional;
with Alire.Projects;

with Semantic_Versioning;
with Semantic_Versioning.Expressions;

package Alire.Versions with Preelaborate is

   --  Helper package to prepare expressions on version for use in Alire.Index.*

   type Versioned is interface;

   function Name (V : Versioned) return Projects.Names is abstract;

   function Version (V : Versioned) return Semantic_Versioning.Version is abstract;

   function This_Version (V : Versioned'Class) return Conditional.Dependencies;
   function Within_Major (V : Versioned'Class) return Conditional.Dependencies;
   function Within_Minor (V : Versioned'Class) return Conditional.Dependencies;

   type Comparable is interface;

   function New_Dependency (L : Comparable; VS : Semantic_Versioning.Version_Set)
                            return Conditional.Dependencies is abstract;

   function New_Dependency_Classwide (L : Comparable'Class; VS : Semantic_Versioning.Version_Set)
                                      return Conditional.Dependencies is (L.New_Dependency (VS));

   package Expressions is new Semantic_Versioning.Expressions
     (Comparable'Class,
      Conditional.Dependencies,
      New_Dependency_Classwide);

private

   use Semantic_Versioning;

   function This_Version (V : Versioned'Class) return Conditional.Dependencies is
     (Conditional.New_Dependency (V.Name, Exactly (V.Version)));

   function Within_Major (V : Versioned'Class) return Conditional.Dependencies is
     (Conditional.New_Dependency (V.Name, Within_Major (V.Version)));

   function Within_Minor (V : Versioned'Class) return Conditional.Dependencies is
     (Conditional.New_Dependency (V.Name, Within_Minor (V.Version)));

end Alire.Versions;
