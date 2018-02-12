with Alire.Containers;
with Alire.Index; use Alire.Index;

package Alire.Query is

   --subtype Solution is Containers.Version_Map; -- A dependence-valid mapping of project -> version
   subtype Instance is Containers.Release_Map; -- A list of releases complying with a Solution

   Empty_Instance : constant Instance := Containers.Project_Release_Maps.Empty_Map;

   function Exists (Project : Project_Name) return Boolean;

   function Resolve (Deps : Dependencies;
                     Success    : out Boolean) return Instance;

   procedure Print_Solution (I : Instance);

end Alire.Query;
