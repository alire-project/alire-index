with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Containers.Indefinite_Ordered_Sets;
with Ada.Containers.Indefinite_Vectors;

with Alire.Releases;

package Alire.Containers with Preelaborate is   
   
   package Release_Sets is new Ada.Containers.Indefinite_Ordered_Sets (Releases.Release,
                                                                       Releases."<",
                                                                       Releases."=");   
   subtype Release_Set is Release_Sets.Set;
   
   package Milestone_Sets is new Ada.Containers.Indefinite_Ordered_Sets (Milestone);
   subtype Milestone_Set is Milestone_Sets.Set;
   
   package Project_Version_Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (Project_Name, Semantic_Versioning.Version, "<", Semantic_Versioning."<");
   subtype Version_Map is Project_Version_Maps.Map;             

end Alire.Containers;
