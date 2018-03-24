with Ada.Containers.Indefinite_Holders;
with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Containers.Indefinite_Ordered_Sets;

with Alire.Milestones;
with Alire.Projects;
with Alire.Releases;

with Semantic_Versioning;

package Alire.Containers with Preelaborate is

   package Milestone_Sets is new Ada.Containers.Indefinite_Ordered_Sets (Milestones.Milestone,
                                                                         Milestones."<",
                                                                         Milestones."=");


   package Release_Sets is new Ada.Containers.Indefinite_Ordered_Sets (Releases.Release,
                                                                       Releases."<",
                                                                       Releases."=");
   subtype Release_Set is Release_Sets.Set;

   package Release_Holders is new Ada.Containers.Indefinite_Holders (Releases.Release,
                                                                     Releases."=");
   subtype Release_H is Release_Holders.Holder;


   package Project_Version_Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (Name_String, Semantic_Versioning.Version, "<", Semantic_Versioning."<");
   subtype Version_Map is Project_Version_Maps.Map;


   package Project_Release_Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (Projects.Names, Releases.Release, Projects."<", Releases."=");
   type Release_Map is new Project_Release_Maps.Map with null record;

   function Excluding (Map : Release_Map; Name : Projects.Names) return Release_Map;

   function To_Map (R : Releases.Release) return Release_Map;

end Alire.Containers;
