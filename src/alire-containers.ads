with Ada.Containers.Indefinite_Holders;
with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Containers.Indefinite_Ordered_Sets;

with Alire.Releases;

with Semantic_Versioning;

package Alire.Containers with Preelaborate is

   package Release_Sets is new Ada.Containers.Indefinite_Ordered_Sets (Releases.Release,
                                                                       Releases."<",
                                                                       Releases."=");
   subtype Release_Set is Release_Sets.Set;

   package Release_Holders is new Ada.Containers.Indefinite_Holders (Releases.Release,
                                                                     Releases."=");
   subtype Release_H is Release_Holders.Holder;


   package Project_Version_Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (Project_Name, Semantic_Versioning.Version, "<", Semantic_Versioning."<");
   subtype Version_Map is Project_Version_Maps.Map;


   package Project_Release_Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (Project_Name, Releases.Release, "<", Releases."=");
   subtype Release_Map is Project_Release_Maps.Map;

   function To_Map (R : Releases.Release) return Release_Map;

end Alire.Containers;
