(* ::Package:: *)

SetDirectory[NotebookDirectory[]];
<< UncertainValue`


MW["CDF2022"] = UncertainValue[80433.5, 6.4, 6.9]
MW["LEPcomb"] = UncertainValue[80376, 25, 22] (* 1302.3415 (7.7) with \[Chi]2/DOF = 48.9/41 *)
MW["ATLAS"]   = UncertainValue[80369.5, 6.8, {10.6, 13.6}](*1701.07240 Sec.11.4 *)
MW["D0-2013"] = Around[80375, 23] (* 1310.8628 *)

(* older data *)
MW["CDF1995"] = Around[80432, 79]
MW["D0-1995"] = Around[80478, 83]


UncertainCombine[Sequence@@#, "S"->1] &@ (MW[#] & /@ {"CDF2022", "LEPcomb", "ATLAS", "D0-2013"})
UncertainCombine @@ (MW[#] & /@ {"CDF2022", "LEPcomb", "ATLAS", "D0-2013"})


UncertainCombine @@ (MW[#] & /@ {"CDF2022", "LEPcomb", "ATLAS", "D0-2013", "CDF1995", "D0-1995"})



