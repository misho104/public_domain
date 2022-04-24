(* ::Package:: *)

(* Time-Stamp: <2022-04-24 17:18:27> *)

(* :Title: Uncertain Value *)
(* :Context: UncertainValue` *)

(* :Author: Sho Iwamoto / Misho *)
(* :Copyright: 2022 Sho Iwamoto / Misho *)

(* Copyright 2022 Sho Iwamoto / Misho
   To the extent possible under law, the author(s) have dedicated all copyright
   and related and neighboring rights to this software to the public domain
   worldwide. This software is distributed without any warranty.
   You should have received a copy of the CC0 Public Domain Dedication along with
   this software. If not, see http://creativecommons.org/publicdomain/zero/1.0/
*)

(* :Package Version: 0.0.1 *)
(* :Mathematica Version: 13.0 *)
(* :History:
   0.0.1 (2022 Apr.) initial version
*)

BeginPackage["UncertainValue`"];

Quiet[Remove["UncertainValue`*", "UncertainValue`Private`*"], Remove::rmnsm];

(* Usage messages *)
FMT = StringReplace[{
  RegularExpression["<(.+?)>"] -> "\!\(\*StyleBox[\"$1\", \"SO\"]\)",     (* arg *)
  RegularExpression["#(.+?)#"] -> "\!\(\*StyleBox[\"$1\", \"TI\"]\)",     (* options *)
  RegularExpression["\\*(.+?)\\*"] -> "\!\(\*StyleBox[\"$1\", \"SB\"]\)", (* emphasize *)
  RegularExpression["'(\\w+)'"] -> "\!\(\*StyleBox[\"\[OpenCurlyDoubleQuote]$1\[CloseCurlyDoubleQuote]\", \"MR\"]\)",  (* quoted fixed string *)
  RegularExpression["`(\\w+)`"] -> "\!\(\*StyleBox[\"$1\", \"MR\"]\)"     (* fixed string *)
}]@*StringTrim;

UncertainValue::usage = FMT["
*UncertainValue*[<x>, <stat>, <syst>] represents a value with statistical uncertainty <stat> and systematic undertainty <syst>.
The uncertainties are assumed symmetric around the representative value <x>.
"];

UncertainCombine::usage = FMT["
*UncertainCombine*[<unc>, ...] combines values with uncertainty, <unc>, each of which is `UncertainValue`.
Options:
   \"S\" -> Automatic | None | other numerical value.
"];

Remove[FMT];


(* messages *)
UncertainCombine::invalids = "Invalid option value for \"S\": `1`";

Begin["`Private`"];
protected = Unprotect[ UncertainValue, UncertainCombine ];

$Debug = False;

SquaredSum[x__] := Total[(#^2)&/@{x}];
RootSquaredSum[x__] := Sqrt[SquaredSum[x]];

UncertainValue[x_, stat_List, syst_] /; AllTrue[stat, NumericQ] := UncertainValue[x, RootSquaredSum@@stat, syst];
UncertainValue[x_, stat_, syst_List] /; AllTrue[syst, NumericQ] := UncertainValue[x, stat, RootSquaredSum@@syst];
Around[UncertainValue[x_, stat_, syst_]] ^:= Around[x, RootSquaredSum[stat, syst]];

Options[UncertainCombine] = { "S" -> Automatic };
Attributes[UncertainCombine] = {Orderless};

UncertainCombineSub[values : Repeated[_UncertainValue], s_] := Block[{
    x, wstat, wsyst, w, combined, chi2
  },
  x = #[[1]] & /@ {values};
  wstat = 1/(#[[2]]^2) & /@ {values};
  wsyst = 1/(s^2 * #[[3]]^2) & /@ {values};
  w = 1/(#[[2]]^2 + s^2 * #[[3]]^2) & /@ {values};
  combined = UncertainValue[w . x / Total[w], Total[wstat]^(-1/2), Total[wsyst]^(-1/2)];
  chi2 = N[Total[MapThread[#2 (#1 - combined[[1]])^2&, {x, w}]]];
  <|"combined"->combined, "chi2"->chi2, "chi2norm"->chi2/(Length[x]-1)|>
]

UncertainCombine[values : Repeated[_UncertainValue], opt : OptionsPattern[]] := Block[{s0, raw, sRule},
  Switch[OptionValue["S"],
    Automatic,  None,
    None,       s0 = 1,
    _?NumericQ, s0 = OptionValue["S"],
    _, Message[UncertainCombine::invalids, OptionValue["S"]]; Abort[]];
  raw = UncertainCombineSub[values, s0];
  If[OptionValue["S"] === Automatic,
    sRule = If[N[raw["chi2norm"] /. s0->1.0] > 1, FindRoot[raw["chi2norm"] == 1, {s0, 1.0}], {s0->1}];
    raw = raw /. sRule;
    s0 = (s0 /. sRule);
  ];
  Return[{raw["combined"], "chi2"->raw["chi2"], "S"->s0}]
]

UncertainCombine[values : Repeated[_Around], opt : OptionsPattern[]] /; AllTrue[#["Uncertainty"] &/@ {values}, NumericQ]:= Block[{
    s, x, w, combined, chi2, chi2norm
  },
  Switch[OptionValue["S"],
    Automatic,    None,
    None,         s = 1,
    _ ? NumericQ, s = OptionValue["S"],
    _, Message[UncertainCombine::invalids, OptionValue["S"]]; Abort[]];

  x = #["Value"] & /@ {values};
  w = 1/(#["Uncertainty"]^2) & /@ {values};
  combined = {w.x / Total[w], Total[w]^(-1/2)};
  chi2 = N[Total[MapThread[#2 (#1 - combined[[1]])^2&, {x, w}]]];
  chi2norm = chi2 / (Length[x] - 1);
  If[OptionValue["S"] === Automatic,
    s = If[chi2norm > 1, Sqrt[chi2norm], 1];
  ];
  Return[{Around[combined[[1]], s * combined[[2]]], "chi2"->chi2/s^2, "S"->s}]
]

UncertainCombine[a_UncertainValue, b_Around, c___, opt : OptionsPattern[]] := UncertainCombine[Around[a], b, c, opt]

(* Display forms *)
MakeBoxes[UncertainValue[x_, stat_, syst_], f:StandardForm|TraditionalForm] ^:= With[{
  box = TemplateBox[
    {MakeBoxes[x, f], MakeBoxes[stat, f], MakeBoxes[syst, f], N[Sqrt[syst^2+stat^2]] // MakeBoxes[#, f]&},
    "UncertainValue",
    DisplayFunction->(
      RowBox[{
        #1,
        StyleBox[RowBox[{
          "\[PlusMinus]\[NegativeThinSpace]", SuperscriptBox[#2, "stat"],
          "\[PlusMinus]\[NegativeThinSpace]", SuperscriptBox[#3, "syst"],
          "(", #4, ")"
        }], FontColor->GrayLevel[0.4]]
      }]
    &)
  ]},
  InterpretationBox[box, UncertainValue[x, stat, syst]]]

Protect[Evaluate[protected]]
End[];
EndPackage[];
