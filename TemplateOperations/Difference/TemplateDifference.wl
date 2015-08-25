(* ::Package:: *)

BeginPackage[
  "CATemplates`TemplateOperations`Difference`TemplateDifference`",
  {
    "CATemplates`Basic`", 
    "CATemplates`TemplateOperations`Intersection`Common`",
    "CATemplates`TemplateOperations`Intersection`TemplateIntersection`"}];


TemplateDifference::usage= "bl1";
DualEquationSystem::usage= "bl2";
DifferenceReplacementRules::usage= "bl3";


Begin["`Private`"];


(*
DualEquationSystem[t1_,t2_]:=
With[{
equationSystem =EquationSystem[t1,t2]
},
	Select[equationSystem,!TrueQ[#]&];(*Remove tautologies*)
	Map[Part[#,1]==1-(Part[#,2])&,%];(*Apply Not to all variables*)
	Or@@(#&/@%) (*Change AND for OR*)
]
*)

(*
TemplateDifference[template1_,template2_,r_:1]:=
	Join[template1/.Solve[DualEquationSystem[template1,template2]],ExceptionTemplates[template2,2,r]];
*)

TemplateDifference[template1_List, template2_List, radius_: 1] :=
 Module[{templateIntersection, exceptionTemplates, templateDifferenceP1, templateDifferenceP2, templateDifference, replacementRules, replacementRulesFinal},
  templateIntersection = TemplateIntersection[template1, template2]//Flatten;
  If[!ValidTemplateQ[templateIntersection] || templateIntersection === {},template1,
	replacementRules = DifferenceReplacementRules[template1, templateIntersection];
	replacementRulesFinal = Select[replacementRules, FreeQ[#, _Rational] &];(*ATENÇÃO: Aqui remove todas as regras que contenham números racionais*)
	(*Print["templateIntersection", templateIntersection, "replacementRules", replacementRules, "replacementRulesFinal", replacementRulesFinal];*)

	If[replacementRulesFinal == {}, {},
	 templateDifferenceP1 = template1 /. replacementRulesFinal;(*Apply*)
	 exceptionTemplates = ExceptionTemplates[template2, 2, radius];
	 templateDifferenceP2 = TemplateIntersection[template1, #] & /@ exceptionTemplates;
	 (*Print["templateDifferenceP1", templateDifferenceP1, "templateDifferenceP2", templateDifferenceP2];*)
	 templateDifference = Join[templateDifferenceP1, templateDifferenceP2]
   ]
  ]
 ]

DualEquationSystem[template1_List, template2_List] :=
 Module[{notTemplate2, equationSystem, dualEquationSystem},
  (*notTemplate2 = 1 - t2; Change 0 to 1 and 1 to 0*)
  equationSystem = EquationSystem[template1,template2];(*Make a equation System*)
  cleanEquationSystem = Select[equationSystem,!TrueQ[#]&];(*Remove tautologies*)
  dualEquationSystem = Map[Part[#,1]==1-(Part[#,2])&,cleanEquationSystem];(*Apply Not to all variables*)
  dualEquationSystemResult = Or @@ (# & /@ dualEquationSystem) (*Change boolean operator AND to OR*)
  ]

DifferenceReplacementRules[template1_, template2_, k_Integer: 0] :=
  Module[{templateVars = SortBy[RuleTemplateVars[template1], FromDigits[StringDrop[SymbolName[#], 1]] &]},
	If[k === 0,
		Quiet[Solve[DualEquationSystem[template1, template2], templateVars]]//Union,
		Quiet[Solve[DualEquationSystem[template1, template2], Reverse[templateVars], Modulus -> k]]//Union
	]
];

End[];
EndPackage[];
