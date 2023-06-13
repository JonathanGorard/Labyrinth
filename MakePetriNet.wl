(* ::Package:: *)

buildTextDot[vertex_, dotsCount_Integer, coordinateAssociation_Association] := 
  Text[StringRepeat["\[FilledSmallCircle]", dotsCount], coordinateAssociation[vertex]]
buildTextDotsList[coordinateAssociation_Association, vertexWeightAssociation_Association, vertices_List] := 
  Module[{dotsList}, dotsList = {}; Scan[AppendTo[dotsList, buildTextDot[#1, vertexWeightAssociation[#1], 
        coordinateAssociation]] & , vertices]; dotsList]
applyBooleanOperatorToList[booleanOperator_, list_List] := Fold[booleanOperator, First[list], Rest[list]]
petriNetQ[Association["Arcs" -> arcs_List, "Places" -> places_List, "Transitions" -> transitions_List]] := 
  Module[{petriNetGraph}, petriNetGraph = Graph[Apply[DirectedEdge, arcs, {1}]]; 
    applyBooleanOperatorToList[And, {Intersection[places, transitions] == {}, BipartiteGraphQ[petriNetGraph]}]]
constructPetriNet[Association["Arcs" -> arcs_List, "Places" -> places_List, "Transitions" -> transitions_List]] := 
  If[petriNetQ[Association["Arcs" -> arcs, "Places" -> places, "Transitions" -> transitions]], 
   {Graph[Join[places, transitions], Apply[DirectedEdge, arcs, {1}], VertexShapeFunction -> 
      Join[Thread[places -> "Circle"], Thread[transitions -> "Square"]], VertexLabels -> "Name", 
     VertexStyle -> Join[Thread[places -> LightBlue], Thread[transitions -> LightGray]], VertexSize -> Large], places, 
    transitions, Apply[DirectedEdge, arcs, {1}]}, $Failed]
renderPetriNet[petriNet_List, initialConditions_List] := 
  Module[{petriNetGraph, coordinateAssociation, vertexWeightAssociation, graphData, vertexWeight, graphEpilog}, 
   petriNetGraph = Graph[First[petriNet], VertexWeight -> Join[Thread[petriNet[[2]] -> initialConditions], 
        Thread[petriNet[[3]] -> 0]]]; graphData = ToExpression[StringReplace[ToString[FullForm[petriNetGraph]], 
       "Graph" -> "List"]]; vertexWeight = Last[Last[graphData[[3]]]]; 
    coordinateAssociation = Association[Thread[graphData[[1]] -> VertexCoordinates /. 
        AbsoluteOptions[petriNetGraph, VertexCoordinates]]]; vertexWeightAssociation = 
     Association[Thread[graphData[[1]] -> vertexWeight]]; graphEpilog = buildTextDotsList[coordinateAssociation, 
      vertexWeightAssociation, graphData[[1]]]; Graph[petriNetGraph, Epilog -> graphEpilog]]
renderPetriNetWeighted[petriNet_List, initialConditions_List] := 
  Graph[First[petriNet], VertexWeight -> Join[Thread[petriNet[[2]] -> initialConditions], Thread[petriNet[[3]] -> 0]]]
PetriNetObject /: MakeBoxes[petriNet:PetriNetObject[Association["Arcs" -> arcs_List, "Places" -> places_List, 
       "Transitions" -> transitions_List], initialConditions_List], format_] := 
   Module[{placeCount, transitionCount, arcCount, tokenCount}, placeCount = Length[places]; 
     transitionCount = Length[transitions]; arcCount = Length[arcs]; tokenCount = Total[initialConditions]; 
     BoxForm`ArrangeSummaryBox["PetriNetObject", petriNet, 
      GraphPlot[First[constructPetriNet[Association["Arcs" -> arcs, "Places" -> places, 
          "Transitions" -> transitions]]], VertexLabels -> None, EdgeShapeFunction -> 
        GraphElementData["ShortFilledArrow", "ArrowSize" -> 0.05]], {{BoxForm`SummaryItem[{"Places: ", placeCount}]}, 
       {BoxForm`SummaryItem[{"Transitions: ", transitionCount}]}, {BoxForm`SummaryItem[{"Arcs: ", arcCount}]}}, 
      {{BoxForm`SummaryItem[{"Tokens: ", tokenCount}]}}, format, "Interpretable" -> Automatic]]
PetriNetObject[Association["Arcs" -> arcs_List, "Places" -> places_List, "Transitions" -> transitions_List], 
    initialConditions_List]["AssociationForm"] := Association["Places" -> places, "Transitions" -> transitions, 
   "Arcs" -> arcs]
PetriNetObject[Association["Arcs" -> arcs_List, "Places" -> places_List, "Transitions" -> transitions_List], 
    initialConditions_List]["Places"] := places
PetriNetObject[Association["Arcs" -> arcs_List, "Places" -> places_List, "Transitions" -> transitions_List], 
    initialConditions_List]["PlaceCount"] := Length[places]
PetriNetObject[Association["Arcs" -> arcs_List, "Places" -> places_List, "Transitions" -> transitions_List], 
    initialConditions_List]["Transitions"] := transitions
PetriNetObject[Association["Arcs" -> arcs_List, "Places" -> places_List, "Transitions" -> transitions_List], 
    initialConditions_List]["TransitionCount"] := Length[transitions]
PetriNetObject[Association["Arcs" -> arcs_List, "Places" -> places_List, "Transitions" -> transitions_List], 
    initialConditions_List]["Arcs"] := arcs
PetriNetObject[Association["Arcs" -> arcs_List, "Places" -> places_List, "Transitions" -> transitions_List], 
    initialConditions_List]["ArcCount"] := Length[arcs]
PetriNetObject[Association["Arcs" -> arcs_List, "Places" -> places_List, "Transitions" -> transitions_List], 
    initialConditions_List]["Tokens"] := initialConditions
PetriNetObject[Association["Arcs" -> arcs_List, "Places" -> places_List, "Transitions" -> transitions_List], 
    initialConditions_List]["TokenCount"] := Total[initialConditions]
PetriNetObject[Association["Arcs" -> arcs_List, "Places" -> places_List, "Transitions" -> transitions_List], 
    initialConditions_List]["UnlabeledGraph"] := 
  First[constructPetriNet[Association["Arcs" -> arcs, "Places" -> places, "Transitions" -> transitions]]]
PetriNetObject[Association["Arcs" -> arcs_List, "Places" -> places_List, "Transitions" -> transitions_List], 
    initialConditions_List]["LabeledGraph"] := 
  renderPetriNet[constructPetriNet[Association["Arcs" -> arcs, "Places" -> places, "Transitions" -> transitions]], 
   initialConditions]
PetriNetObject[Association["Arcs" -> arcs_List, "Places" -> places_List, "Transitions" -> transitions_List], 
    initialConditions_List]["WeightedGraph"] := renderPetriNetWeighted[
   constructPetriNet[Association["Arcs" -> arcs, "Places" -> places, "Transitions" -> transitions]], initialConditions]
PetriNetObject[Association["Arcs" -> arcs_List, "Places" -> places_List, "Transitions" -> transitions_List], 
    initialConditions_List]["Properties"] := {"AssociationForm", "Places", "PlaceCount", "Transitions", 
   "TransitionCount", "Arcs", "ArcCount", "Tokens", "TokenCount", "UnlabeledGraph", "LabeledGraph", "WeightedGraph"}
MakePetriNet[associationForm_Association] := PetriNetObject[KeySort[associationForm], 
    ConstantArray[0, Length[associationForm["Places"]]]] /; KeyExistsQ[associationForm, "Arcs"] && 
    KeyExistsQ[associationForm, "Places"] && KeyExistsQ[associationForm, "Transitions"]
MakePetriNet[associationForm_Association, initialConditions_List] := 
  PetriNetObject[KeySort[associationForm], initialConditions] /; KeyExistsQ[associationForm, "Arcs"] && 
    KeyExistsQ[associationForm, "Places"] && KeyExistsQ[associationForm, "Transitions"] && 
    Length[initialConditions] == Length[associationForm["Places"]]
MakePetriNet[places_List, transitions_List, arcs_List] := 
  PetriNetObject[Association["Arcs" -> arcs, "Places" -> places, "Transitions" -> transitions], 
   ConstantArray[0, Length[places]]]
MakePetriNet[places_List, transitions_List, arcs_List, initialConditions_List] := 
  PetriNetObject[Association["Arcs" -> arcs, "Places" -> places, "Transitions" -> transitions], initialConditions] /; 
   Length[initialConditions] == Length[places]
