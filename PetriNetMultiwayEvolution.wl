(* ::Package:: *)

Options[PetriNetMultiwayEvolution] = Options[Graph]; 
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
     transitionCount = Length[transitions]; arcCount = Length[arcs]; tokenCount = Length[initialConditions]; 
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
makePetriNet[associationForm_Association] := PetriNetObject[KeySort[associationForm], 
    ConstantArray[0, Length[associationForm["Places"]]]] /; KeyExistsQ[associationForm, "Arcs"] && 
    KeyExistsQ[associationForm, "Places"] && KeyExistsQ[associationForm, "Transitions"]
makePetriNet[associationForm_Association, initialConditions_List] := 
  PetriNetObject[KeySort[associationForm], initialConditions] /; KeyExistsQ[associationForm, "Arcs"] && 
    KeyExistsQ[associationForm, "Places"] && KeyExistsQ[associationForm, "Transitions"] && 
    Length[initialConditions] == Length[associationForm["Places"]]
makePetriNet[places_List, transitions_List, arcs_List] := 
  PetriNetObject[Association["Arcs" -> arcs, "Places" -> places, "Transitions" -> transitions], 
   ConstantArray[0, Length[places]]]
makePetriNet[places_List, transitions_List, arcs_List, initialConditions_List] := 
  PetriNetObject[Association["Arcs" -> arcs, "Places" -> places, "Transitions" -> transitions], initialConditions] /; 
   Length[initialConditions] == Length[places]
transitionFiringQ[petriNet_List, transition_, currentTokens_List] := 
  Module[{tokenCount}, tokenCount = (Length[EdgeList[First[petriNet], DirectedEdge[#1, transition]]] & ) /@ 
      petriNet[[2]]; AllTrue[currentTokens - tokenCount, NonNegative]]
applyTransitionFiring[petriNet_List, transition_, currentTokens_List] := Module[{deletedTokensCount, addedTokensCount}, 
   deletedTokensCount = (Length[EdgeList[First[petriNet], DirectedEdge[#1, transition]]] & ) /@ petriNet[[2]]; 
    addedTokensCount = (Length[EdgeList[First[petriNet], DirectedEdge[transition, #1]]] & ) /@ petriNet[[2]]; 
    currentTokens - deletedTokensCount + addedTokensCount]
updateTransitionFiringMultiway[petriNet_List][currentTokens_List] := 
  Module[{transitionIndices, validTransitionIndices}, 
   transitionIndices = First /@ Position[(transitionFiringQ[petriNet, #1, currentTokens] & ) /@ petriNet[[3]], True]; 
    validTransitionIndices = Select[transitionIndices, IntegerQ]; 
    (applyTransitionFiring[petriNet, petriNet[[3,#1]], currentTokens] & ) /@ validTransitionIndices]
petriNetMultiwayEvolution[petriNet_List, initialConditions_List] := 
  Module[{currentTokens, transitionIndices, validTransitionIndices}, currentTokens = initialConditions; 
    currentTokens = updateTransitionFiringMultiway[petriNet][currentTokens]; 
    transitionIndices = Catenate[
      (Module[{tokens = #1}, First /@ Position[(transitionFiringQ[petriNet, #1, tokens] & ) /@ petriNet[[3]], 
           True]] & ) /@ currentTokens]; validTransitionIndices = Select[transitionIndices, IntegerQ]; 
    (Module[{tokens = #1}, (applyTransitionFiring[petriNet, petriNet[[3,#1]], tokens] & ) /@ 
        validTransitionIndices] & ) /@ currentTokens; 
    {DeleteDuplicates[(makePetriNet[petriNet[[2]], petriNet[[3]], petriNet[[4]], #1] & ) /@ currentTokens], 
     {petriNet, currentTokens}}]
petriNetMultiwayEvolution[multiwayStates_List] := Module[{multiwayEvolution}, 
   multiwayEvolution = (petriNetMultiwayEvolution[First[Last[multiwayStates]], #1] & ) /@ Last[Last[multiwayStates]]; 
    {DeleteDuplicates[Catenate[First /@ multiwayEvolution]], {First[Last[multiwayStates]], 
      DeleteDuplicates[Catenate[(Last[Last[#1]] & ) /@ multiwayEvolution]]}}]
petriNetMultiwayEvolutionLabeledGraphs[petriNet_List, initialConditions_List] := 
  Module[{currentTokens, transitionIndices, validTransitionIndices}, currentTokens = initialConditions; 
    currentTokens = updateTransitionFiringMultiway[petriNet][currentTokens]; 
    transitionIndices = Catenate[
      (Module[{tokens = #1}, First /@ Position[(transitionFiringQ[petriNet, #1, tokens] & ) /@ petriNet[[3]], 
           True]] & ) /@ currentTokens]; validTransitionIndices = Select[transitionIndices, IntegerQ]; 
    (Module[{tokens = #1}, (applyTransitionFiring[petriNet, petriNet[[3,#1]], tokens] & ) /@ 
        validTransitionIndices] & ) /@ currentTokens; 
    {DeleteDuplicates[(renderPetriNet[petriNet, #1] & ) /@ currentTokens], {petriNet, currentTokens}}]
petriNetMultiwayEvolutionLabeledGraphs[multiwayStates_List] := Module[{multiwayEvolution}, 
   multiwayEvolution = (petriNetMultiwayEvolutionLabeledGraphs[First[Last[multiwayStates]], #1] & ) /@ 
      Last[Last[multiwayStates]]; {DeleteDuplicates[Catenate[First /@ multiwayEvolution]], 
     {First[Last[multiwayStates]], DeleteDuplicates[Catenate[(Last[Last[#1]] & ) /@ multiwayEvolution]]}}]
petriNetMultiwayEvolutionLabeledGraphsHighlighted[petriNet_List, initialConditions_List] := 
  Module[{currentTokens, transitionIndices, validTransitionIndices, petriNetGraphs}, 
   currentTokens = initialConditions; currentTokens = updateTransitionFiringMultiway[petriNet][currentTokens]; 
    transitionIndices = Catenate[
      (Module[{tokens = #1}, First /@ Position[(transitionFiringQ[petriNet, #1, tokens] & ) /@ petriNet[[3]], 
           True]] & ) /@ currentTokens]; validTransitionIndices = Select[transitionIndices, IntegerQ]; 
    (Module[{tokens = #1}, (applyTransitionFiring[petriNet, petriNet[[3,#1]], tokens] & ) /@ 
        validTransitionIndices] & ) /@ currentTokens; petriNetGraphs = (renderPetriNet[petriNet, #1] & ) /@ 
      currentTokens; 
    {DeleteDuplicates[Catenate[(Module[{petriNetGraph = #1}, (HighlightGraph[petriNetGraph, {petriNet[[3,#1]]}] & ) /@ 
           validTransitionIndices] & ) /@ petriNetGraphs]], {petriNet, currentTokens}}]
petriNetMultiwayEvolutionLabeledGraphsHighlighted[multiwayStates_List] := 
  Module[{multiwayEvolution}, 
   multiwayEvolution = (petriNetMultiwayEvolutionLabeledGraphsHighlighted[First[Last[multiwayStates]], #1] & ) /@ 
      Last[Last[multiwayStates]]; {DeleteDuplicates[Catenate[First /@ multiwayEvolution]], 
     {First[Last[multiwayStates]], DeleteDuplicates[Catenate[(Last[Last[#1]] & ) /@ multiwayEvolution]]}}]
petriNetMultiwayEvolutionWeightedGraphs[petriNet_List, initialConditions_List] := 
  Module[{currentTokens, transitionIndices, validTransitionIndices}, currentTokens = initialConditions; 
    currentTokens = updateTransitionFiringMultiway[petriNet][currentTokens]; 
    transitionIndices = Catenate[
      (Module[{tokens = #1}, First /@ Position[(transitionFiringQ[petriNet, #1, tokens] & ) /@ petriNet[[3]], 
           True]] & ) /@ currentTokens]; validTransitionIndices = Select[transitionIndices, IntegerQ]; 
    (Module[{tokens = #1}, (applyTransitionFiring[petriNet, petriNet[[3,#1]], tokens] & ) /@ 
        validTransitionIndices] & ) /@ currentTokens; 
    {DeleteDuplicates[(renderPetriNetWeighted[petriNet, #1] & ) /@ currentTokens], {petriNet, currentTokens}}]
petriNetMultiwayEvolutionWeightedGraphs[multiwayStates_List] := Module[{multiwayEvolution}, 
   multiwayEvolution = (petriNetMultiwayEvolutionWeightedGraphs[First[Last[multiwayStates]], #1] & ) /@ 
      Last[Last[multiwayStates]]; {DeleteDuplicates[Catenate[First /@ multiwayEvolution]], 
     {First[Last[multiwayStates]], DeleteDuplicates[Catenate[(Last[Last[#1]] & ) /@ multiwayEvolution]]}}]
petriNetMultiwayEvolutionWeightedGraphsHighlighted[petriNet_List, initialConditions_List] := 
  Module[{currentTokens, transitionIndices, validTransitionIndices, petriNetGraphsWeighted}, 
   currentTokens = initialConditions; currentTokens = updateTransitionFiringMultiway[petriNet][currentTokens]; 
    transitionIndices = Catenate[
      (Module[{tokens = #1}, First /@ Position[(transitionFiringQ[petriNet, #1, tokens] & ) /@ petriNet[[3]], 
           True]] & ) /@ currentTokens]; validTransitionIndices = Select[transitionIndices, IntegerQ]; 
    (Module[{tokens = #1}, (applyTransitionFiring[petriNet, petriNet[[3,#1]], tokens] & ) /@ 
        validTransitionIndices] & ) /@ currentTokens; petriNetGraphsWeighted = 
     (renderPetriNetWeighted[petriNet, #1] & ) /@ currentTokens; 
    {DeleteDuplicates[Catenate[(Module[{petriNetGraphWeighted = #1}, 
          (HighlightGraph[petriNetGraphWeighted, {petriNet[[3,#1]]}] & ) /@ validTransitionIndices] & ) /@ 
        petriNetGraphsWeighted]], {petriNet, currentTokens}}]
petriNetMultiwayEvolutionWeightedGraphsHighlighted[multiwayStates_List] := 
  Module[{multiwayEvolution}, 
   multiwayEvolution = (petriNetMultiwayEvolutionWeightedGraphsHighlighted[First[Last[multiwayStates]], #1] & ) /@ 
      Last[Last[multiwayStates]]; {DeleteDuplicates[Catenate[First /@ multiwayEvolution]], 
     {First[Last[multiwayStates]], DeleteDuplicates[Catenate[(Last[Last[#1]] & ) /@ multiwayEvolution]]}}]
petriNetMultiwayEvolutionTokens[petriNet_List, initialConditions_List] := 
  Module[{currentTokens, transitionIndices, validTransitionIndices}, currentTokens = initialConditions; 
    currentTokens = updateTransitionFiringMultiway[petriNet][currentTokens]; 
    transitionIndices = Catenate[
      (Module[{tokens = #1}, First /@ Position[(transitionFiringQ[petriNet, #1, tokens] & ) /@ petriNet[[3]], 
           True]] & ) /@ currentTokens]; validTransitionIndices = Select[transitionIndices, IntegerQ]; 
    (Module[{tokens = #1}, (applyTransitionFiring[petriNet, petriNet[[3,#1]], tokens] & ) /@ 
        validTransitionIndices] & ) /@ currentTokens; {DeleteDuplicates[currentTokens], {petriNet, currentTokens}}]
petriNetMultiwayEvolutionTokens[multiwayStates_List] := Module[{multiwayEvolution}, 
   multiwayEvolution = (petriNetMultiwayEvolutionTokens[First[Last[multiwayStates]], #1] & ) /@ 
      Last[Last[multiwayStates]]; {DeleteDuplicates[Catenate[First /@ multiwayEvolution]], 
     {First[Last[multiwayStates]], DeleteDuplicates[Catenate[(Last[Last[#1]] & ) /@ multiwayEvolution]]}}]
petriNetMultiwayEvolutionTokenFirings[petriNet_List, initialConditions_List] := 
  Module[{currentTokens, transitionIndices, validTransitionIndices}, currentTokens = initialConditions; 
    currentTokens = updateTransitionFiringMultiway[petriNet][currentTokens]; 
    transitionIndices = Catenate[
      (Module[{tokens = #1}, First /@ Position[(transitionFiringQ[petriNet, #1, tokens] & ) /@ petriNet[[3]], 
           True]] & ) /@ currentTokens]; validTransitionIndices = Select[transitionIndices, IntegerQ]; 
    (Module[{tokens = #1}, (applyTransitionFiring[petriNet, petriNet[[3,#1]], tokens] & ) /@ 
        validTransitionIndices] & ) /@ currentTokens; 
    {DeleteDuplicates[Catenate[(Module[{tokens = #1}, ({tokens, petriNet[[3,#1]]} & ) /@ validTransitionIndices] & ) /@ 
        currentTokens]], {petriNet, currentTokens}}]
petriNetMultiwayEvolutionTokenFirings[multiwayStates_List] := Module[{multiwayEvolution}, 
   multiwayEvolution = (petriNetMultiwayEvolutionTokenFirings[First[Last[multiwayStates]], #1] & ) /@ 
      Last[Last[multiwayStates]]; {DeleteDuplicates[Catenate[First /@ multiwayEvolution]], 
     {First[Last[multiwayStates]], DeleteDuplicates[Catenate[(Last[Last[#1]] & ) /@ multiwayEvolution]]}}]
PetriNetMultiwayEvolution[associationForm_Association, options:OptionsPattern[]] := 
  PetriNetMultiwayEvolution[makePetriNet[associationForm], "PetriNetObjects", options] /; 
   KeyExistsQ[associationForm, "Arcs"] && KeyExistsQ[associationForm, "Places"] && 
    KeyExistsQ[associationForm, "Transitions"]
PetriNetMultiwayEvolution[associationForm_Association, initialConditions_List, options:OptionsPattern[]] := 
  PetriNetMultiwayEvolution[makePetriNet[associationForm, initialConditions], "PetriNetObjects", options] /; 
   KeyExistsQ[associationForm, "Arcs"] && KeyExistsQ[associationForm, "Places"] && 
    KeyExistsQ[associationForm, "Transitions"] && Length[initialConditions] == Length[associationForm["Places"]]
PetriNetMultiwayEvolution[associationForm_Association, stepCount_Integer, options:OptionsPattern[]] := 
  PetriNetMultiwayEvolution[makePetriNet[associationForm], stepCount, "PetriNetObjects", options] /; 
   KeyExistsQ[associationForm, "Arcs"] && KeyExistsQ[associationForm, "Places"] && 
    KeyExistsQ[associationForm, "Transitions"]
PetriNetMultiwayEvolution[associationForm_Association, initialConditions_List, stepCount_Integer, 
   options:OptionsPattern[]] := PetriNetMultiwayEvolution[makePetriNet[associationForm, initialConditions], stepCount, 
    "PetriNetObjects", options] /; KeyExistsQ[associationForm, "Arcs"] && KeyExistsQ[associationForm, "Places"] && 
    KeyExistsQ[associationForm, "Transitions"] && Length[initialConditions] == Length[associationForm["Places"]]
PetriNetMultiwayEvolution[associationForm_Association, property_String, options:OptionsPattern[]] := 
  PetriNetMultiwayEvolution[makePetriNet[associationForm], property, options] /; 
   KeyExistsQ[associationForm, "Arcs"] && KeyExistsQ[associationForm, "Places"] && 
    KeyExistsQ[associationForm, "Transitions"]
PetriNetMultiwayEvolution[associationForm_Association, initialConditions_List, property_String, 
   options:OptionsPattern[]] := PetriNetMultiwayEvolution[makePetriNet[associationForm, initialConditions], property, 
    options] /; KeyExistsQ[associationForm, "Arcs"] && KeyExistsQ[associationForm, "Places"] && 
    KeyExistsQ[associationForm, "Transitions"] && Length[initialConditions] == Length[associationForm["Places"]]
PetriNetMultiwayEvolution[associationForm_Association, stepCount_Integer, property_String, options:OptionsPattern[]] := 
  PetriNetMultiwayEvolution[makePetriNet[associationForm], stepCount, property, options] /; 
   KeyExistsQ[associationForm, "Arcs"] && KeyExistsQ[associationForm, "Places"] && 
    KeyExistsQ[associationForm, "Transitions"]
PetriNetMultiwayEvolution[associationForm_Association, initialConditions_List, stepCount_Integer, property_String, 
   options:OptionsPattern[]] := PetriNetMultiwayEvolution[makePetriNet[associationForm, initialConditions], stepCount, 
    property, options] /; KeyExistsQ[associationForm, "Arcs"] && KeyExistsQ[associationForm, "Places"] && 
    KeyExistsQ[associationForm, "Transitions"] && Length[initialConditions] == Length[associationForm["Places"]]
PetriNetMultiwayEvolution[places_List, transitions_List, arcs_List, options:OptionsPattern[]] := 
  PetriNetMultiwayEvolution[makePetriNet[places, transitions, arcs], "PetriNetObjects", options]
PetriNetMultiwayEvolution[places_List, transitions_List, arcs_List, initialConditions_List, 
   options:OptionsPattern[]] := PetriNetMultiwayEvolution[makePetriNet[places, transitions, arcs, initialConditions], 
    "PetriNetObjects", options] /; Length[initialConditions] == Length[places]
PetriNetMultiwayEvolution[places_List, transitions_List, arcs_List, stepCount_Integer, options:OptionsPattern[]] := 
  PetriNetMultiwayEvolution[makePetriNet[places, transitions, arcs], stepCount, "PetriNetObjects", options]
PetriNetMultiwayEvolution[places_List, transitions_List, arcs_List, initialConditions_List, stepCount_Integer, 
   options:OptionsPattern[]] := PetriNetMultiwayEvolution[makePetriNet[places, transitions, arcs, initialConditions], 
    stepCount, "PetriNetObjects", options] /; Length[initialConditions] == Length[places]
PetriNetMultiwayEvolution[places_List, transitions_List, arcs_List, property_String, options:OptionsPattern[]] := 
  PetriNetMultiwayEvolution[makePetriNet[places, transitions, arcs], property, options]
PetriNetMultiwayEvolution[places_List, transitions_List, arcs_List, initialConditions_List, property_String, 
   options:OptionsPattern[]] := PetriNetMultiwayEvolution[makePetriNet[places, transitions, arcs, initialConditions], 
    property, options] /; Length[initialConditions] == Length[places]
PetriNetMultiwayEvolution[places_List, transitions_List, arcs_List, stepCount_Integer, property_String, 
   options:OptionsPattern[]] := PetriNetMultiwayEvolution[makePetriNet[places, transitions, arcs], stepCount, property, 
   options]
PetriNetMultiwayEvolution[places_List, transitions_List, arcs_List, initialConditions_List, stepCount_Integer, 
   property_String, options:OptionsPattern[]] := 
  PetriNetMultiwayEvolution[makePetriNet[places, transitions, arcs, initialConditions], stepCount, property, 
    options] /; Length[initialConditions] == Length[places]
PetriNetMultiwayEvolution[(petriNetObject_)[Association["Arcs" -> arcs_List, "Places" -> places_List, 
     "Transitions" -> transitions_List], initialConditions_List], options:OptionsPattern[]] := 
  PetriNetMultiwayEvolution[PetriNetObject[Association["Arcs" -> arcs, "Places" -> places, 
      "Transitions" -> transitions], initialConditions], "PetriNetObjects", options] /; 
   SymbolName[petriNetObject] === "PetriNetObject"
PetriNetMultiwayEvolution[(petriNetObject_)[Association["Arcs" -> arcs_List, "Places" -> places_List, 
     "Transitions" -> transitions_List], initialConditions_List], stepCount_Integer, options:OptionsPattern[]] := 
  PetriNetMultiwayEvolution[PetriNetObject[Association["Arcs" -> arcs, "Places" -> places, 
      "Transitions" -> transitions], initialConditions], stepCount, "PetriNetObjects", options] /; 
   SymbolName[petriNetObject] === "PetriNetObject"
PetriNetMultiwayEvolution[(petriNetObject_)[Association["Arcs" -> arcs_List, "Places" -> places_List, 
     "Transitions" -> transitions_List], initialConditions_List], "PetriNetObjects", options:OptionsPattern[]] := 
  First[petriNetMultiwayEvolution[constructPetriNet[Association["Arcs" -> arcs, "Places" -> places, 
       "Transitions" -> transitions]], initialConditions]] /; SymbolName[petriNetObject] === "PetriNetObject"
PetriNetMultiwayEvolution[(petriNetObject_)[Association["Arcs" -> arcs_List, "Places" -> places_List, 
     "Transitions" -> transitions_List], initialConditions_List], stepCount_Integer, "PetriNetObjects", 
   options:OptionsPattern[]] := 
  First /@ NestList[petriNetMultiwayEvolution, 
     {{PetriNetObject[Association["Arcs" -> arcs, "Places" -> places, "Transitions" -> transitions], 
        initialConditions]}, {constructPetriNet[Association["Arcs" -> arcs, "Places" -> places, 
         "Transitions" -> transitions]], {initialConditions}}}, stepCount] /; 
   SymbolName[petriNetObject] === "PetriNetObject"
PetriNetMultiwayEvolution[(petriNetObject_)[Association["Arcs" -> arcs_List, "Places" -> places_List, 
     "Transitions" -> transitions_List], initialConditions_List], "LabeledGraphs", options:OptionsPattern[]] := 
  (Graph[#1, options] & ) /@ First[petriNetMultiwayEvolutionLabeledGraphs[
      constructPetriNet[Association["Arcs" -> arcs, "Places" -> places, "Transitions" -> transitions]], 
      initialConditions]] /; SymbolName[petriNetObject] === "PetriNetObject"
PetriNetMultiwayEvolution[(petriNetObject_)[Association["Arcs" -> arcs_List, "Places" -> places_List, 
     "Transitions" -> transitions_List], initialConditions_List], stepCount_Integer, "LabeledGraphs", 
   options:OptionsPattern[]] := (Module[{generation = #1}, (Graph[#1, options] & ) /@ generation] & ) /@ 
    First /@ NestList[petriNetMultiwayEvolutionLabeledGraphs, 
      {{renderPetriNet[constructPetriNet[Association["Arcs" -> arcs, "Places" -> places, 
           "Transitions" -> transitions]], initialConditions]}, 
       {constructPetriNet[Association["Arcs" -> arcs, "Places" -> places, "Transitions" -> transitions]], 
        {initialConditions}}}, stepCount] /; SymbolName[petriNetObject] === "PetriNetObject"
PetriNetMultiwayEvolution[(petriNetObject_)[Association["Arcs" -> arcs_List, "Places" -> places_List, 
     "Transitions" -> transitions_List], initialConditions_List], "LabeledGraphsHighlighted", 
   options:OptionsPattern[]] := (Graph[#1, options] & ) /@ First[petriNetMultiwayEvolutionLabeledGraphsHighlighted[
      constructPetriNet[Association["Arcs" -> arcs, "Places" -> places, "Transitions" -> transitions]], 
      initialConditions]] /; SymbolName[petriNetObject] === "PetriNetObject"
PetriNetMultiwayEvolution[(petriNetObject_)[Association["Arcs" -> arcs_List, "Places" -> places_List, 
     "Transitions" -> transitions_List], initialConditions_List], stepCount_Integer, "LabeledGraphsHighlighted", 
   options:OptionsPattern[]] := (Module[{generation = #1}, (Graph[#1, options] & ) /@ generation] & ) /@ 
    First /@ NestList[petriNetMultiwayEvolutionLabeledGraphsHighlighted, 
      {{renderPetriNet[constructPetriNet[Association["Arcs" -> arcs, "Places" -> places, 
           "Transitions" -> transitions]], initialConditions]}, 
       {constructPetriNet[Association["Arcs" -> arcs, "Places" -> places, "Transitions" -> transitions]], 
        {initialConditions}}}, stepCount] /; SymbolName[petriNetObject] === "PetriNetObject"
PetriNetMultiwayEvolution[(petriNetObject_)[Association["Arcs" -> arcs_List, "Places" -> places_List, 
     "Transitions" -> transitions_List], initialConditions_List], "WeightedGraphs", options:OptionsPattern[]] := 
  (Graph[#1, options] & ) /@ First[petriNetMultiwayEvolutionWeightedGraphs[
      constructPetriNet[Association["Arcs" -> arcs, "Places" -> places, "Transitions" -> transitions]], 
      initialConditions]] /; SymbolName[petriNetObject] === "PetriNetObject"
PetriNetMultiwayEvolution[(petriNetObject_)[Association["Arcs" -> arcs_List, "Places" -> places_List, 
     "Transitions" -> transitions_List], initialConditions_List], stepCount_Integer, "WeightedGraphs", 
   options:OptionsPattern[]] := (Module[{generation = #1}, (Graph[#1, options] & ) /@ generation] & ) /@ 
    First /@ NestList[petriNetMultiwayEvolutionWeightedGraphs, 
      {{renderPetriNetWeighted[constructPetriNet[Association["Arcs" -> arcs, "Places" -> places, 
           "Transitions" -> transitions]], initialConditions]}, 
       {constructPetriNet[Association["Arcs" -> arcs, "Places" -> places, "Transitions" -> transitions]], 
        {initialConditions}}}, stepCount] /; SymbolName[petriNetObject] === "PetriNetObject"
PetriNetMultiwayEvolution[(petriNetObject_)[Association["Arcs" -> arcs_List, "Places" -> places_List, 
     "Transitions" -> transitions_List], initialConditions_List], "WeightedGraphsHighlighted", 
   options:OptionsPattern[]] := (Graph[#1, options] & ) /@ First[petriNetMultiwayEvolutionWeightedGraphsHighlighted[
      constructPetriNet[Association["Arcs" -> arcs, "Places" -> places, "Transitions" -> transitions]], 
      initialConditions]] /; SymbolName[petriNetObject] === "PetriNetObject"
PetriNetMultiwayEvolution[(petriNetObject_)[Association["Arcs" -> arcs_List, "Places" -> places_List, 
     "Transitions" -> transitions_List], initialConditions_List], stepCount_Integer, "WeightedGraphsHighlighted", 
   options:OptionsPattern[]] := (Module[{generation = #1}, (Graph[#1, options] & ) /@ generation] & ) /@ 
    First /@ NestList[petriNetMultiwayEvolutionWeightedGraphsHighlighted, 
      {{renderPetriNetWeighted[constructPetriNet[Association["Arcs" -> arcs, "Places" -> places, 
           "Transitions" -> transitions]], initialConditions]}, 
       {constructPetriNet[Association["Arcs" -> arcs, "Places" -> places, "Transitions" -> transitions]], 
        {initialConditions}}}, stepCount] /; SymbolName[petriNetObject] === "PetriNetObject"
PetriNetMultiwayEvolution[(petriNetObject_)[Association["Arcs" -> arcs_List, "Places" -> places_List, 
     "Transitions" -> transitions_List], initialConditions_List], "Tokens", options:OptionsPattern[]] := 
  First[petriNetMultiwayEvolutionTokens[constructPetriNet[Association["Arcs" -> arcs, "Places" -> places, 
       "Transitions" -> transitions]], initialConditions]] /; SymbolName[petriNetObject] === "PetriNetObject"
PetriNetMultiwayEvolution[(petriNetObject_)[Association["Arcs" -> arcs_List, "Places" -> places_List, 
     "Transitions" -> transitions_List], initialConditions_List], stepCount_Integer, "Tokens", 
   options:OptionsPattern[]] := First /@ NestList[petriNetMultiwayEvolutionTokens, 
     {{initialConditions}, {constructPetriNet[Association["Arcs" -> arcs, "Places" -> places, 
         "Transitions" -> transitions]], {initialConditions}}}, stepCount] /; 
   SymbolName[petriNetObject] === "PetriNetObject"
PetriNetMultiwayEvolution[(petriNetObject_)[Association["Arcs" -> arcs_List, "Places" -> places_List, 
     "Transitions" -> transitions_List], initialConditions_List], "TokenFirings", options:OptionsPattern[]] := 
  First[petriNetMultiwayEvolutionTokenFirings[constructPetriNet[Association["Arcs" -> arcs, "Places" -> places, 
       "Transitions" -> transitions]], initialConditions]] /; SymbolName[petriNetObject] === "PetriNetObject"
PetriNetMultiwayEvolution[(petriNetObject_)[Association["Arcs" -> arcs_List, "Places" -> places_List, 
     "Transitions" -> transitions_List], initialConditions_List], stepCount_Integer, "TokenFirings", 
   options:OptionsPattern[]] := First /@ NestList[petriNetMultiwayEvolutionTokenFirings, 
     {{{initialConditions, Null}}, {constructPetriNet[Association["Arcs" -> arcs, "Places" -> places, 
         "Transitions" -> transitions]], {initialConditions}}}, stepCount] /; 
   SymbolName[petriNetObject] === "PetriNetObject"
