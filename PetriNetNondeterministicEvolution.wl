(* ::Package:: *)

Options[PetriNetNondeterministicEvolution] = Options[Graph]; 
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
updateTransitionFiringNondeterministic[petriNet_List][currentTokens_List] := 
  Module[{transitionIndices, validTransitionIndices}, 
   transitionIndices = First /@ Position[(transitionFiringQ[petriNet, #1, currentTokens] & ) /@ petriNet[[3]], True]; 
    validTransitionIndices = Select[transitionIndices, IntegerQ]; 
    (applyTransitionFiring[petriNet, petriNet[[3,#1]], currentTokens] & ) /@ validTransitionIndices]
petriNetNondeterministicEvolution[petriNet_List, initialConditions_List] := 
  Module[{currentTokens, transitionIndices, validTransitionIndices, currentToken}, 
   currentTokens = initialConditions; currentTokens = updateTransitionFiringNondeterministic[petriNet][currentTokens]; 
    transitionIndices = Catenate[
      (Module[{tokens = #1}, First /@ Position[(transitionFiringQ[petriNet, #1, tokens] & ) /@ petriNet[[3]], 
           True]] & ) /@ currentTokens]; validTransitionIndices = Select[transitionIndices, IntegerQ]; 
    (Module[{tokens = #1}, (applyTransitionFiring[petriNet, petriNet[[3,#1]], tokens] & ) /@ 
        validTransitionIndices] & ) /@ currentTokens; If[Length[currentTokens] > 0, 
     currentToken = RandomChoice[currentTokens]; {makePetriNet[petriNet[[2]], petriNet[[3]], petriNet[[4]], 
        currentToken], {petriNet, currentToken}}, {makePetriNet[petriNet[[2]], petriNet[[3]], petriNet[[4]], 
       initialConditions], {petriNet, initialConditions}}]]
petriNetNondeterministicEvolution[nondeterministicStates_List] := Module[{nondeterministicEvolution}, 
   nondeterministicEvolution = petriNetNondeterministicEvolution[First[Last[nondeterministicStates]], 
      Last[Last[nondeterministicStates]]]; {First[nondeterministicEvolution], {First[Last[nondeterministicStates]], 
      Last[Last[nondeterministicEvolution]]}}]
petriNetNondeterministicEvolutionLabeledGraphs[petriNet_List, initialConditions_List] := 
  Module[{currentTokens, transitionIndices, validTransitionIndices, currentToken}, 
   currentTokens = initialConditions; currentTokens = updateTransitionFiringNondeterministic[petriNet][currentTokens]; 
    transitionIndices = Catenate[
      (Module[{tokens = #1}, First /@ Position[(transitionFiringQ[petriNet, #1, tokens] & ) /@ petriNet[[3]], 
           True]] & ) /@ currentTokens]; validTransitionIndices = Select[transitionIndices, IntegerQ]; 
    (Module[{tokens = #1}, (applyTransitionFiring[petriNet, petriNet[[3,#1]], tokens] & ) /@ 
        validTransitionIndices] & ) /@ currentTokens; If[Length[currentTokens] > 0, 
     currentToken = RandomChoice[currentTokens]; {renderPetriNet[petriNet, currentToken], {petriNet, currentToken}}, 
     {renderPetriNet[petriNet, initialConditions], {petriNet, initialConditions}}]]
petriNetNondeterministicEvolutionLabeledGraphs[nondeterministicStates_List] := 
  Module[{nondeterministicEvolution}, nondeterministicEvolution = petriNetNondeterministicEvolutionLabeledGraphs[
      First[Last[nondeterministicStates]], Last[Last[nondeterministicStates]]]; 
    {First[nondeterministicEvolution], {First[Last[nondeterministicStates]], Last[Last[nondeterministicEvolution]]}}]
petriNetNondeterministicEvolutionLabeledGraphsHighlighted[petriNet_List, initialConditions_List] := 
  Module[{currentTokens, transitionIndices, validTransitionIndices, currentToken, validTransitionIndex, petriNetGraph}, 
   currentTokens = initialConditions; currentTokens = updateTransitionFiringNondeterministic[petriNet][currentTokens]; 
    transitionIndices = Catenate[
      (Module[{tokens = #1}, First /@ Position[(transitionFiringQ[petriNet, #1, tokens] & ) /@ petriNet[[3]], 
           True]] & ) /@ currentTokens]; validTransitionIndices = Select[transitionIndices, IntegerQ]; 
    (Module[{tokens = #1}, (applyTransitionFiring[petriNet, petriNet[[3,#1]], tokens] & ) /@ 
        validTransitionIndices] & ) /@ currentTokens; 
    If[Length[currentTokens] > 0 && Length[validTransitionIndices] > 0, currentToken = RandomChoice[currentTokens]; 
      validTransitionIndex = RandomChoice[validTransitionIndices]; 
      petriNetGraph = renderPetriNet[petriNet, currentToken]; 
      {HighlightGraph[petriNetGraph, {petriNet[[3,validTransitionIndex]]}], {petriNet, currentToken}}, 
     {renderPetriNet[petriNet, initialConditions], {petriNet, initialConditions}}]]
petriNetNondeterministicEvolutionLabeledGraphsHighlighted[nondeterministicStates_List] := 
  Module[{nondeterministicEvolution}, nondeterministicEvolution = 
     petriNetNondeterministicEvolutionLabeledGraphsHighlighted[First[Last[nondeterministicStates]], 
      Last[Last[nondeterministicStates]]]; {First[nondeterministicEvolution], {First[Last[nondeterministicStates]], 
      Last[Last[nondeterministicEvolution]]}}]
petriNetNondeterministicEvolutionWeightedGraphs[petriNet_List, initialConditions_List] := 
  Module[{currentTokens, transitionIndices, validTransitionIndices, currentToken}, 
   currentTokens = initialConditions; currentTokens = updateTransitionFiringNondeterministic[petriNet][currentTokens]; 
    transitionIndices = Catenate[
      (Module[{tokens = #1}, First /@ Position[(transitionFiringQ[petriNet, #1, tokens] & ) /@ petriNet[[3]], 
           True]] & ) /@ currentTokens]; validTransitionIndices = Select[transitionIndices, IntegerQ]; 
    (Module[{tokens = #1}, (applyTransitionFiring[petriNet, petriNet[[3,#1]], tokens] & ) /@ 
        validTransitionIndices] & ) /@ currentTokens; If[Length[currentTokens] > 0, 
     currentToken = RandomChoice[currentTokens]; {renderPetriNetWeighted[petriNet, currentToken], 
       {petriNet, currentToken}}, {renderPetriNetWeighted[petriNet, initialConditions], {petriNet, initialConditions}}]]
petriNetNondeterministicEvolutionWeightedGraphs[nondeterministicStates_List] := 
  Module[{nondeterministicEvolution}, nondeterministicEvolution = petriNetNondeterministicEvolutionWeightedGraphs[
      First[Last[nondeterministicStates]], Last[Last[nondeterministicStates]]]; 
    {First[nondeterministicEvolution], {First[Last[nondeterministicStates]], Last[Last[nondeterministicEvolution]]}}]
petriNetNondeterministicEvolutionWeightedGraphsHighlighted[petriNet_List, initialConditions_List] := 
  Module[{currentTokens, transitionIndices, validTransitionIndices, currentToken, validTransitionIndex, 
    petriNetGraphWeighted}, currentTokens = initialConditions; 
    currentTokens = updateTransitionFiringNondeterministic[petriNet][currentTokens]; 
    transitionIndices = Catenate[
      (Module[{tokens = #1}, First /@ Position[(transitionFiringQ[petriNet, #1, tokens] & ) /@ petriNet[[3]], 
           True]] & ) /@ currentTokens]; validTransitionIndices = Select[transitionIndices, IntegerQ]; 
    (Module[{tokens = #1}, (applyTransitionFiring[petriNet, petriNet[[3,#1]], tokens] & ) /@ 
        validTransitionIndices] & ) /@ currentTokens; 
    If[Length[currentTokens] > 0 && Length[validTransitionIndices] > 0, currentToken = RandomChoice[currentTokens]; 
      validTransitionIndex = RandomChoice[validTransitionIndices]; petriNetGraphWeighted = 
       renderPetriNetWeighted[petriNet, currentToken]; {HighlightGraph[petriNetGraphWeighted, 
        {petriNet[[3,validTransitionIndex]]}], {petriNet, currentToken}}, 
     {renderPetriNetWeighted[petriNet, initialConditions], {petriNet, initialConditions}}]]
petriNetNondeterministicEvolutionWeightedGraphsHighlighted[nondeterministicStates_List] := 
  Module[{nondeterministicEvolution}, nondeterministicEvolution = 
     petriNetNondeterministicEvolutionWeightedGraphsHighlighted[First[Last[nondeterministicStates]], 
      Last[Last[nondeterministicStates]]]; {First[nondeterministicEvolution], {First[Last[nondeterministicStates]], 
      Last[Last[nondeterministicEvolution]]}}]
petriNetNondeterministicEvolutionTokens[petriNet_List, initialConditions_List] := 
  Module[{currentTokens, transitionIndices, validTransitionIndices, currentToken}, 
   currentTokens = initialConditions; currentTokens = updateTransitionFiringNondeterministic[petriNet][currentTokens]; 
    transitionIndices = Catenate[
      (Module[{tokens = #1}, First /@ Position[(transitionFiringQ[petriNet, #1, tokens] & ) /@ petriNet[[3]], 
           True]] & ) /@ currentTokens]; validTransitionIndices = Select[transitionIndices, IntegerQ]; 
    (Module[{tokens = #1}, (applyTransitionFiring[petriNet, petriNet[[3,#1]], tokens] & ) /@ 
        validTransitionIndices] & ) /@ currentTokens; If[Length[currentTokens] > 0, 
     currentToken = RandomChoice[currentTokens]; {currentToken, {petriNet, currentToken}}, 
     {initialConditions, {petriNet, initialConditions}}]]
petriNetNondeterministicEvolutionTokens[nondeterministicStates_List] := 
  Module[{nondeterministicEvolution}, nondeterministicEvolution = petriNetNondeterministicEvolutionTokens[
      First[Last[nondeterministicStates]], Last[Last[nondeterministicStates]]]; 
    {First[nondeterministicEvolution], {First[Last[nondeterministicStates]], Last[Last[nondeterministicEvolution]]}}]
petriNetNondeterministicEvolutionTokenFirings[petriNet_List, initialConditions_List] := 
  Module[{currentTokens, transitionIndices, validTransitionIndices, currentToken, validTransitionIndex}, 
   currentTokens = initialConditions; currentTokens = updateTransitionFiringNondeterministic[petriNet][currentTokens]; 
    transitionIndices = Catenate[
      (Module[{tokens = #1}, First /@ Position[(transitionFiringQ[petriNet, #1, tokens] & ) /@ petriNet[[3]], 
           True]] & ) /@ currentTokens]; validTransitionIndices = Select[transitionIndices, IntegerQ]; 
    (Module[{tokens = #1}, (applyTransitionFiring[petriNet, petriNet[[3,#1]], tokens] & ) /@ 
        validTransitionIndices] & ) /@ currentTokens; 
    If[Length[currentTokens] > 0 && Length[validTransitionIndices] > 0, currentToken = RandomChoice[currentTokens]; 
      validTransitionIndex = RandomChoice[validTransitionIndices]; {{currentToken, petriNet[[3,validTransitionIndex]]}, 
       {petriNet, currentToken}}, {{initialConditions, Null}, {petriNet, initialConditions}}]]
petriNetNondeterministicEvolutionTokenFirings[nondeterministicStates_List] := 
  Module[{nondeterministicEvolution}, nondeterministicEvolution = petriNetNondeterministicEvolutionTokenFirings[
      First[Last[nondeterministicStates]], Last[Last[nondeterministicStates]]]; 
    {First[nondeterministicEvolution], {First[Last[nondeterministicStates]], Last[Last[nondeterministicEvolution]]}}]
PetriNetNondeterministicEvolution[associationForm_Association, options:OptionsPattern[]] := 
  PetriNetNondeterministicEvolution[makePetriNet[associationForm], "PetriNetObjects", options] /; 
   KeyExistsQ[associationForm, "Arcs"] && KeyExistsQ[associationForm, "Places"] && 
    KeyExistsQ[associationForm, "Transitions"]
PetriNetNondeterministicEvolution[associationForm_Association, initialConditions_List, options:OptionsPattern[]] := 
  PetriNetNondeterministicEvolution[makePetriNet[associationForm, initialConditions], "PetriNetObjects", options] /; 
   KeyExistsQ[associationForm, "Arcs"] && KeyExistsQ[associationForm, "Places"] && 
    KeyExistsQ[associationForm, "Transitions"] && Length[initialConditions] == Length[associationForm["Places"]]
PetriNetNondeterministicEvolution[associationForm_Association, stepCount_Integer, options:OptionsPattern[]] := 
  PetriNetNondeterministicEvolution[makePetriNet[associationForm], stepCount, "PetriNetObjects", options] /; 
   KeyExistsQ[associationForm, "Arcs"] && KeyExistsQ[associationForm, "Places"] && 
    KeyExistsQ[associationForm, "Transitions"]
PetriNetNondeterministicEvolution[associationForm_Association, initialConditions_List, stepCount_Integer, 
   options:OptionsPattern[]] := PetriNetNondeterministicEvolution[makePetriNet[associationForm, initialConditions], 
    stepCount, "PetriNetObjects", options] /; KeyExistsQ[associationForm, "Arcs"] && 
    KeyExistsQ[associationForm, "Places"] && KeyExistsQ[associationForm, "Transitions"] && 
    Length[initialConditions] == Length[associationForm["Places"]]
PetriNetNondeterministicEvolution[associationForm_Association, property_String, options:OptionsPattern[]] := 
  PetriNetNondeterministicEvolution[makePetriNet[associationForm], property, options] /; 
   KeyExistsQ[associationForm, "Arcs"] && KeyExistsQ[associationForm, "Places"] && 
    KeyExistsQ[associationForm, "Transitions"]
PetriNetNondeterministicEvolution[associationForm_Association, initialConditions_List, property_String, 
   options:OptionsPattern[]] := PetriNetNondeterministicEvolution[makePetriNet[associationForm, initialConditions], 
    property, options] /; KeyExistsQ[associationForm, "Arcs"] && KeyExistsQ[associationForm, "Places"] && 
    KeyExistsQ[associationForm, "Transitions"] && Length[initialConditions] == Length[associationForm["Places"]]
PetriNetNondeterministicEvolution[associationForm_Association, stepCount_Integer, property_String, 
   options:OptionsPattern[]] := PetriNetNondeterministicEvolution[makePetriNet[associationForm], stepCount, property, 
    options] /; KeyExistsQ[associationForm, "Arcs"] && KeyExistsQ[associationForm, "Places"] && 
    KeyExistsQ[associationForm, "Transitions"]
PetriNetNondeterministicEvolution[associationForm_Association, initialConditions_List, stepCount_Integer, 
   property_String, options:OptionsPattern[]] := 
  PetriNetNondeterministicEvolution[makePetriNet[associationForm, initialConditions], stepCount, property, options] /; 
   KeyExistsQ[associationForm, "Arcs"] && KeyExistsQ[associationForm, "Places"] && 
    KeyExistsQ[associationForm, "Transitions"] && Length[initialConditions] == Length[associationForm["Places"]]
PetriNetNondeterministicEvolution[places_List, transitions_List, arcs_List, options:OptionsPattern[]] := 
  PetriNetNondeterministicEvolution[makePetriNet[places, transitions, arcs], "PetriNetObjects", options]
PetriNetNondeterministicEvolution[places_List, transitions_List, arcs_List, initialConditions_List, 
   options:OptionsPattern[]] := PetriNetNondeterministicEvolution[makePetriNet[places, transitions, arcs, 
     initialConditions], "PetriNetObjects", options] /; Length[initialConditions] == Length[places]
PetriNetNondeterministicEvolution[places_List, transitions_List, arcs_List, stepCount_Integer, 
   options:OptionsPattern[]] := PetriNetNondeterministicEvolution[makePetriNet[places, transitions, arcs], stepCount, 
   "PetriNetObjects", options]
PetriNetNondeterministicEvolution[places_List, transitions_List, arcs_List, initialConditions_List, stepCount_Integer, 
   options:OptionsPattern[]] := PetriNetNondeterministicEvolution[makePetriNet[places, transitions, arcs, 
     initialConditions], stepCount, "PetriNetObjects", options] /; Length[initialConditions] == Length[places]
PetriNetNondeterministicEvolution[places_List, transitions_List, arcs_List, property_String, 
   options:OptionsPattern[]] := PetriNetNondeterministicEvolution[makePetriNet[places, transitions, arcs], property, 
   options]
PetriNetNondeterministicEvolution[places_List, transitions_List, arcs_List, initialConditions_List, property_String, 
   options:OptionsPattern[]] := PetriNetNondeterministicEvolution[makePetriNet[places, transitions, arcs, 
     initialConditions], property, options] /; Length[initialConditions] == Length[places]
PetriNetNondeterministicEvolution[places_List, transitions_List, arcs_List, stepCount_Integer, property_String, 
   options:OptionsPattern[]] := PetriNetNondeterministicEvolution[makePetriNet[places, transitions, arcs], stepCount, 
   property, options]
PetriNetNondeterministicEvolution[places_List, transitions_List, arcs_List, initialConditions_List, stepCount_Integer, 
   property_String, options:OptionsPattern[]] := 
  PetriNetNondeterministicEvolution[makePetriNet[places, transitions, arcs, initialConditions], stepCount, property, 
    options] /; Length[initialConditions] == Length[places]
PetriNetNondeterministicEvolution[(petriNetObject_)[Association["Arcs" -> arcs_List, "Places" -> places_List, 
     "Transitions" -> transitions_List], initialConditions_List], options:OptionsPattern[]] := 
  PetriNetNondeterministicEvolution[PetriNetObject[Association["Arcs" -> arcs, "Places" -> places, 
      "Transitions" -> transitions], initialConditions], "PetriNetObjects", options] /; 
   SymbolName[petriNetObject] === "PetriNetObject"
PetriNetNondeterministicEvolution[(petriNetObject_)[Association["Arcs" -> arcs_List, "Places" -> places_List, 
     "Transitions" -> transitions_List], initialConditions_List], stepCount_Integer, options:OptionsPattern[]] := 
  PetriNetNondeterministicEvolution[PetriNetObject[Association["Arcs" -> arcs, "Places" -> places, 
      "Transitions" -> transitions], initialConditions], stepCount, "PetriNetObjects", options] /; 
   SymbolName[petriNetObject] === "PetriNetObject"
PetriNetNondeterministicEvolution[(petriNetObject_)[Association["Arcs" -> arcs_List, "Places" -> places_List, 
     "Transitions" -> transitions_List], initialConditions_List], "PetriNetObjects", options:OptionsPattern[]] := 
  First[petriNetNondeterministicEvolution[constructPetriNet[Association["Arcs" -> arcs, "Places" -> places, 
       "Transitions" -> transitions]], initialConditions]] /; SymbolName[petriNetObject] === "PetriNetObject"
PetriNetNondeterministicEvolution[(petriNetObject_)[Association["Arcs" -> arcs_List, "Places" -> places_List, 
     "Transitions" -> transitions_List], initialConditions_List], stepCount_Integer, "PetriNetObjects", 
   options:OptionsPattern[]] := First /@ NestList[petriNetNondeterministicEvolution, 
     {PetriNetObject[Association["Arcs" -> arcs, "Places" -> places, "Transitions" -> transitions], initialConditions], 
      {constructPetriNet[Association["Arcs" -> arcs, "Places" -> places, "Transitions" -> transitions]], 
       initialConditions}}, stepCount] /; SymbolName[petriNetObject] === "PetriNetObject"
PetriNetNondeterministicEvolution[(petriNetObject_)[Association["Arcs" -> arcs_List, "Places" -> places_List, 
     "Transitions" -> transitions_List], initialConditions_List], "LabeledGraphs", options:OptionsPattern[]] := 
  Graph[First[petriNetNondeterministicEvolutionLabeledGraphs[constructPetriNet[Association["Arcs" -> arcs, 
        "Places" -> places, "Transitions" -> transitions]], initialConditions]], options] /; 
   SymbolName[petriNetObject] === "PetriNetObject"
PetriNetNondeterministicEvolution[(petriNetObject_)[Association["Arcs" -> arcs_List, "Places" -> places_List, 
     "Transitions" -> transitions_List], initialConditions_List], stepCount_Integer, "LabeledGraphs", 
   options:OptionsPattern[]] := 
  (Graph[#1, options] & ) /@ First /@ NestList[petriNetNondeterministicEvolutionLabeledGraphs, 
      {renderPetriNet[constructPetriNet[Association["Arcs" -> arcs, "Places" -> places, "Transitions" -> transitions]], 
        initialConditions], {constructPetriNet[Association["Arcs" -> arcs, "Places" -> places, 
          "Transitions" -> transitions]], initialConditions}}, stepCount] /; 
   SymbolName[petriNetObject] === "PetriNetObject"
PetriNetNondeterministicEvolution[(petriNetObject_)[Association["Arcs" -> arcs_List, "Places" -> places_List, 
     "Transitions" -> transitions_List], initialConditions_List], "LabeledGraphsHighlighted", 
   options:OptionsPattern[]] := 
  Graph[First[petriNetNondeterministicEvolutionLabeledGraphsHighlighted[
      constructPetriNet[Association["Arcs" -> arcs, "Places" -> places, "Transitions" -> transitions]], 
      initialConditions]], options] /; SymbolName[petriNetObject] === "PetriNetObject"
PetriNetNondeterministicEvolution[(petriNetObject_)[Association["Arcs" -> arcs_List, "Places" -> places_List, 
     "Transitions" -> transitions_List], initialConditions_List], stepCount_Integer, "LabeledGraphsHighlighted", 
   options:OptionsPattern[]] := 
  (Graph[#1, options] & ) /@ First /@ NestList[petriNetNondeterministicEvolutionLabeledGraphsHighlighted, 
      {renderPetriNet[constructPetriNet[Association["Arcs" -> arcs, "Places" -> places, "Transitions" -> transitions]], 
        initialConditions], {constructPetriNet[Association["Arcs" -> arcs, "Places" -> places, 
          "Transitions" -> transitions]], initialConditions}}, stepCount] /; 
   SymbolName[petriNetObject] === "PetriNetObject"
PetriNetNondeterministicEvolution[(petriNetObject_)[Association["Arcs" -> arcs_List, "Places" -> places_List, 
     "Transitions" -> transitions_List], initialConditions_List], "WeightedGraphs", options:OptionsPattern[]] := 
  Graph[First[petriNetNondeterministicEvolutionWeightedGraphs[constructPetriNet[Association["Arcs" -> arcs, 
        "Places" -> places, "Transitions" -> transitions]], initialConditions]], options] /; 
   SymbolName[petriNetObject] === "PetriNetObject"
PetriNetNondeterministicEvolution[(petriNetObject_)[Association["Arcs" -> arcs_List, "Places" -> places_List, 
     "Transitions" -> transitions_List], initialConditions_List], stepCount_Integer, "WeightedGraphs", 
   options:OptionsPattern[]] := 
  (Graph[#1, options] & ) /@ First /@ NestList[petriNetNondeterministicEvolutionWeightedGraphs, 
      {renderPetriNetWeighted[constructPetriNet[Association["Arcs" -> arcs, "Places" -> places, 
          "Transitions" -> transitions]], initialConditions], 
       {constructPetriNet[Association["Arcs" -> arcs, "Places" -> places, "Transitions" -> transitions]], 
        initialConditions}}, stepCount] /; SymbolName[petriNetObject] === "PetriNetObject"
PetriNetNondeterministicEvolution[(petriNetObject_)[Association["Arcs" -> arcs_List, "Places" -> places_List, 
     "Transitions" -> transitions_List], initialConditions_List], "WeightedGraphsHighlighted", 
   options:OptionsPattern[]] := 
  Graph[First[petriNetNondeterministicEvolutionWeightedGraphsHighlighted[
      constructPetriNet[Association["Arcs" -> arcs, "Places" -> places, "Transitions" -> transitions]], 
      initialConditions]], options] /; SymbolName[petriNetObject] === "PetriNetObject"
PetriNetNondeterministicEvolution[(petriNetObject_)[Association["Arcs" -> arcs_List, "Places" -> places_List, 
     "Transitions" -> transitions_List], initialConditions_List], stepCount_Integer, "WeightedGraphsHighlighted", 
   options:OptionsPattern[]] := 
  (Graph[#1, options] & ) /@ First /@ NestList[petriNetNondeterministicEvolutionWeightedGraphsHighlighted, 
      {renderPetriNetWeighted[constructPetriNet[Association["Arcs" -> arcs, "Places" -> places, 
          "Transitions" -> transitions]], initialConditions], 
       {constructPetriNet[Association["Arcs" -> arcs, "Places" -> places, "Transitions" -> transitions]], 
        initialConditions}}, stepCount] /; SymbolName[petriNetObject] === "PetriNetObject"
PetriNetNondeterministicEvolution[(petriNetObject_)[Association["Arcs" -> arcs_List, "Places" -> places_List, 
     "Transitions" -> transitions_List], initialConditions_List], "Tokens", options:OptionsPattern[]] := 
  First[petriNetNondeterministicEvolutionTokens[constructPetriNet[Association["Arcs" -> arcs, "Places" -> places, 
       "Transitions" -> transitions]], initialConditions]] /; SymbolName[petriNetObject] === "PetriNetObject"
PetriNetNondeterministicEvolution[(petriNetObject_)[Association["Arcs" -> arcs_List, "Places" -> places_List, 
     "Transitions" -> transitions_List], initialConditions_List], stepCount_Integer, "Tokens", 
   options:OptionsPattern[]] := First /@ NestList[petriNetNondeterministicEvolutionTokens, 
     {initialConditions, {constructPetriNet[Association["Arcs" -> arcs, "Places" -> places, 
         "Transitions" -> transitions]], initialConditions}}, stepCount] /; 
   SymbolName[petriNetObject] === "PetriNetObject"
PetriNetNondeterministicEvolution[(petriNetObject_)[Association["Arcs" -> arcs_List, "Places" -> places_List, 
     "Transitions" -> transitions_List], initialConditions_List], "TokenFirings", options:OptionsPattern[]] := 
  First[petriNetNondeterministicEvolutionTokenFirings[constructPetriNet[Association["Arcs" -> arcs, "Places" -> places, 
       "Transitions" -> transitions]], initialConditions]] /; SymbolName[petriNetObject] === "PetriNetObject"
PetriNetNondeterministicEvolution[(petriNetObject_)[Association["Arcs" -> arcs_List, "Places" -> places_List, 
     "Transitions" -> transitions_List], initialConditions_List], stepCount_Integer, "TokenFirings", 
   options:OptionsPattern[]] := First /@ NestList[petriNetNondeterministicEvolutionTokenFirings, 
     {{initialConditions, Null}, {constructPetriNet[Association["Arcs" -> arcs, "Places" -> places, 
         "Transitions" -> transitions]], initialConditions}}, stepCount] /; 
   SymbolName[petriNetObject] === "PetriNetObject"
