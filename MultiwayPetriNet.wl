(* ::Package:: *)

Options[MultiwayPetriNet] = Join[{"IncludeStepNumber" -> False, "IncludeStateID" -> False, 
     "IncludeInitializationEvents" -> False, "IncludeEventInstances" -> False, "IncludeStateWeights" -> False, 
     "IncludeStatePathWeights" -> False, "StateRenderingFunction" -> Automatic, "EventRenderingFunction" -> Automatic, 
     "MaxItems" -> Infinity, "GivePredecessors" -> False, "GiveResolvents" -> False, "IncludeSelfPairs" -> False, 
     "IncludeFullBranchialSpace" -> False, "LineThickness" -> 1, "VertexSizeMultiplier" -> 1}, Options[Graph]]; 
getEventSelectionFunction["Random"] := {RandomChoice[#1]} & 
getEventSelectionFunction["Sequential"] := {First[#1]} & 
getEventSelectionFunction[{"Random", eventCount_Integer}] := RandomChoice[#1, eventCount] & 
getEventSelectionFunction[(function_Symbol) | (function_Function)] := function
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
    KeyExistsQ[associationForm, "Places"] && KeyExistsQ[associationForm, "Transitions"]
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
        validTransitionIndices] & ) /@ currentTokens; DeleteDuplicates[
     Catenate[(Module[{tokens = #1}, ({tokens, petriNet[[3,#1]]} & ) /@ validTransitionIndices] & ) /@ currentTokens]]]
getPetriNetStateEvolutionFunction[petriNet_List, initialConditions_String] := 
  ToString /@ petriNetMultiwayEvolution[petriNet, First[ToExpression[initialConditions]]]
getPetriNetStateEvolutionFunction[petriNet_List, {stepNumber_Integer, stateID_Integer} -> initialConditions_String] := 
  ({stepNumber + 1, RandomInteger[10^10]} -> #1 & ) /@ ToString /@ petriNetMultiwayEvolution[petriNet, 
     First[ToExpression[initialConditions]]]
getPetriNetStateEvolutionFunction[petriNet_List, stepNumber_Integer -> initialConditions_String] := 
  (stepNumber + 1 -> #1 & ) /@ ToString /@ petriNetMultiwayEvolution[petriNet, First[ToExpression[initialConditions]]]
getPetriNetStateEvolutionFunction[petriNet_List, {Automatic, stateID_Integer} -> initialConditions_String] := 
  ({Automatic, RandomInteger[10^10]} -> #1 & ) /@ ToString /@ petriNetMultiwayEvolution[petriNet, 
     First[ToExpression[initialConditions]]]
getPetriNetStateEventFunction[initialConditions_String, petriNet_List] := 
  Module[{evolution = ToString /@ petriNetMultiwayEvolution[petriNet, First[ToExpression[initialConditions]]]}, 
   (Module[{length = StringLength[Last[Last[System`Dump`showStringDiff[initialConditions, #1]]]]}, 
      {StringDrop[initialConditions, -length] -> StringDrop[#1, -length], StringDrop[initialConditions, -length], 
       {"", StringTake[initialConditions, -length]}}] & ) /@ evolution]
getPetriNetStateEventFunction[initialConditions:{_String...}, petriNet_List] := 
  Catenate[(getPetriNetStateEventFunction[#1, petriNet] & ) /@ initialConditions]
getPetriNetStateEventFunction[{stepNumber_Integer, stateID_Integer} -> initialConditions_String, petriNet_List] := 
  ({stepNumber, {stateID, RandomInteger[10^10]}} -> #1 & ) /@ 
   Module[{evolution = ToString /@ petriNetMultiwayEvolution[petriNet, First[ToExpression[initialConditions]]]}, 
    (Module[{length = StringLength[Last[Last[System`Dump`showStringDiff[initialConditions, #1]]]]}, 
       {StringDrop[initialConditions, -length] -> StringDrop[#1, -length], StringDrop[initialConditions, -length], 
        {"", StringTake[initialConditions, -length]}}] & ) /@ evolution]
getPetriNetStateEventFunction[stepNumber_Integer -> initialConditions_String, petriNet_List] := 
  (stepNumber -> #1 & ) /@ 
   Module[{evolution = ToString /@ petriNetMultiwayEvolution[petriNet, First[ToExpression[initialConditions]]]}, 
    (Module[{length = StringLength[Last[Last[System`Dump`showStringDiff[initialConditions, #1]]]]}, 
       {StringDrop[initialConditions, -length] -> StringDrop[#1, -length], StringDrop[initialConditions, -length], 
        {"", StringTake[initialConditions, -length]}}] & ) /@ evolution]
getPetriNetStateEventFunction[{Automatic, stateID_Integer} -> initialConditions_String, petriNet_List] := 
  ({Automatic, {stateID, RandomInteger[10^10]}} -> #1 & ) /@ 
   Module[{evolution = ToString /@ petriNetMultiwayEvolution[petriNet, First[ToExpression[initialConditions]]]}, 
    (Module[{length = StringLength[Last[Last[System`Dump`showStringDiff[initialConditions, #1]]]]}, 
       {StringDrop[initialConditions, -length] -> StringDrop[#1, -length], StringDrop[initialConditions, -length], 
        {"", StringTake[initialConditions, -length]}}] & ) /@ evolution]
getPetriNetStateEventFunction[initialConditions:{_Rule...}, petriNet_List] := 
  Catenate[(getPetriNetStateEventFunction[#1, petriNet] & ) /@ initialConditions]
getPetriNetEventDecompositionFunction[event:{(input_String) | (input_Symbol) -> output_String, 
     (input_String) | (input_Symbol), {prefix_String, suffix_String}}] := 
  Join[{getPetriNetElementShifts[event]}, (getPetriNetElements[{#1, {prefix, suffix}}] & ) /@ {input, output}]
getPetriNetElementShifts[shiftedFragment_String, {input:{inputPrefix_String, inputSuffix_String}, 
    output:{outputPrefix_String, outputSuffix_String}}] := 
  Table[Rule @@ Apply[{StringPart[shiftedFragment, position], 
       {StringJoin[#1, StringTake[shiftedFragment, 1 ;; position - 1]], 
        StringJoin[StringTake[shiftedFragment, position + 1 ;; All], #2]}} & , {input, output}, {1}], 
   {position, StringLength[shiftedFragment]}]
getPetriNetElementShifts[{(input_String) | (input_Symbol) -> output_String, (input_String) | (input_Symbol), 
    {prefix_String, suffix_String}}] := Join[getPetriNetElementShifts[prefix, 
    {{"", StringJoin[input, suffix]}, {"", StringJoin[output, suffix]}}], 
   getPetriNetElementShifts[suffix, {{StringJoin[prefix, input], ""}, {StringJoin[prefix, output], ""}}]]
getPetriNetElements[{substring_String, {prefix_String, suffix_String}}] := 
  ({StringPart[substring, #1], {StringJoin[prefix, StringTake[substring, 1 ;; #1 - 1]], 
      StringJoin[StringTake[substring, #1 + 1 ;; All], suffix]}} & ) /@ Range[StringLength[substring]]
getPetriNetEventApplicationFunction[{(input_String) | (input_Symbol) -> output_String, (input_String) | (input_Symbol), 
    {prefix_String, suffix_String}}] := StringJoin[prefix, output, suffix]
getPetriNetEventApplicationFunction[{stepNumber_Integer, {(inputStateID_Integer) | (inputStateID_Symbol), 
      outputStateID_Integer}} -> {(input_String) | (input_Symbol) -> output_String, (input_String) | (input_Symbol), 
     {prefix_String, suffix_String}}] := {stepNumber + 1, outputStateID} -> StringJoin[prefix, output, suffix]
getPetriNetEventApplicationFunction[stepNumber_Integer -> {(input_String) | (input_Symbol) -> output_String, 
     (input_String) | (input_Symbol), {prefix_String, suffix_String}}] := 
  stepNumber + 1 -> StringJoin[prefix, output, suffix]
getPetriNetEventApplicationFunction[{Automatic, {(inputStateID_Integer) | (inputStateID_Symbol), 
      outputStateID_Integer}} -> {(input_String) | (input_Symbol) -> output_String, (input_String) | (input_Symbol), 
     {prefix_String, suffix_String}}] := {Automatic, outputStateID} -> StringJoin[prefix, output, suffix]
stripMetadata[expression_] := If[Head[expression] === Rule, Last[expression], expression]
getPetriNetStateRenderingFunction[petriNet_List] := 
  Inset[Framed[Style[GraphPlot[If[Last[ToExpression[stripMetadata[#2]]] === Null, renderPetriNet[petriNet, 
         First[ToExpression[stripMetadata[#2]]]], HighlightGraph[renderPetriNet[petriNet, 
          First[ToExpression[stripMetadata[#2]]]], {Last[ToExpression[stripMetadata[#2]]], 
          ToString[Last[ToExpression[stripMetadata[#2]]]]}]], VertexLabels -> None, 
       EdgeShapeFunction -> GraphElementData["ShortFilledArrow", "ArrowSize" -> 0.05]], Hue[0.62, 1, 0.48]], 
     Background -> Directive[Opacity[0.2], Hue[0.62, 0.45, 0.87]], RoundingRadius -> 0, 
     FrameStyle -> Directive[Opacity[0.5], Hue[0.62, 0.52, 0.82]]], #1, {0, 0}, #3] & 
getPetriNetEventRenderingFunction[petriNet_List] := 
  If[First[First[stripMetadata[#2]]] === Null, 
    Inset[Framed[Column[{Framed[Style[GraphPlot[If[Last[ToExpression[StringJoin[stripMetadata[#2][[3,1]], 
                stripMetadata[#2][[1,2]], stripMetadata[#2][[3,2]]]]] === Null, renderPetriNet[petriNet, 
             First[ToExpression[StringJoin[stripMetadata[#2][[3,1]], stripMetadata[#2][[1,2]], stripMetadata[#2][[3,
                 2]]]]]], HighlightGraph[renderPetriNet[First[ToExpression[StringJoin[stripMetadata[#2][[3,1]], 
                 stripMetadata[#2][[1,2]], stripMetadata[#2][[3,2]]]]]], {Last[ToExpression[StringJoin[
                 stripMetadata[#2][[3,1]], stripMetadata[#2][[1,2]], stripMetadata[#2][[3,2]]]]], 
              ToString[Last[ToExpression[StringJoin[stripMetadata[#2][[3,1]], stripMetadata[#2][[1,2]], 
                  stripMetadata[#2][[3,2]]]]]]}]], VertexLabels -> None, EdgeShapeFunction -> 
            GraphElementData["ShortFilledArrow", "ArrowSize" -> 0.05], ImageSize -> Scaled[0.5]]]]}, Center, 0], 
      Background -> Directive[Opacity[0.7], RGBColor[0.259, 0.576, 1]], FrameMargins -> {{2, 2}, {0, 0}}, 
      RoundingRadius -> 0, FrameStyle -> Directive[Opacity[0.4], Hue[0.09, 1, 0.91]]], #1, Center, #3], 
    Inset[Framed[Column[{Framed[Style[GraphPlot[If[Last[ToExpression[StringJoin[stripMetadata[#2][[3,1]], 
                stripMetadata[#2][[1,1]], stripMetadata[#2][[3,2]]]]] === Null, renderPetriNet[petriNet, 
             First[ToExpression[StringJoin[stripMetadata[#2][[3,1]], stripMetadata[#2][[1,1]], stripMetadata[#2][[3,
                 2]]]]]], HighlightGraph[renderPetriNet[petriNet, First[ToExpression[StringJoin[stripMetadata[#2][[3,
                  1]], stripMetadata[#2][[1,1]], stripMetadata[#2][[3,2]]]]]], 
             {Last[ToExpression[StringJoin[stripMetadata[#2][[3,1]], stripMetadata[#2][[1,1]], stripMetadata[#2][[3,
                  2]]]]], ToString[Last[ToExpression[StringJoin[stripMetadata[#2][[3,1]], stripMetadata[#2][[1,1]], 
                  stripMetadata[#2][[3,2]]]]]]}]], VertexLabels -> None, EdgeShapeFunction -> 
            GraphElementData["ShortFilledArrow", "ArrowSize" -> 0.05], ImageSize -> Scaled[0.5]]]], 
        Framed[Style[GraphPlot[If[Last[ToExpression[StringJoin[stripMetadata[#2][[3,1]], stripMetadata[#2][[1,2]], 
                stripMetadata[#2][[3,2]]]]] === Null, renderPetriNet[petriNet, First[ToExpression[StringJoin[
                stripMetadata[#2][[3,1]], stripMetadata[#2][[1,2]], stripMetadata[#2][[3,2]]]]]], 
            HighlightGraph[renderPetriNet[petriNet, First[ToExpression[StringJoin[stripMetadata[#2][[3,1]], 
                 stripMetadata[#2][[1,2]], stripMetadata[#2][[3,2]]]]]], {Last[ToExpression[StringJoin[
                 stripMetadata[#2][[3,1]], stripMetadata[#2][[1,2]], stripMetadata[#2][[3,2]]]]], 
              ToString[Last[ToExpression[StringJoin[stripMetadata[#2][[3,1]], stripMetadata[#2][[1,2]], 
                  stripMetadata[#2][[3,2]]]]]]}]], VertexLabels -> None, EdgeShapeFunction -> 
            GraphElementData["ShortFilledArrow", "ArrowSize" -> 0.05], ImageSize -> Scaled[0.5]]]]}, Center, 0], 
      Background -> Directive[Opacity[0.7], Hue[0.14, 0.34, 1]], FrameMargins -> {{2, 2}, {0, 0}}, RoundingRadius -> 0, 
      FrameStyle -> Directive[Opacity[0.4], Hue[0.09, 1, 0.91]]], #1, Center, #3]] & 
MultiwayPetriNet[associationForm_Association, options:OptionsPattern[]] := 
  MultiwayPetriNet[makePetriNet[associationForm] -> Identity, 1, "PetriNetObjects", options] /; 
   KeyExistsQ[associationForm, "Arcs"] && KeyExistsQ[associationForm, "Places"] && 
    KeyExistsQ[associationForm, "Transitions"]
MultiwayPetriNet[associationForm_Association, initialConditions_List, options:OptionsPattern[]] := 
  MultiwayPetriNet[makePetriNet[associationForm, initialConditions] -> Identity, 1, "PetriNetObjects", options] /; 
   KeyExistsQ[associationForm, "Arcs"] && KeyExistsQ[associationForm, "Places"] && 
    KeyExistsQ[associationForm, "Transitions"]
MultiwayPetriNet[associationForm_Association, stepCount_Integer, options:OptionsPattern[]] := 
  MultiwayPetriNet[makePetriNet[associationForm] -> Identity, stepCount, "PetriNetObjects", options] /; 
   KeyExistsQ[associationForm, "Arcs"] && KeyExistsQ[associationForm, "Places"] && 
    KeyExistsQ[associationForm, "Transitions"]
MultiwayPetriNet[associationForm_Association, initialConditions_List, stepCount_Integer, options:OptionsPattern[]] := 
  MultiwayPetriNet[makePetriNet[associationForm, initialConditions] -> Identity, stepCount, "PetriNetObjects", 
    options] /; KeyExistsQ[associationForm, "Arcs"] && KeyExistsQ[associationForm, "Places"] && 
    KeyExistsQ[associationForm, "Transitions"]
MultiwayPetriNet[associationForm_Association, property_String, options:OptionsPattern[]] := 
  MultiwayPetriNet[makePetriNet[associationForm] -> Identity, 1, property, options] /; 
   KeyExistsQ[associationForm, "Arcs"] && KeyExistsQ[associationForm, "Places"] && 
    KeyExistsQ[associationForm, "Transitions"]
MultiwayPetriNet[associationForm_Association, initialConditions_List, property_String, options:OptionsPattern[]] := 
  MultiwayPetriNet[makePetriNet[associationForm, initialConditions] -> Identity, 1, property, options] /; 
   KeyExistsQ[associationForm, "Arcs"] && KeyExistsQ[associationForm, "Places"] && 
    KeyExistsQ[associationForm, "Transitions"]
MultiwayPetriNet[associationForm_Association, stepCount_Integer, property_String, options:OptionsPattern[]] := 
  MultiwayPetriNet[makePetriNet[associationForm] -> Identity, stepCount, property, options] /; 
   KeyExistsQ[associationForm, "Arcs"] && KeyExistsQ[associationForm, "Places"] && 
    KeyExistsQ[associationForm, "Transitions"]
MultiwayPetriNet[associationForm_Association, initialConditions_List, stepCount_Integer, property_String, 
   options:OptionsPattern[]] := MultiwayPetriNet[makePetriNet[associationForm, initialConditions] -> Identity, 
    stepCount, property, options] /; KeyExistsQ[associationForm, "Arcs"] && KeyExistsQ[associationForm, "Places"] && 
    KeyExistsQ[associationForm, "Transitions"]
MultiwayPetriNet[associationForm_Association -> eventSelectionFunction_, options:OptionsPattern[]] := 
  MultiwayPetriNet[makePetriNet[associationForm] -> eventSelectionFunction, 1, "PetriNetObjects", options] /; 
   KeyExistsQ[associationForm, "Arcs"] && KeyExistsQ[associationForm, "Places"] && 
    KeyExistsQ[associationForm, "Transitions"]
MultiwayPetriNet[associationForm_Association -> eventSelectionFunction_, initialConditions_List, 
   options:OptionsPattern[]] := MultiwayPetriNet[makePetriNet[associationForm, initialConditions] -> 
     eventSelectionFunction, 1, "PetriNetObjects", options] /; KeyExistsQ[associationForm, "Arcs"] && 
    KeyExistsQ[associationForm, "Places"] && KeyExistsQ[associationForm, "Transitions"]
MultiwayPetriNet[associationForm_Association -> eventSelectionFunction_, stepCount_Integer, 
   options:OptionsPattern[]] := MultiwayPetriNet[makePetriNet[associationForm] -> eventSelectionFunction, stepCount, 
    "PetriNetObjects", options] /; KeyExistsQ[associationForm, "Arcs"] && KeyExistsQ[associationForm, "Places"] && 
    KeyExistsQ[associationForm, "Transitions"]
MultiwayPetriNet[associationForm_Association -> eventSelectionFunction_, initialConditions_List, stepCount_Integer, 
   options:OptionsPattern[]] := MultiwayPetriNet[makePetriNet[associationForm, initialConditions] -> 
     eventSelectionFunction, stepCount, "PetriNetObjects", options] /; KeyExistsQ[associationForm, "Arcs"] && 
    KeyExistsQ[associationForm, "Places"] && KeyExistsQ[associationForm, "Transitions"]
MultiwayPetriNet[associationForm_Association -> eventSelectionFunction_, property_String, options:OptionsPattern[]] := 
  MultiwayPetriNet[makePetriNet[associationForm] -> eventSelectionFunction, 1, property, options] /; 
   KeyExistsQ[associationForm, "Arcs"] && KeyExistsQ[associationForm, "Places"] && 
    KeyExistsQ[associationForm, "Transitions"]
MultiwayPetriNet[associationForm_Association -> eventSelectionFunction_, initialConditions_List, property_String, 
   options:OptionsPattern[]] := MultiwayPetriNet[makePetriNet[associationForm, initialConditions] -> 
     eventSelectionFunction, 1, property, options] /; KeyExistsQ[associationForm, "Arcs"] && 
    KeyExistsQ[associationForm, "Places"] && KeyExistsQ[associationForm, "Transitions"]
MultiwayPetriNet[associationForm_Association -> eventSelectionFunction_, stepCount_Integer, property_String, 
   options:OptionsPattern[]] := MultiwayPetriNet[makePetriNet[associationForm] -> eventSelectionFunction, stepCount, 
    property, options] /; KeyExistsQ[associationForm, "Arcs"] && KeyExistsQ[associationForm, "Places"] && 
    KeyExistsQ[associationForm, "Transitions"]
MultiwayPetriNet[associationForm_Association -> eventSelectionFunction_, initialConditions_List, stepCount_Integer, 
   property_String, options:OptionsPattern[]] := 
  MultiwayPetriNet[makePetriNet[associationForm, initialConditions] -> eventSelectionFunction, stepCount, property, 
    options] /; KeyExistsQ[associationForm, "Arcs"] && KeyExistsQ[associationForm, "Places"] && 
    KeyExistsQ[associationForm, "Transitions"]
MultiwayPetriNet[places_List, transitions_List, arcs_List, options:OptionsPattern[]] := 
  MultiwayPetriNet[makePetriNet[places, transitions, arcs] -> Identity, 1, "PetriNetObjects", options]
MultiwayPetriNet[places_List, transitions_List, arcs_List, initialConditions_List, options:OptionsPattern[]] := 
  MultiwayPetriNet[makePetriNet[places, transitions, arcs, initialConditions] -> Identity, 1, "PetriNetObjects", 
    options] /; Length[initialConditions] == Length[places]
MultiwayPetriNet[places_List, transitions_List, arcs_List, stepCount_Integer, options:OptionsPattern[]] := 
  MultiwayPetriNet[makePetriNet[places, transitions, arcs] -> Identity, stepCount, "PetriNetObjects", options]
MultiwayPetriNet[places_List, transitions_List, arcs_List, initialConditions_List, stepCount_Integer, 
   options:OptionsPattern[]] := MultiwayPetriNet[makePetriNet[places, transitions, arcs, initialConditions] -> 
     Identity, stepCount, "PetriNetObjects", options] /; Length[initialConditions] == Length[places]
MultiwayPetriNet[places_List, transitions_List, arcs_List, property_String, options:OptionsPattern[]] := 
  MultiwayPetriNet[makePetriNet[places, transitions, arcs] -> Identity, 1, property, options]
MultiwayPetriNet[places_List, transitions_List, arcs_List, initialConditions_List, property_String, 
   options:OptionsPattern[]] := MultiwayPetriNet[makePetriNet[places, transitions, arcs, initialConditions] -> 
     Identity, 1, property, options] /; Length[initialConditions] == Length[places]
MultiwayPetriNet[places_List, transitions_List, arcs_List, stepCount_Integer, property_String, 
   options:OptionsPattern[]] := MultiwayPetriNet[makePetriNet[places, transitions, arcs] -> Identity, stepCount, 
   property, options]
MultiwayPetriNet[places_List, transitions_List, arcs_List, initialConditions_List, stepCount_Integer, property_String, 
   options:OptionsPattern[]] := MultiwayPetriNet[makePetriNet[places, transitions, arcs, initialConditions] -> 
     Identity, stepCount, property, options] /; Length[initialConditions] == Length[places]
MultiwayPetriNet[{places_List, transitions_List, arcs_List} -> eventSelectionFunction_, options:OptionsPattern[]] := 
  MultiwayPetriNet[makePetriNet[places, transitions, arcs] -> eventSelectionFunction, 1, "PetriNetObjects", options]
MultiwayPetriNet[{places_List, transitions_List, arcs_List} -> eventSelectionFunction_, initialConditions_List, 
   options:OptionsPattern[]] := MultiwayPetriNet[makePetriNet[places, transitions, arcs, initialConditions] -> 
     eventSelectionFunction, 1, "PetriNetObjects", options] /; Length[initialConditions] == Length[places]
MultiwayPetriNet[{places_List, transitions_List, arcs_List} -> eventSelectionFunction_, stepCount_Integer, 
   options:OptionsPattern[]] := MultiwayPetriNet[makePetriNet[places, transitions, arcs] -> eventSelectionFunction, 
   stepCount, "PetriNetObjects", options]
MultiwayPetriNet[{places_List, transitions_List, arcs_List} -> eventSelectionFunction_, initialConditions_List, 
   stepCount_Integer, options:OptionsPattern[]] := 
  MultiwayPetriNet[makePetriNet[places, transitions, arcs, initialConditions] -> eventSelectionFunction, stepCount, 
    "PetriNetObjects", options] /; Length[initialConditions] == Length[places]
MultiwayPetriNet[{places_List, transitions_List, arcs_List} -> eventSelectionFunction_, property_String, 
   options:OptionsPattern[]] := MultiwayPetriNet[makePetriNet[places, transitions, arcs] -> eventSelectionFunction, 1, 
   property, options]
MultiwayPetriNet[{places_List, transitions_List, arcs_List} -> eventSelectionFunction_, initialConditions_List, 
   property_String, options:OptionsPattern[]] := 
  MultiwayPetriNet[makePetriNet[places, transitions, arcs, initialConditions] -> eventSelectionFunction, 1, property, 
    options] /; Length[initialConditions] == Length[places]
MultiwayPetriNet[{places_List, transitions_List, arcs_List} -> eventSelectionFunction_, stepCount_Integer, 
   property_String, options:OptionsPattern[]] := MultiwayPetriNet[makePetriNet[places, transitions, arcs] -> 
    eventSelectionFunction, stepCount, property, options]
MultiwayPetriNet[{places_List, transitions_List, arcs_List} -> eventSelectionFunction_, initialConditions_List, 
   stepCount_Integer, property_String, options:OptionsPattern[]] := 
  MultiwayPetriNet[makePetriNet[places, transitions, arcs, initialConditions] -> eventSelectionFunction, stepCount, 
    property, options] /; Length[initialConditions] == Length[places]
MultiwayPetriNet[(petriNetObject_)[Association["Arcs" -> arcs_List, "Places" -> places_List, 
     "Transitions" -> transitions_List], initialConditions_List], options:OptionsPattern[]] := 
  MultiwayPetriNet[makePetriNet[Association["Arcs" -> arcs, "Places" -> places, "Transitions" -> transitions], 
      initialConditions] -> Identity, 1, "PetriNetObjects", options] /; SymbolName[petriNetObject] === "PetriNetObject"
MultiwayPetriNet[(petriNetObject_)[Association["Arcs" -> arcs_List, "Places" -> places_List, 
     "Transitions" -> transitions_List], initialConditions_List], stepCount_Integer, options:OptionsPattern[]] := 
  MultiwayPetriNet[makePetriNet[Association["Arcs" -> arcs, "Places" -> places, "Transitions" -> transitions], 
      initialConditions] -> Identity, stepCount, "PetriNetObjects", options] /; 
   SymbolName[petriNetObject] === "PetriNetObject"
MultiwayPetriNet[(petriNetObject_)[Association["Arcs" -> arcs_List, "Places" -> places_List, 
     "Transitions" -> transitions_List], initialConditions_List], property_String, options:OptionsPattern[]] := 
  MultiwayPetriNet[makePetriNet[Association["Arcs" -> arcs, "Places" -> places, "Transitions" -> transitions], 
      initialConditions] -> Identity, 1, property, options] /; SymbolName[petriNetObject] === "PetriNetObject"
MultiwayPetriNet[(petriNetObject_)[Association["Arcs" -> arcs_List, "Places" -> places_List, 
     "Transitions" -> transitions_List], initialConditions_List], stepCount_Integer, property_String, 
   options:OptionsPattern[]] := 
  MultiwayPetriNet[makePetriNet[Association["Arcs" -> arcs, "Places" -> places, "Transitions" -> transitions], 
      initialConditions] -> Identity, stepCount, property, options] /; SymbolName[petriNetObject] === "PetriNetObject"
MultiwayPetriNet[(petriNetObject_)[Association["Arcs" -> arcs_List, "Places" -> places_List, 
      "Transitions" -> transitions_List], initialConditions_List] -> eventSelectionFunction_, 
   options:OptionsPattern[]] := 
  MultiwayPetriNet[makePetriNet[Association["Arcs" -> arcs, "Places" -> places, "Transitions" -> transitions], 
      initialConditions] -> eventSelectionFunction, 1, "PetriNetObjects", options] /; 
   SymbolName[petriNetObject] === "PetriNetObject"
MultiwayPetriNet[(petriNetObject_)[Association["Arcs" -> arcs_List, "Places" -> places_List, 
      "Transitions" -> transitions_List], initialConditions_List] -> eventSelectionFunction_, stepCount_Integer, 
   options:OptionsPattern[]] := 
  MultiwayPetriNet[makePetriNet[Association["Arcs" -> arcs, "Places" -> places, "Transitions" -> transitions], 
      initialConditions] -> eventSelectionFunction, stepCount, "PetriNetObjects", options] /; 
   SymbolName[petriNetObject] === "PetriNetObject"
MultiwayPetriNet[(petriNetObject_)[Association["Arcs" -> arcs_List, "Places" -> places_List, 
      "Transitions" -> transitions_List], initialConditions_List] -> eventSelectionFunction_, property_String, 
   options:OptionsPattern[]] := 
  MultiwayPetriNet[makePetriNet[Association["Arcs" -> arcs, "Places" -> places, "Transitions" -> transitions], 
      initialConditions] -> eventSelectionFunction, 1, property, options] /; 
   SymbolName[petriNetObject] === "PetriNetObject"
MultiwayPetriNet[(petriNetObject_)[Association["Arcs" -> arcs_List, "Places" -> places_List, 
      "Transitions" -> transitions_List], initialConditions_List] -> eventSelectionFunction_, stepCount_Integer, 
   property_String, options:OptionsPattern[]] := 
  ResourceFunction["MultiwaySystem"][Association["StateEvolutionFunction" -> 
      (getPetriNetStateEvolutionFunction[constructPetriNet[Association["Arcs" -> arcs, "Places" -> places, 
          "Transitions" -> transitions]], #1] & ), "StateEquivalenceFunction" -> SameQ, 
     "StateEventFunction" -> (getPetriNetStateEventFunction[#1, constructPetriNet[Association["Arcs" -> arcs, 
          "Places" -> places, "Transitions" -> transitions]]] & ), "EventDecompositionFunction" -> 
      getPetriNetEventDecompositionFunction, "EventApplicationFunction" -> getPetriNetEventApplicationFunction, 
     "SystemType" -> "PetriNet", "EventSelectionFunction" -> getEventSelectionFunction[eventSelectionFunction]], 
    {ToString[{initialConditions, Null}]}, stepCount, property, options, "StateRenderingFunction" -> 
     getPetriNetStateRenderingFunction[constructPetriNet[Association["Arcs" -> arcs, "Places" -> places, 
        "Transitions" -> transitions]]], "EventRenderingFunction" -> getPetriNetEventRenderingFunction[
      constructPetriNet[Association["Arcs" -> arcs, "Places" -> places, "Transitions" -> transitions]]]] /; 
   SymbolName[petriNetObject] === "PetriNetObject" && property =!= "PetriNetObjects" && property =!= "LabeledGraphs" && 
    property =!= "LabeledGraphsHighlighted" && property =!= "WeightedGraphs" && 
    property =!= "WeightedGraphsHighlighted" && property =!= "Tokens" && property =!= "TokenFirings"
MultiwayPetriNet[(petriNetObject_)[Association["Arcs" -> arcs_List, "Places" -> places_List, 
      "Transitions" -> transitions_List], initialConditions_List] -> eventSelectionFunction_, stepCount_Integer, 
   "PetriNetObjects", options:OptionsPattern[]] := 
  (Module[{generation = #1}, DeleteDuplicates[(makePetriNet[Association["Arcs" -> arcs, "Places" -> places, 
           "Transitions" -> transitions], First[ToExpression[stripMetadata[#1]]]] & ) /@ generation]] & ) /@ 
    ResourceFunction["MultiwaySystem"][Association["StateEvolutionFunction" -> 
       (getPetriNetStateEvolutionFunction[constructPetriNet[Association["Arcs" -> arcs, "Places" -> places, 
           "Transitions" -> transitions]], #1] & ), "StateEquivalenceFunction" -> SameQ, 
      "StateEventFunction" -> (getPetriNetStateEventFunction[#1, constructPetriNet[Association["Arcs" -> arcs, 
           "Places" -> places, "Transitions" -> transitions]]] & ), "EventDecompositionFunction" -> 
       getPetriNetEventDecompositionFunction, "EventApplicationFunction" -> getPetriNetEventApplicationFunction, 
      "SystemType" -> "PetriNet", "EventSelectionFunction" -> getEventSelectionFunction[eventSelectionFunction]], 
     {ToString[{initialConditions, Null}]}, stepCount, "AllStatesList", options] /; 
   SymbolName[petriNetObject] === "PetriNetObject"
MultiwayPetriNet[(petriNetObject_)[Association["Arcs" -> arcs_List, "Places" -> places_List, 
      "Transitions" -> transitions_List], initialConditions_List] -> eventSelectionFunction_, stepCount_Integer, 
   "LabeledGraphs", options:OptionsPattern[]] := 
  (Module[{generation = #1}, (Graph[#1, FilterRules[{options}, Options[Graph]]] & ) /@ 
       DeleteDuplicates[(renderPetriNet[constructPetriNet[Association["Arcs" -> arcs, "Places" -> places, 
             "Transitions" -> transitions]], First[ToExpression[stripMetadata[#1]]]] & ) /@ generation]] & ) /@ 
    ResourceFunction["MultiwaySystem"][Association["StateEvolutionFunction" -> 
       (getPetriNetStateEvolutionFunction[constructPetriNet[Association["Arcs" -> arcs, "Places" -> places, 
           "Transitions" -> transitions]], #1] & ), "StateEquivalenceFunction" -> SameQ, 
      "StateEventFunction" -> (getPetriNetStateEventFunction[#1, constructPetriNet[Association["Arcs" -> arcs, 
           "Places" -> places, "Transitions" -> transitions]]] & ), "EventDecompositionFunction" -> 
       getPetriNetEventDecompositionFunction, "EventApplicationFunction" -> getPetriNetEventApplicationFunction, 
      "SystemType" -> "PetriNet", "EventSelectionFunction" -> getEventSelectionFunction[eventSelectionFunction]], 
     {ToString[{initialConditions, Null}]}, stepCount, "AllStatesList", options] /; 
   SymbolName[petriNetObject] === "PetriNetObject"
MultiwayPetriNet[(petriNetObject_)[Association["Arcs" -> arcs_List, "Places" -> places_List, 
      "Transitions" -> transitions_List], initialConditions_List] -> eventSelectionFunction_, stepCount_Integer, 
   "LabeledGraphsHighlighted", options:OptionsPattern[]] := 
  (Module[{generation = #1}, (Graph[#1, FilterRules[{options}, Options[Graph]]] & ) /@ 
       DeleteDuplicates[(HighlightGraph[renderPetriNet[constructPetriNet[Association["Arcs" -> arcs, 
              "Places" -> places, "Transitions" -> transitions]], First[ToExpression[stripMetadata[#1]]]], 
           {Last[ToExpression[stripMetadata[#1]]]}] & ) /@ generation]] & ) /@ 
    ResourceFunction["MultiwaySystem"][Association["StateEvolutionFunction" -> 
       (getPetriNetStateEvolutionFunction[constructPetriNet[Association["Arcs" -> arcs, "Places" -> places, 
           "Transitions" -> transitions]], #1] & ), "StateEquivalenceFunction" -> SameQ, 
      "StateEventFunction" -> (getPetriNetStateEventFunction[#1, constructPetriNet[Association["Arcs" -> arcs, 
           "Places" -> places, "Transitions" -> transitions]]] & ), "EventDecompositionFunction" -> 
       getPetriNetEventDecompositionFunction, "EventApplicationFunction" -> getPetriNetEventApplicationFunction, 
      "SystemType" -> "PetriNet", "EventSelectionFunction" -> getEventSelectionFunction[eventSelectionFunction]], 
     {ToString[{initialConditions, Null}]}, stepCount, "AllStatesList", options] /; 
   SymbolName[petriNetObject] === "PetriNetObject"
MultiwayPetriNet[(petriNetObject_)[Association["Arcs" -> arcs_List, "Places" -> places_List, 
      "Transitions" -> transitions_List], initialConditions_List] -> eventSelectionFunction_, stepCount_Integer, 
   "WeightedGraphs", options:OptionsPattern[]] := 
  (Module[{generation = #1}, (Graph[#1, FilterRules[{options}, Options[Graph]]] & ) /@ 
       DeleteDuplicates[(renderPetriNetWeighted[constructPetriNet[Association["Arcs" -> arcs, "Places" -> places, 
             "Transitions" -> transitions]], First[ToExpression[stripMetadata[#1]]]] & ) /@ generation]] & ) /@ 
    ResourceFunction["MultiwaySystem"][Association["StateEvolutionFunction" -> 
       (getPetriNetStateEvolutionFunction[constructPetriNet[Association["Arcs" -> arcs, "Places" -> places, 
           "Transitions" -> transitions]], #1] & ), "StateEquivalenceFunction" -> SameQ, 
      "StateEventFunction" -> (getPetriNetStateEventFunction[#1, constructPetriNet[Association["Arcs" -> arcs, 
           "Places" -> places, "Transitions" -> transitions]]] & ), "EventDecompositionFunction" -> 
       getPetriNetEventDecompositionFunction, "EventApplicationFunction" -> getPetriNetEventApplicationFunction, 
      "SystemType" -> "PetriNet", "EventSelectionFunction" -> getEventSelectionFunction[eventSelectionFunction]], 
     {ToString[{initialConditions, Null}]}, stepCount, "AllStatesList", options] /; 
   SymbolName[petriNetObject] === "PetriNetObject"
MultiwayPetriNet[(petriNetObject_)[Association["Arcs" -> arcs_List, "Places" -> places_List, 
      "Transitions" -> transitions_List], initialConditions_List] -> eventSelectionFunction_, stepCount_Integer, 
   "WeightedGraphsHighlighted", options:OptionsPattern[]] := 
  (Module[{generation = #1}, (Graph[#1, FilterRules[{options}, Options[Graph]]] & ) /@ 
       DeleteDuplicates[(HighlightGraph[renderPetriNetWeighted[constructPetriNet[Association["Arcs" -> arcs, 
              "Places" -> places, "Transitions" -> transitions]], First[ToExpression[stripMetadata[#1]]]], 
           {Last[ToExpression[stripMetadata[#1]]]}] & ) /@ generation]] & ) /@ 
    ResourceFunction["MultiwaySystem"][Association["StateEvolutionFunction" -> 
       (getPetriNetStateEvolutionFunction[constructPetriNet[Association["Arcs" -> arcs, "Places" -> places, 
           "Transitions" -> transitions]], #1] & ), "StateEquivalenceFunction" -> SameQ, 
      "StateEventFunction" -> (getPetriNetStateEventFunction[#1, constructPetriNet[Association["Arcs" -> arcs, 
           "Places" -> places, "Transitions" -> transitions]]] & ), "EventDecompositionFunction" -> 
       getPetriNetEventDecompositionFunction, "EventApplicationFunction" -> getPetriNetEventApplicationFunction, 
      "SystemType" -> "PetriNet", "EventSelectionFunction" -> getEventSelectionFunction[eventSelectionFunction]], 
     {ToString[{initialConditions, Null}]}, stepCount, "AllStatesList", options] /; 
   SymbolName[petriNetObject] === "PetriNetObject"
MultiwayPetriNet[(petriNetObject_)[Association["Arcs" -> arcs_List, "Places" -> places_List, 
      "Transitions" -> transitions_List], initialConditions_List] -> eventSelectionFunction_, stepCount_Integer, 
   "Tokens", options:OptionsPattern[]] := 
  (Module[{generation = #1}, DeleteDuplicates[(First[ToExpression[stripMetadata[#1]]] & ) /@ generation]] & ) /@ 
    ResourceFunction["MultiwaySystem"][Association["StateEvolutionFunction" -> 
       (getPetriNetStateEvolutionFunction[constructPetriNet[Association["Arcs" -> arcs, "Places" -> places, 
           "Transitions" -> transitions]], #1] & ), "StateEquivalenceFunction" -> SameQ, 
      "StateEventFunction" -> (getPetriNetStateEventFunction[#1, constructPetriNet[Association["Arcs" -> arcs, 
           "Places" -> places, "Transitions" -> transitions]]] & ), "EventDecompositionFunction" -> 
       getPetriNetEventDecompositionFunction, "EventApplicationFunction" -> getPetriNetEventApplicationFunction, 
      "SystemType" -> "PetriNet", "EventSelectionFunction" -> getEventSelectionFunction[eventSelectionFunction]], 
     {ToString[{initialConditions, Null}]}, stepCount, "AllStatesList", options] /; 
   SymbolName[petriNetObject] === "PetriNetObject"
MultiwayPetriNet[(petriNetObject_)[Association["Arcs" -> arcs_List, "Places" -> places_List, 
      "Transitions" -> transitions_List], initialConditions_List] -> eventSelectionFunction_, stepCount_Integer, 
   "TokenFirings", options:OptionsPattern[]] := 
  (Module[{generation = #1}, DeleteDuplicates[(ToExpression[stripMetadata[#1]] & ) /@ generation]] & ) /@ 
    ResourceFunction["MultiwaySystem"][Association["StateEvolutionFunction" -> 
       (getPetriNetStateEvolutionFunction[constructPetriNet[Association["Arcs" -> arcs, "Places" -> places, 
           "Transitions" -> transitions]], #1] & ), "StateEquivalenceFunction" -> SameQ, 
      "StateEventFunction" -> (getPetriNetStateEventFunction[#1, constructPetriNet[Association["Arcs" -> arcs, 
           "Places" -> places, "Transitions" -> transitions]]] & ), "EventDecompositionFunction" -> 
       getPetriNetEventDecompositionFunction, "EventApplicationFunction" -> getPetriNetEventApplicationFunction, 
      "SystemType" -> "PetriNet", "EventSelectionFunction" -> getEventSelectionFunction[eventSelectionFunction]], 
     {ToString[{initialConditions, Null}]}, stepCount, "AllStatesList", options] /; 
   SymbolName[petriNetObject] === "PetriNetObject"
