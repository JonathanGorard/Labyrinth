(* ::Package:: *)

addLivenessTokens[vertex_, tokenCount_Integer, coordinateAssociation_Association] := 
  Text[Row[ConstantArray[Style["\[FilledCircle]", Purple], tokenCount]], coordinateAssociation[vertex]]
addLivenessTokensList[coordinateAssociation_Association, vertexWeightAssociation_Association, vertices_List] := 
  Module[{livenessTokensList}, livenessTokensList = {}; 
    Scan[AppendTo[livenessTokensList, addLivenessTokens[#1, vertexWeightAssociation[#1], coordinateAssociation]] & , 
     vertices]; livenessTokensList]
applyBooleanOperatorToList[booleanOperator_, list_List] := Fold[booleanOperator, First[list], Rest[list]]
validDaedaelusProtocolQ[Association["AlicePlaces" -> alicePlaces_List, "Arcs" -> arcs_List, 
    "BobPlaces" -> bobPlaces_List, "ForwardArcs" -> forwardArcs_List, "ForwardTransitions" -> forwardTransitions_List, 
    "ReverseArcs" -> reverseArcs_List, "ReverseTransitions" -> reverseTransitions_List, 
    "Transitions" -> transitions_List]] := Module[{daedaelusProtocolGraph}, 
   daedaelusProtocolGraph = Graph[Apply[DirectedEdge, Join[arcs, forwardArcs, reverseArcs], {1}]]; 
    applyBooleanOperatorToList[And, {Intersection[Join[alicePlaces, bobPlaces], Join[transitions, forwardTransitions, 
         reverseTransitions]] == {}, BipartiteGraphQ[daedaelusProtocolGraph]}]]
constructDaedaelusProtocolGraph[Association["AlicePlaces" -> alicePlaces_List, "Arcs" -> arcs_List, 
    "BobPlaces" -> bobPlaces_List, "ForwardArcs" -> forwardArcs_List, "ForwardTransitions" -> forwardTransitions_List, 
    "ReverseArcs" -> reverseArcs_List, "ReverseTransitions" -> reverseTransitions_List, 
    "Transitions" -> transitions_List]] := If[validDaedaelusProtocolQ[Association["AlicePlaces" -> alicePlaces, 
     "Arcs" -> arcs, "BobPlaces" -> bobPlaces, "ForwardArcs" -> forwardArcs, "ForwardTransitions" -> forwardTransitions, 
     "ReverseArcs" -> reverseArcs, "ReverseTransitions" -> reverseTransitions, "Transitions" -> transitions]], 
   {Graph[Join[alicePlaces, bobPlaces, transitions, forwardTransitions, reverseTransitions], 
     Apply[DirectedEdge, Join[arcs, forwardArcs, reverseArcs], {1}], VertexShapeFunction -> 
      Join[Thread[Join[alicePlaces, bobPlaces] -> "Circle"], 
       Thread[Join[transitions, forwardTransitions, reverseTransitions] -> "Square"]], 
     VertexStyle -> Join[Thread[alicePlaces -> Yellow], Thread[bobPlaces -> Blue], Thread[transitions -> Gray], 
       Thread[forwardTransitions -> Green], Thread[reverseTransitions -> Red]], 
     EdgeStyle -> Join[Thread[Apply[DirectedEdge, arcs, {1}] -> Directive[Black, Thick]], 
       Thread[Apply[DirectedEdge, forwardArcs, {1}] -> Directive[Green, Thick]], 
       Thread[Apply[DirectedEdge, reverseArcs, {1}] -> Directive[Red, Thick]]], VertexSize -> Large, 
     VertexLabels -> "Name", VertexLabelStyle -> Medium], alicePlaces, bobPlaces, transitions, forwardTransitions, 
    reverseTransitions, Apply[DirectedEdge, arcs, {1}], Apply[DirectedEdge, forwardArcs, {1}], 
    Apply[DirectedEdge, reverseArcs, {1}]}, $Failed]
renderDaedaelusProtocolGraph[daedaelusProtocol_List, aliceTokens_List, bobTokens_List] := 
  Module[{daedaelusProtocolGraph, coordinateAssociation, vertexWeightAssociation, graphData, vertexWeight, graphEpilog}, 
   daedaelusProtocolGraph = Graph[First[daedaelusProtocol], VertexWeight -> 
       Join[Thread[daedaelusProtocol[[2]] -> aliceTokens], Thread[daedaelusProtocol[[3]] -> bobTokens], 
        Thread[Join[daedaelusProtocol[[4]], daedaelusProtocol[[5]], daedaelusProtocol[[6]]] -> 0]]]; 
    graphData = ToExpression[StringReplace[ToString[FullForm[daedaelusProtocolGraph]], "Graph" -> "List"]]; 
    vertexWeight = Last[Last[graphData[[3]]]]; coordinateAssociation = 
     Association[Thread[graphData[[1]] -> VertexCoordinates /. AbsoluteOptions[daedaelusProtocolGraph, 
         VertexCoordinates]]]; vertexWeightAssociation = Association[Thread[graphData[[1]] -> vertexWeight]]; 
    graphEpilog = addLivenessTokensList[coordinateAssociation, vertexWeightAssociation, graphData[[1]]]; 
    Graph[daedaelusProtocolGraph, Epilog -> graphEpilog]]
renderDaedaelusProtocolGraphWeighted[daedaelusProtocol_List, aliceTokens_List, bobTokens_List] := 
  Graph[First[daedaelusProtocol], VertexWeight -> Join[Thread[daedaelusProtocol[[2]] -> aliceTokens], 
     Thread[daedaelusProtocol[[3]] -> bobTokens], Thread[Join[daedaelusProtocol[[4]], daedaelusProtocol[[5]], 
        daedaelusProtocol[[6]]] -> 0]]]
MakeDaedaelusProtocol[associationForm_Association] := 
  DaedaelusProtocol[KeySort[associationForm], ConstantArray[0, Length[associationForm["AlicePlaces"]]], 
    ConstantArray[0, Length[associationForm["BobPlaces"]]]] /; KeyExistsQ[associationForm, "AlicePlaces"] && 
    KeyExistsQ[associationForm, "Arcs"] && KeyExistsQ[associationForm, "BobPlaces"] && 
    KeyExistsQ[associationForm, "ForwardArcs"] && KeyExistsQ[associationForm, "ForwardTransitions"] && 
    KeyExistsQ[associationForm, "ReverseArcs"] && KeyExistsQ[associationForm, "ReverseTransitions"] && 
    KeyExistsQ[associationForm, "Transitions"]
MakeDaedaelusProtocol[associationForm_Association, aliceTokens_List, bobTokens_List] := 
  DaedaelusProtocol[KeySort[associationForm], aliceTokens, bobTokens] /; KeyExistsQ[associationForm, "AlicePlaces"] && 
    KeyExistsQ[associationForm, "Arcs"] && KeyExistsQ[associationForm, "BobPlaces"] && 
    KeyExistsQ[associationForm, "ForwardArcs"] && KeyExistsQ[associationForm, "ForwardTransitions"] && 
    KeyExistsQ[associationForm, "ReverseArcs"] && KeyExistsQ[associationForm, "ReverseTransitions"] && 
    KeyExistsQ[associationForm, "Transitions"]
MakeDaedaelusProtocol[alicePlaces_List, bobPlaces_List, transitions_List, forwardTransitions_List, 
   reverseTransitions_List, arcs_List, forwardArcs_List, reverseArcs_List] := 
  DaedaelusProtocol[Association["AlicePlaces" -> alicePlaces, "Arcs" -> arcs, "BobPlaces" -> bobPlaces, 
    "ForwardArcs" -> forwardArcs, "ForwardTransitions" -> forwardTransitions, "ReverseArcs" -> reverseArcs, 
    "ReverseTransitions" -> reverseTransitions, "Transitions" -> transitions], ConstantArray[0, Length[alicePlaces]], 
   ConstantArray[0, Length[bobPlaces]]]
MakeDaedaelusProtocol[alicePlaces_List, bobPlaces_List, transitions_List, forwardTransitions_List, 
   reverseTransitions_List, arcs_List, forwardArcs_List, reverseArcs_List, aliceTokens_List, bobTokens_List] := 
  DaedaelusProtocol[Association["AlicePlaces" -> alicePlaces, "Arcs" -> arcs, "BobPlaces" -> bobPlaces, 
     "ForwardArcs" -> forwardArcs, "ForwardTransitions" -> forwardTransitions, "ReverseArcs" -> reverseArcs, 
     "ReverseTransitions" -> reverseTransitions, "Transitions" -> transitions], aliceTokens, bobTokens] /; 
   Length[aliceTokens] == Length[alicePlaces] && Length[bobTokens] == Length[bobPlaces]
DaedaelusProtocol[Association["AlicePlaces" -> alicePlaces_List, "Arcs" -> arcs_List, "BobPlaces" -> bobPlaces_List, 
     "ForwardArcs" -> forwardArcs_List, "ForwardTransitions" -> forwardTransitions_List, 
     "ReverseArcs" -> reverseArcs_List, "ReverseTransitions" -> reverseTransitions_List, 
     "Transitions" -> transitions_List], aliceTokens_List, bobTokens_List]["AssociationForm"] := 
  Association["AlicePlaces" -> alicePlaces, "BobPlaces" -> bobPlaces, "Transitions" -> transitions, 
   "ForwardTransitions" -> forwardTransitions, "ReverseTransitions" -> reverseTransitions, "Arcs" -> arcs, 
   "ForwardArcs" -> forwardArcs, "ReverseArcs" -> reverseArcs]
DaedaelusProtocol[Association["AlicePlaces" -> alicePlaces_List, "Arcs" -> arcs_List, "BobPlaces" -> bobPlaces_List, 
     "ForwardArcs" -> forwardArcs_List, "ForwardTransitions" -> forwardTransitions_List, 
     "ReverseArcs" -> reverseArcs_List, "ReverseTransitions" -> reverseTransitions_List, 
     "Transitions" -> transitions_List], aliceTokens_List, bobTokens_List]["UnlabeledGraph"] := 
  First[constructDaedaelusProtocolGraph[Association["AlicePlaces" -> alicePlaces, "Arcs" -> arcs, 
     "BobPlaces" -> bobPlaces, "ForwardArcs" -> forwardArcs, "ForwardTransitions" -> forwardTransitions, 
     "ReverseArcs" -> reverseArcs, "ReverseTransitions" -> reverseTransitions, "Transitions" -> transitions]]]
DaedaelusProtocol[Association["AlicePlaces" -> alicePlaces_List, "Arcs" -> arcs_List, "BobPlaces" -> bobPlaces_List, 
     "ForwardArcs" -> forwardArcs_List, "ForwardTransitions" -> forwardTransitions_List, 
     "ReverseArcs" -> reverseArcs_List, "ReverseTransitions" -> reverseTransitions_List, 
     "Transitions" -> transitions_List], aliceTokens_List, bobTokens_List]["LabeledGraph"] := 
  renderDaedaelusProtocolGraph[constructDaedaelusProtocolGraph[Association["AlicePlaces" -> alicePlaces, "Arcs" -> arcs, 
     "BobPlaces" -> bobPlaces, "ForwardArcs" -> forwardArcs, "ForwardTransitions" -> forwardTransitions, 
     "ReverseArcs" -> reverseArcs, "ReverseTransitions" -> reverseTransitions, "Transitions" -> transitions]], 
   aliceTokens, bobTokens]
DaedaelusProtocol[Association["AlicePlaces" -> alicePlaces_List, "Arcs" -> arcs_List, "BobPlaces" -> bobPlaces_List, 
     "ForwardArcs" -> forwardArcs_List, "ForwardTransitions" -> forwardTransitions_List, 
     "ReverseArcs" -> reverseArcs_List, "ReverseTransitions" -> reverseTransitions_List, 
     "Transitions" -> transitions_List], aliceTokens_List, bobTokens_List]["WeightedGraph"] := 
  renderDaedaelusProtocolGraphWeighted[constructDaedaelusProtocolGraph[Association["AlicePlaces" -> alicePlaces, 
     "Arcs" -> arcs, "BobPlaces" -> bobPlaces, "ForwardArcs" -> forwardArcs, "ForwardTransitions" -> forwardTransitions, 
     "ReverseArcs" -> reverseArcs, "ReverseTransitions" -> reverseTransitions, "Transitions" -> transitions]], 
   aliceTokens, bobTokens]
DaedaelusProtocol /: MakeBoxes[daedaelusProtocol:DaedaelusProtocol[Association["AlicePlaces" -> alicePlaces_List, 
       "Arcs" -> arcs_List, "BobPlaces" -> bobPlaces_List, "ForwardArcs" -> forwardArcs_List, 
       "ForwardTransitions" -> forwardTransitions_List, "ReverseArcs" -> reverseArcs_List, 
       "ReverseTransitions" -> reverseTransitions_List, "Transitions" -> transitions_List], aliceTokens_List, 
      bobTokens_List], format_] := Module[{placeCount, transitionCount, arcCount, tokenCount}, 
    placeCount = StringJoin[ToString[Length[alicePlaces]], "+", ToString[Length[bobPlaces]]]; 
     transitionCount = StringJoin[ToString[Length[transitions]], "+", ToString[Length[forwardTransitions]], "+", 
       ToString[Length[reverseTransitions]]]; arcCount = StringJoin[ToString[Length[arcs]], "+", 
       ToString[Length[forwardArcs]], "+", ToString[Length[reverseArcs]]]; 
     tokenCount = StringJoin[ToString[Total[aliceTokens]], "+", ToString[Total[bobTokens]]]; 
     BoxForm`ArrangeSummaryBox["DaedaelusProtocol", daedaelusProtocol, 
      GraphPlot[First[constructDaedaelusProtocolGraph[Association["AlicePlaces" -> alicePlaces, "Arcs" -> arcs, 
          "BobPlaces" -> bobPlaces, "ForwardArcs" -> forwardArcs, "ForwardTransitions" -> forwardTransitions, 
          "ReverseArcs" -> reverseArcs, "ReverseTransitions" -> reverseTransitions, "Transitions" -> transitions]]], 
       VertexLabels -> None, EdgeShapeFunction -> GraphElementData["ShortFilledArrow", "ArrowSize" -> 0.05]], 
      {{BoxForm`SummaryItem[{"Places: ", placeCount}], BoxForm`SummaryItem[{"Transitions: ", transitionCount}]}, 
       {BoxForm`SummaryItem[{"Arcs: ", arcCount}], BoxForm`SummaryItem[{"Tokens: ", tokenCount}]}}, {{}}, format, 
      "Interpretable" -> Automatic]]
transitionFiringQ[daedaelusProtocol_List, transition_, currentTokens_List] := 
  Module[{tokenCount}, tokenCount = (Length[EdgeList[First[daedaelusProtocol], DirectedEdge[#1, transition]]] & ) /@ 
      Join[daedaelusProtocol[[2]], daedaelusProtocol[[3]]]; AllTrue[currentTokens - tokenCount, NonNegative]]
applyTransitionFiring[daedaelusProtocol_List, transition_, currentTokens_List] := 
  Module[{deletedTokensCount, addedTokensCount}, 
   deletedTokensCount = (Length[EdgeList[First[daedaelusProtocol], DirectedEdge[#1, transition]]] & ) /@ 
      Join[daedaelusProtocol[[2]], daedaelusProtocol[[3]]]; addedTokensCount = 
     (Length[EdgeList[First[daedaelusProtocol], DirectedEdge[transition, #1]]] & ) /@ 
      Join[daedaelusProtocol[[2]], daedaelusProtocol[[3]]]; currentTokens - deletedTokensCount + addedTokensCount]
updateTransitionFiringMultiway[daedaelusProtocol_List][currentTokens_List] := 
  Module[{transitionIndices, validTransitionIndices}, 
   transitionIndices = First /@ Position[(transitionFiringQ[daedaelusProtocol, #1, currentTokens] & ) /@ 
        Join[daedaelusProtocol[[4]], daedaelusProtocol[[5]], daedaelusProtocol[[6]]], True]; 
    validTransitionIndices = Select[transitionIndices, IntegerQ]; 
    (applyTransitionFiring[daedaelusProtocol, Join[daedaelusProtocol[[4]], daedaelusProtocol[[5]], 
         daedaelusProtocol[[6]]][[#1]], currentTokens] & ) /@ validTransitionIndices]
daedaelusMultiwayEvolution[daedaelusProtocol_List, aliceTokens_List, bobTokens_List] := 
  Module[{currentTokens, transitionIndices, validTransitionIndices}, currentTokens = Join[aliceTokens, bobTokens]; 
    currentTokens = updateTransitionFiringMultiway[daedaelusProtocol][currentTokens]; 
    transitionIndices = Catenate[
      (Module[{tokens = #1}, First /@ Position[(transitionFiringQ[daedaelusProtocol, #1, tokens] & ) /@ 
            Join[daedaelusProtocol[[4]], daedaelusProtocol[[5]], daedaelusProtocol[[6]]], True]] & ) /@ currentTokens]; 
    validTransitionIndices = Select[transitionIndices, IntegerQ]; 
    (Module[{tokens = #1}, (applyTransitionFiring[daedaelusProtocol, Join[daedaelusProtocol[[4]], daedaelusProtocol[[5]], 
            daedaelusProtocol[[6]]][[#1]], tokens] & ) /@ validTransitionIndices] & ) /@ currentTokens; 
    DeleteDuplicates[Catenate[(Module[{tokens = #1}, ({Take[tokens, Length[daedaelusProtocol[[2]]]], 
            Take[tokens, -Length[daedaelusProtocol[[3]]]], Join[daedaelusProtocol[[4]], daedaelusProtocol[[5]], 
              daedaelusProtocol[[6]]][[#1]]} & ) /@ validTransitionIndices] & ) /@ currentTokens]]]
getDaedaelusStateEvolutionFunction[daedaelusProtocol_List, initialConditions_String] := 
  ToString /@ daedaelusMultiwayEvolution[daedaelusProtocol, ToExpression[initialConditions][[1]], 
    ToExpression[initialConditions][[2]]]
getDaedaelusStateRenderingFunction[daedaelusProtocol_List] := 
  Inset[Framed[Style[GraphPlot[If[Last[ToExpression[#2]] === Null, renderDaedaelusProtocolGraph[daedaelusProtocol, 
         ToExpression[#2][[1]], ToExpression[#2][[2]]], HighlightGraph[renderDaedaelusProtocolGraph[daedaelusProtocol, 
          ToExpression[#2][[1]], ToExpression[#2][[2]]], {ToExpression[#2][[3]], ToString[ToExpression[#2][[3]]]}]], 
       VertexLabels -> None, EdgeShapeFunction -> GraphElementData["ShortFilledArrow", "ArrowSize" -> 0.05]], 
      Hue[0.62, 1, 0.48]], Background -> Directive[Opacity[0.2], Hue[0.62, 0.45, 0.87]], RoundingRadius -> 0, 
     FrameStyle -> Directive[Opacity[0.5], Hue[0.62, 0.52, 0.82]]], #1, {0, 0}, #3] & 
SimulateDaedaelusProtocol[(daedaelusProtocol_)[Association["AlicePlaces" -> alicePlaces_List, "Arcs" -> arcs_List, 
     "BobPlaces" -> bobPlaces_List, "ForwardArcs" -> forwardArcs_List, "ForwardTransitions" -> forwardTransitions_List, 
     "ReverseArcs" -> reverseArcs_List, "ReverseTransitions" -> reverseTransitions_List, 
     "Transitions" -> transitions_List], aliceTokens_List, bobTokens_List], stepCount_Integer] := 
  ResourceFunction["MultiwaySystem"][Association["StateEvolutionFunction" -> 
      (getDaedaelusStateEvolutionFunction[constructDaedaelusProtocolGraph[Association["AlicePlaces" -> alicePlaces, 
          "Arcs" -> arcs, "BobPlaces" -> bobPlaces, "ForwardArcs" -> forwardArcs, "ForwardTransitions" -> 
           forwardTransitions, "ReverseArcs" -> reverseArcs, "ReverseTransitions" -> reverseTransitions, 
          "Transitions" -> transitions]], #1] & ), "StateEquivalenceFunction" -> SameQ, "StateEventFunction" -> Identity, 
     "EventDecompositionFunction" -> Identity, "EventApplicationFunction" -> Identity, "SystemType" -> "PetriNet", 
     "EventSelectionFunction" -> Identity], {ToString[{aliceTokens, bobTokens, Null}]}, stepCount, "StatesGraph", 
    "StateRenderingFunction" -> getDaedaelusStateRenderingFunction[constructDaedaelusProtocolGraph[
       Association["AlicePlaces" -> alicePlaces, "Arcs" -> arcs, "BobPlaces" -> bobPlaces, "ForwardArcs" -> forwardArcs, 
        "ForwardTransitions" -> forwardTransitions, "ReverseArcs" -> reverseArcs, "ReverseTransitions" -> 
         reverseTransitions, "Transitions" -> transitions]]], VertexSize -> 1] /; 
   SymbolName[daedaelusProtocol] === "DaedaelusProtocol"
SimulateDaedaelusProtocolEntanglement[(daedaelusProtocol_)[Association["AlicePlaces" -> alicePlaces_List, 
     "Arcs" -> arcs_List, "BobPlaces" -> bobPlaces_List, "ForwardArcs" -> forwardArcs_List, 
     "ForwardTransitions" -> forwardTransitions_List, "ReverseArcs" -> reverseArcs_List, 
     "ReverseTransitions" -> reverseTransitions_List, "Transitions" -> transitions_List], aliceTokens_List, 
    bobTokens_List], stepCount_Integer] := ResourceFunction["MultiwaySystem"][
    Association["StateEvolutionFunction" -> (getDaedaelusStateEvolutionFunction[constructDaedaelusProtocolGraph[
         Association["AlicePlaces" -> alicePlaces, "Arcs" -> arcs, "BobPlaces" -> bobPlaces, 
          "ForwardArcs" -> forwardArcs, "ForwardTransitions" -> forwardTransitions, "ReverseArcs" -> reverseArcs, 
          "ReverseTransitions" -> reverseTransitions, "Transitions" -> transitions]], #1] & ), 
     "StateEquivalenceFunction" -> SameQ, "StateEventFunction" -> Identity, "EventDecompositionFunction" -> Identity, 
     "EventApplicationFunction" -> Identity, "SystemType" -> "PetriNet", "EventSelectionFunction" -> Identity], 
    {ToString[{aliceTokens, bobTokens, Null}]}, stepCount, "BranchialGraph", 
    "StateRenderingFunction" -> getDaedaelusStateRenderingFunction[constructDaedaelusProtocolGraph[
       Association["AlicePlaces" -> alicePlaces, "Arcs" -> arcs, "BobPlaces" -> bobPlaces, "ForwardArcs" -> forwardArcs, 
        "ForwardTransitions" -> forwardTransitions, "ReverseArcs" -> reverseArcs, "ReverseTransitions" -> 
         reverseTransitions, "Transitions" -> transitions]]], VertexSize -> 1] /; 
   SymbolName[daedaelusProtocol] === "DaedaelusProtocol"
