%%%-------------------------------------------------------------------
%%% @author yuvalfro
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. Apr 2020 13:43
%%%-------------------------------------------------------------------
-module(exf).
-author("yuvalfro").

%% API
-export([exp_to_bdd/2, solve_bdd/2, booleanGenerator/2]).

%%---------------------------------- exp_to_bdd function - Returns the corresponding BDD tree representation for that Boolean function --------------

exp_to_bdd(BoolFunc, Ordering) ->
  StartTime = os:timestamp(),                                        % Start timer
  ParamsList = getparams(BoolFunc),                                  % Get all the parameters x1,...,xn
  PermutationsList = perms(ParamsList),                              % Get all the permutations of x1,...,xn
  TreesList = buildAllTrees(BoolFunc, PermutationsList, []),         % Build all the possible trees for all the permutations
  case Ordering of
    tree_height ->
      BDDTree = bestOfTreeHeights(TreesList),                        % Find the most efficient tree in the manner of tree height
      Error = no;
    num_of_nodes ->
      BDDTree = bestOfTreeNodes(TreesList),                          % Find the most efficient tree in the manner of tree number of nodes
      Error = no;
    num_of_leafs ->
      BDDTree = bestOfTreeLeafs(TreesList),                          % Find the most efficient tree in the manner of tree number of leafs
      Error = no;
    _ ->
      BDDTree = Ordering,                                            % Ordering value is not one of the three above - Error
      Error = yes
  end,
  case Error of
    no ->  io:format("Total Time of action: ~f miliseconds~n", [timer:now_diff(os:timestamp(), StartTime) / 1000]);  % Finish-time stamp
    yes -> io:fwrite("Ordering value ERROR, the input is:~n")
  end,
  BDDTree.

%%---------------------------------------------------------------------------------------------------------------------------------------------------

%%---------------------------------- solve_bdd function - Returns the result of the boolean function, according to the given BDD tree ---------------

solve_bdd(BddTree, VarList) ->
  StartTime = os:timestamp(),                                        % Start timer
  VarMap = maps:from_list(VarList),                                  % Convert the list of tuples to map
  Result = solve(BddTree,VarMap),
  io:format("Total Time of action: ~f miliseconds~n", [timer:now_diff(os:timestamp(), StartTime) / 1000]),  % Finish-time stamp
  Result.

%%---------------------------------------------------------------------------------------------------------------------------------------------------

%%---------------------------------- perms function - Returns all the permutations of a List --------------------------------------------------------

perms([]) -> [[]];
perms(L)  -> [[H|T] || H <- L, T <- perms(L--[H])].

%%---------------------------------------------------------------------------------------------------------------------------------------------------

%%---------------------------------- removedup function - Removes all duplicate elements in a List --------------------------------------------------

removedup([]) -> [];
removedup([H|T]) -> [H | [X || X <- removedup(T), X =/= H]].

%%---------------------------------------------------------------------------------------------------------------------------------------------------

%%---------------------------------- getparams function - Returns the parameters x1,...,xn from boolean function ------------------------------------

getparams(L) -> removedup(getparams(L,[])).     % Remove duplicates of the parameters
getparams(L,N) ->
  case L of
    {'not' ,Arg} when is_tuple(Arg) -> getparams(Arg, N);
    {'not' ,Arg} -> [Arg|N];
    % First argument is 'and' or 'or'
    {_ ,{Arg1, Arg2}} when is_tuple(Arg1) and is_tuple(Arg2) -> getparams(Arg1, N) ++ getparams(Arg2, N);     % Both tuples
    {_ ,{Arg1, Arg2}} when is_tuple(Arg1) and is_atom(Arg2) -> getparams(Arg1, N) ++ [Arg2|N];                % Arg1 is tuple, Arg2 is atom
    {_ ,{Arg1, Arg2}} when is_atom(Arg1) and is_tuple(Arg2) -> [Arg1|N] ++ getparams(Arg2, N);                % Arg2 is tuple, Arg1 is atom
    {_ ,{Arg1, Arg2}} -> [Arg1|N] ++ [Arg2|N]                                                                 % Both atoms
  end.

%%---------------------------------------------------------------------------------------------------------------------------------------------------

%%---------------------------------- buildAllTress function - Build a list of all trees  ------------------------------------------------------------

%% Create all trees from all permutations
buildAllTrees(_,[],TreesList) -> TreesList;
buildAllTrees(BoolFunc, [H|T], TreesList) -> buildAllTrees(BoolFunc,T,TreesList ++ [reduceTree(buildTree(BoolFunc,H,H,[]))]).

%% List = the current permutation, Path = List of true/false values
%% Calculate the leaf value
buildTree(BoolFunc, [], List, Path) -> leafValue(buildMap(#{},List,Path),BoolFunc);
%% Build tree by tree {node, left, right} when left is for xi=0 and right is for xi=1 (by Shannon expansion theory)
buildTree(BoolFunc, [H|T], List, Path) -> {H,buildTree(BoolFunc,T,List,Path++[false]),buildTree(BoolFunc,T,List,Path++[true])}.

%% Map for matching the parameters to their value
buildMap(M,[],[]) -> M;
buildMap(M,[H1|T1],[H2|T2]) -> buildMap(M#{H1 => H2},T1,T2).

%% Returns the argument value from the map
leafValue(M,{'not',Arg}) -> not leafValue(M,Arg);
leafValue(M,{'and',{Arg1,Arg2}}) -> leafValue(M,Arg1) and leafValue(M,Arg2);
leafValue(M,{'or',{Arg1,Arg2}}) -> leafValue(M,Arg1) or leafValue(M,Arg2);
leafValue(M,Arg) -> maps:get(Arg,M).

%% Reducing the tree according to left/right sons value (converting all true/false to 1/0 for mor readable tree)
reduceTree({Node,true,false}) -> {Node, 1, 0};                  % Different sons, end of tree
reduceTree({Node,false,true}) -> {Node, 0, 1};                  % Different sons, end of tree
reduceTree({_,false,false}) -> 0;                               % Same son, reduce! end of tree
reduceTree({_,true,true}) -> 1;                                 % Same son, reduce! end of tree
reduceTree({_,X,X}) -> reduceTree(X);                           % Same son, reduce! Keep reducing the son
reduceTree({Node,Left,Right}) -> {Node,reduceTree(Left),reduceTree(Right)}.

%%---------------------------------------------------------------------------------------------------------------------------------------------------

%%---------------------------------- Find the most efficient tree in the manner specified in variable Ordering --------------------------------------

%% The most efficient tree in the manner of tree height
bestOfTreeHeights(TreeList) ->
  Heights = listOfHeigths(TreeList,[]),
  MinIndex = findMinIndex(lists:min(Heights), Heights),
  lists:nth(MinIndex,TreeList).

%% List of all trees heights
listOfHeigths([],List) -> List;
listOfHeigths([H|T],List) -> listOfHeigths(T,List++[singleTreeHeight(H)]).

%% Calculate a single tree height
singleTreeHeight({_,Left,Right}) -> max(singleTreeHeight(Left)+1, singleTreeHeight(Right)+1);
singleTreeHeight(_) -> 0.

%% The most efficient tree in the manner of tree nodes
bestOfTreeNodes(TreeList) ->
  Nodes = listOfNodes(TreeList,[]),
  MinIndex = findMinIndex(lists:min(Nodes), Nodes),
  lists:nth(MinIndex,TreeList).

%% List of all trees number of nodes
listOfNodes([],List) -> List;
listOfNodes([H|T],List) -> listOfNodes(T,List++[singleTreeNodes(H,0)]).

%% Calculate a single tree number of nodes
singleTreeNodes({ _,Left,Right}, Nodes) -> singleTreeNodes(Left, Nodes) + singleTreeNodes(Right,Nodes) + 1;
singleTreeNodes(_,Nodes) -> Nodes+1.

%% The most efficient tree in the manner of tree leafs
bestOfTreeLeafs(TreeList) ->
  Leafs = listOfLeafs(TreeList,[]),
  MinIndex = findMinIndex(lists:min(Leafs), Leafs),
  lists:nth(MinIndex,TreeList).

%% List of all trees number of leafs
listOfLeafs([],List) -> List;
listOfLeafs([H|T],List) -> listOfLeafs(T,List++[singleTreeLeafs(H,0)]).

%% Calculate a single tree number of leafs
singleTreeLeafs({_,Left,Right}, Leafs) -> singleTreeNodes(Left, Leafs) + singleTreeNodes(Right,Leafs);
singleTreeLeafs(_,Leafs) -> Leafs+1.

%% Find the index of the minimum value in the list
findMinIndex(Item, List) -> findMinIndex(Item, List, 1).
findMinIndex(Item, [Item|_], Index) -> Index;
findMinIndex(Item, [_|Tl], Index) -> findMinIndex(Item, Tl, Index+1).

%%---------------------------------------------------------------------------------------------------------------------------------------------------

%%---------------------------------- solve function - Returns the result of the boolean function, according to the given BDD tree and variable map --

solve({Node,Left,Right},VarMap) ->
  case maps:get(Node,VarMap) of
    true -> solve(Right,VarMap);         % BDD representation - if true/1 we take the right son
    1 -> solve(Right,VarMap);
    false -> solve(Left,VarMap);         % BDD representation - if false/0 we take the left son
    0 -> solve(Left,VarMap)
  end;
solve(Leaf, _) -> Leaf.                  % Leaf - so return the value of the leaf

%%---------------------------------------------------------------------------------------------------------------------------------------------------

%%---------------------------------- Boolean Generator - generates list of equations ----------------------------------------------------------------

booleanGenerator(NumOfVars,NumOfEquations) -> booleanGenerator(NumOfVars,NumOfEquations,[]).
booleanGenerator(_,0,N) -> N;                 % Finish creating all the requested number of equations
booleanGenerator(V,E,N) ->
  BoolFunc = createBoolFunc(V,V+1),
  booleanGenerator(V,E-1,[BoolFunc|N]).

%% Create a random boolean function
createBoolFunc(V,0) -> randomVar(V);
createBoolFunc(V,R) ->
  % R is the maximum of the recursive calls we can make so the program won't run forever, V = NumOfVars
  case rand:uniform(8) of
    1 -> {'and',{createBoolFunc(V,R-1),createBoolFunc(V,R-1)}};
    2 -> {'or',{createBoolFunc(V,R-1),createBoolFunc(V,R-1)}};
    3 -> {'not', createBoolFunc(V,R-1)};
    4 -> {'and',{randomVar(V),createBoolFunc(V,R-1)}};
    5 -> {'or',{randomVar(V),createBoolFunc(V,R-1)}};
    6 -> {'and',{createBoolFunc(V,R-1),randomVar(V)}};
    7 -> {'or',{createBoolFunc(V,R-1),randomVar(V)}};
    8 -> {'not', randomVar(V)}
  end.

%% Choose a random variable out of x1,...,xn where n = NumOfVars
randomVar(NumOfVars) ->
  ListOfVars = [list_to_atom(lists:flatten(io_lib:format("x~B", [X]))) || X <- lists:seq(1, NumOfVars)],  % Create list of [x1,...,xn]
  lists:nth(rand:uniform(NumOfVars), ListOfVars).                                                         % Get random xi from the list

%%---------------------------------------------------------------------------------------------------------------------------------------------------