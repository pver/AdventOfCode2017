open System

type Component = {input:int;output:int}

type Solution = {side1:int;side2:int;score:int;included:Component list;possibleOthers:Component list}

let inputExample = [{input=0;output=2};
                    {input=2;output=2};
                    {input=2;output=3};
                    {input=3;output=4};
                    {input=3;output=5};
                    {input=0;output=1};
                    {input=10;output=1};
                    {input=9;output=10}]

let inputChallenge = [
                        {input=32;output=31};
                        {input=2;output=2};
                        {input=0;output=43};
                        {input=45;output=15};
                        {input=33;output=24};
                        {input=20;output=20};
                        {input=14;output=42};
                        {input=2;output=35 };
                        {input=50;output=27};
                        {input=2;output=17 };
                        {input=5;output=45 };
                        {input=3;output=14 };
                        {input=26;output=1 };
                        {input=33;output=38};
                        {input=29;output=6 };
                        {input=50;output=32};
                        {input=9;output=48 };
                        {input=36;output=34};
                        {input=33;output=50};
                        {input=37;output=35};
                        {input=12;output=12};
                        {input=26;output=13};
                        {input=19;output=4 };
                        {input=5;output=5  };
                        {input=14;output=46};
                        {input=17;output=29};
                        {input=45;output=43};
                        {input=5;output=0  };
                        {input=18;output=18};
                        {input=41;output=22};
                        {input=50;output=3 };
                        {input=4;output=4  };
                        {input=17;output=1};
                        {input=40;output=7};
                        {input=19;output=0};
                        {input=33;output=7};
                        {input=22;output=48};
                        {input=9;output=14 };
                        {input=50;output=43};
                        {input=26;output=29};
                        {input=19;output=33};
                        {input=46;output=31};
                        {input=3;output=16 };
                        {input=29;output=46};
                        {input=16;output=0 };
                        {input=34;output=17};
                        {input=31;output=7 };
                        {input=5;output=27 };
                        {input=7;output=4  };
                        {input=49;output=49};
                        {input=14;output=21};
                        {input=50;output=9 };
                        {input=14;output=44};
                        {input=29;output=29};
                        {input=13;output=38};
                        {input=31;output=11} ]

let startItems (components:Component list) = 
    components 
                 |> List.map (fun x -> {side1=x.input; 
                                        side2=x.output; 
                                        score=x.input+x.output; included=[x]; 
                                        possibleOthers=(components|>List.filter ((<>)x))})

//startItems inputExample |> printfn "%A"

let extendSolution (s:Solution) =
    let possibleMatchesSide1 = s.possibleOthers |> List.filter (fun x -> x.input = s.side1 || x.output = s.side1)
    let extendedSide1 = match possibleMatchesSide1 with
                        | [] -> [{s with possibleOthers=[]}]
                        | _ -> possibleMatchesSide1 |> List.map (fun x -> {s with side1=(if x.input=s.side1 then x.output else x.input);
                                                                              score=(s.score+x.input+x.output);
                                                                              included=(s.included @ [x]);
                                                                              possibleOthers=(s.possibleOthers|>List.filter ((<>)x))})

    let possibleMatchesSide2 = s.possibleOthers |> List.filter (fun x -> x.input = s.side2 || x.output = s.side2)
    let extendedSide2 = match possibleMatchesSide2 with
                        | [] -> [{s with possibleOthers=[]}]
                        | _ -> possibleMatchesSide2 |> List.map (fun x -> {s with side2=(if x.input=s.side2 then x.output else x.input);
                                                                              score=(s.score+x.input+x.output);
                                                                              included=(s.included @ [x]);
                                                                              possibleOthers=(s.possibleOthers|>List.filter ((<>)x))})
    extendedSide1@extendedSide2

let rec findStrongestSolution (s:Solution list) (maxSoFar:int) =
    let maxUnexpandables = s |> List.filter (fun x->x.possibleOthers=[]) |> List.map (fun x->x.score) |> List.max
    let canBeExtended = s |> List.filter (fun x->x.possibleOthers<>[])
    match canBeExtended with
    | [] -> s |> List.filter (fun x -> x.side1=0 || x.side2=0) |> List.sortByDescending (fun x -> x.score) |> List.head |> fun x -> x.score
    | _ -> findStrongestSolution <| (s |> List.collect extendSolution)

startItems inputExample |> findStrongestSolution |> printfn "Strongest example: %A"

startItems inputChallenge |> findStrongestSolution |> printfn "Strongest Challenge 24 A: %A"