module StringConvertor

open Microsoft.FSharp.Reflection

let toString (x:'a) = 
    let (case, _ ) = FSharpValue.GetUnionFields(x, typeof<'a>)
    case.Name

let fromString<'a> (s:string) =
    match FSharpType.GetUnionCases typeof<'a> |> Array.filter (fun case -> case.Name = s) with
    |[|case|] -> Some(FSharpValue.MakeUnion(case,[||]) :?> 'a)
    |_ -> None