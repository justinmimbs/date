module Shim exposing (Expectation, Test, describe, equal, run, test)

-- temporary shim for elm-test


type Test
    = One String (() -> Expectation)
    | Many String (List Test)


type Expectation
    = Pass
    | NotEqual ( String, String )


equal : a -> a -> Expectation
equal x y =
    if x == y then
        Pass

    else
        NotEqual ( Debug.toString x, Debug.toString y )


test : String -> (() -> Expectation) -> Test
test =
    One


describe : String -> List Test -> Test
describe =
    Many


{-| test -> ( count of tests run, [ ( failing test context, not-equal pair ) ] )
-}
run : Test -> ( Int, List ( List String, ( String, String ) ) )
run t =
    case t of
        One desc thunk ->
            ( 1
            , case thunk () of
                Pass ->
                    []

                NotEqual pair ->
                    [ ( [ desc ], pair ) ]
            )

        Many desc tests ->
            List.foldl
                (\t1 result ->
                    run t1
                        |> Tuple.mapSecond (List.map (Tuple.mapFirst ((::) desc)))
                        |> append result
                )
                empty
                tests


empty : ( Int, List a )
empty =
    ( 0, [] )


append : ( Int, List a ) -> ( Int, List a ) -> ( Int, List a )
append ( n2, list2 ) ( n1, list1 ) =
    ( n1 + n2, list1 ++ list2 )
