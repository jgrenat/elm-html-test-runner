module Palindrome exposing (main)

import Expect
import Fuzz exposing (Fuzzer, list, string, tuple)
import Html exposing (Html, div, h1, text)
import Random
import Test exposing (Test, describe, fuzz, test)
import Test.Runner.Html exposing (defaultConfig, hidePassedTests, showPassedTests, viewResults, withFuzzCount)


type alias Person =
    { firstName : String
    , lastName : String
    }


config =
    Random.initialSeed 10000
        |> defaultConfig
        |> withFuzzCount 50
        |> showPassedTests


main : Html msg
main =
    div []
        [ h1 [] [ text "My Test Suite" ]
        , div [] [ viewResults config myTestSuite ]
        ]


sortByFirstNameThenLastName : List Person -> List Person
sortByFirstNameThenLastName persons =
    List.sortBy (\person -> person.firstName ++ person.lastName) persons


myTestSuite : Test
myTestSuite =
    describe "`sortByFirstNameThenLastName`"
        [ test "should be case-insensitive" <|
            \_ -> Expect.equalLists [ hannahSmith, henrySmith ] (sortByFirstNameThenLastName [ henrySmith, hannahSmith ])
        , test "should compare first name before last name" <|
            \_ -> Expect.equalLists [ evaGreen, evanCzaplicki ] (sortByFirstNameThenLastName [ evaGreen, evanCzaplicki ])
        , fuzz (list personFuzzer) "should not change an already-sorted list" <|
            \unsortedList ->
                let
                    sortedList =
                        sortByFirstNameThenLastName unsortedList
                in
                Expect.equalLists sortedList (sortByFirstNameThenLastName sortedList)
        ]


personFuzzer : Fuzzer Person
personFuzzer =
    tuple ( string, string ) |> Fuzz.map (\( firstName, lastName ) -> Person firstName lastName)


henrySmith =
    Person "Henry" "Smith"


hannahSmith =
    Person "hannah" "Smith"


evaGreen =
    Person "Eva" "Turner"


evanCzaplicki =
    Person "Evan" "Czaplicki"
