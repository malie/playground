type Rulename = String
data Parser
     = Def Rulename Parser
     | Seq [Parser]
     | Alt [Parser]
     | Ch Char
     | Range Char Char
     | Rep Parser
     | Opt Parser
     | Ref Rulename
     | Not Parser -- ?
     | Wordboundary -- ?
     | Empty -- ?

gram1 =
  [ Def "upper"      (Range 'A' 'Z')
  , Def "lower"      (Range 'a' 'z')
  , Def "digit"      (Range '0' '9')
  , Def "integer"    (Rep (Ref "digit"))
  , Def "wordchar"   (Alt [Ref "digit", Ref "lower", Ref "upper"])
  , Def "space"      (Ch ' ')
  , Def "optspaces"  (Opt (Rep (Ref "space")))
  , Def "spaces"     (Seq [Ref "space", Ref "optspaces"])
  , Def "var"        (Seq [Ref "upper", Opt (Rep (Ref "wordchar"))])
  , Def "symbol"     (Seq [Ref "lower", Opt (Rep (Ref "wordchar"))])
  , Def "element"    (Alt [Ref "integer", Ref "var", Ref "symbol"])
  , Def "list"       (Rep (Seq [Ref "element", Ref "spaces"]))
  , Def "entry"      (Ref "list")
  ]

type Id = Int

-- id for every fraction of the grammar, some correspond to rule names,
-- but also one for every sublist of every seq!

-- or use the fractions as ID's!
-- and use lists of IDs, so no ID's for sublists are needed

type Input = String

data PartialMatch = [Parser]
data Match = Match Input [PartialMatch]

rew1 = [
  
     ]

