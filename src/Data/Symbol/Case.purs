module Data.Symbol.Case where

import Data.Symbol (SProxy(..))
import Prim.Symbol as Sym

foreign import kind Specifier
foreign import data AlphaL :: Symbol -> Specifier
foreign import data AlphaU :: Symbol -> Specifier
foreign import data NonAlpha :: Symbol -> Specifier

foreign import kind CList
foreign import data CNil :: CList
foreign import data CCons :: Specifier -> CList -> CList

class CaseLookup (head :: Symbol) (matchingCase :: Symbol) (out :: Specifier)
  | head -> matchingCase out, matchingCase -> head out, out -> head matchingCase
instance caseLookupLowerA :: CaseLookup "a" "A" (AlphaL "a")
else instance caseLookupLowerB :: CaseLookup "b" "B" (AlphaL "b")
else instance caseLookupLowerC :: CaseLookup "c" "C" (AlphaL "c")
else instance caseLookupLowerD :: CaseLookup "d" "D" (AlphaL "d")
else instance caseLookupLowerE :: CaseLookup "e" "E" (AlphaL "e")
else instance caseLookupLowerF :: CaseLookup "f" "F" (AlphaL "f")
else instance caseLookupLowerG :: CaseLookup "g" "G" (AlphaL "g")
else instance caseLookupLowerH :: CaseLookup "h" "H" (AlphaL "h")
else instance caseLookupLowerI :: CaseLookup "i" "I" (AlphaL "i")
else instance caseLookupLowerJ :: CaseLookup "j" "J" (AlphaL "j")
else instance caseLookupLowerK :: CaseLookup "k" "K" (AlphaL "k")
else instance caseLookupLowerL :: CaseLookup "l" "L" (AlphaL "l")
else instance caseLookupLowerM :: CaseLookup "m" "M" (AlphaL "m")
else instance caseLookupLowerN :: CaseLookup "n" "N" (AlphaL "n")
else instance caseLookupLowerO :: CaseLookup "o" "O" (AlphaL "o")
else instance caseLookupLowerP :: CaseLookup "p" "P" (AlphaL "p")
else instance caseLookupLowerQ :: CaseLookup "q" "Q" (AlphaL "q")
else instance caseLookupLowerR :: CaseLookup "r" "R" (AlphaL "r")
else instance caseLookupLowerS :: CaseLookup "s" "S" (AlphaL "s")
else instance caseLookupLowerT :: CaseLookup "t" "T" (AlphaL "t")
else instance caseLookupLowerU :: CaseLookup "u" "U" (AlphaL "u")
else instance caseLookupLowerV :: CaseLookup "v" "V" (AlphaL "v")
else instance caseLookupLowerW :: CaseLookup "w" "W" (AlphaL "w")
else instance caseLookupLowerX :: CaseLookup "x" "X" (AlphaL "x")
else instance caseLookupLowerY :: CaseLookup "y" "Y" (AlphaL "y")
else instance caseLookupLowerZ :: CaseLookup "z" "Z" (AlphaL "z")
else instance caseLookupUpperA :: CaseLookup "A" "a" (AlphaU "A")
else instance caseLookupUpperB :: CaseLookup "B" "b" (AlphaU "B")
else instance caseLookupUpperC :: CaseLookup "C" "c" (AlphaU "C")
else instance caseLookupUpperD :: CaseLookup "D" "d" (AlphaU "D")
else instance caseLookupUpperE :: CaseLookup "E" "e" (AlphaU "E")
else instance caseLookupUpperF :: CaseLookup "F" "f" (AlphaU "F")
else instance caseLookupUpperG :: CaseLookup "G" "g" (AlphaU "G")
else instance caseLookupUpperH :: CaseLookup "H" "h" (AlphaU "H")
else instance caseLookupUpperI :: CaseLookup "I" "i" (AlphaU "I")
else instance caseLookupUpperJ :: CaseLookup "J" "j" (AlphaU "J")
else instance caseLookupUpperK :: CaseLookup "K" "k" (AlphaU "K")
else instance caseLookupUpperL :: CaseLookup "L" "l" (AlphaU "L")
else instance caseLookupUpperM :: CaseLookup "M" "m" (AlphaU "M")
else instance caseLookupUpperN :: CaseLookup "N" "n" (AlphaU "N")
else instance caseLookupUpperO :: CaseLookup "O" "o" (AlphaU "O")
else instance caseLookupUpperP :: CaseLookup "P" "p" (AlphaU "P")
else instance caseLookupUpperQ :: CaseLookup "Q" "q" (AlphaU "Q")
else instance caseLookupUpperR :: CaseLookup "R" "r" (AlphaU "R")
else instance caseLookupUpperS :: CaseLookup "S" "s" (AlphaU "S")
else instance caseLookupUpperT :: CaseLookup "T" "t" (AlphaU "T")
else instance caseLookupUpperU :: CaseLookup "U" "u" (AlphaU "U")
else instance caseLookupUpperV :: CaseLookup "V" "v" (AlphaU "V")
else instance caseLookupUpperW :: CaseLookup "W" "w" (AlphaU "W")
else instance caseLookupUpperX :: CaseLookup "X" "x" (AlphaU "X")
else instance caseLookupUpperY :: CaseLookup "Y" "y" (AlphaU "Y")
else instance caseLookupUpperZ :: CaseLookup "Z" "z" (AlphaU "Z")
else instance caseLookupNonAlpha :: CaseLookup head head (NonAlpha head)

class ParseSym (sym :: Symbol) (out :: CList) | sym -> out, out -> sym
instance parseNil :: ParseSym "" CNil
else instance parseOthers
  ::
  ( Sym.Cons head tail sym
  , CaseLookup head mc spec
  , ParseSym tail clistRest
  ) => ParseSym sym (CCons spec clistRest)

class ToUpperInterpreter (clist :: CList) (sym :: Symbol) | clist -> sym
instance toUpperInterpreterNil :: ToUpperInterpreter CNil ""
instance toUpperInterpreterAlphaLCons
  ::
  ( CaseLookup letter upper spec
  , Sym.Append upper tail out
  , ToUpperInterpreter rest tail
  )
  => ToUpperInterpreter (CCons (AlphaL letter) rest) out
else instance toUpperInterpreterOther
  ::
  ( Sym.Append other tail out
  , ToUpperInterpreter rest tail
  )
  => ToUpperInterpreter (CCons (spec other) rest) out

toUpper :: ∀ sym clist out.
  ParseSym sym clist =>
  ToUpperInterpreter clist out =>
  SProxy sym -> SProxy out
toUpper _ = SProxy

class ToLowerInterpreter (clist :: CList) (sym :: Symbol) | clist -> sym
instance toLowerInterpreterNil :: ToLowerInterpreter CNil ""
instance toLowerInterpreterAlphaUCons
  ::
  ( CaseLookup letter lower spec
  , Sym.Append lower tail out
  , ToLowerInterpreter rest tail
  )
  => ToLowerInterpreter (CCons (AlphaU letter) rest) out
else instance toLowerInterpreterOther
  ::
  ( Sym.Append other tail out
  , ToLowerInterpreter rest tail
  )
  => ToLowerInterpreter (CCons (spec other) rest) out

toLower :: ∀ sym clist out.
  ParseSym sym clist =>
  ToLowerInterpreter clist out =>
  SProxy sym -> SProxy out
toLower _ = SProxy

class ToCamelOrPascalInterpreter (clist :: CList) (sym :: Symbol) | clist -> sym
instance toCamelOrPascalIntrNil :: ToCamelOrPascalInterpreter CNil ""
instance toCamelOrPascalIntrConsNaL
  ::
  ( CaseLookup letter upper spec
  , Sym.Append upper tail out
  , ToCamelOrPascalInterpreter rest tail
  ) -- non alpha followed by a lowercase alpha
  => ToCamelOrPascalInterpreter (CCons (NonAlpha na) (CCons (AlphaL letter) rest)) out
else instance toCamelOrPascalIntrConsNa
  ::
  ( Sym.Append "" tail out
  , ToCamelOrPascalInterpreter rest tail
  )
  => ToCamelOrPascalInterpreter (CCons (NonAlpha na) rest) out
else instance toCamelOrPascalIntrConsAlpha
  ::
  ( Sym.Append letter tail out
  , ToCamelOrPascalInterpreter rest tail
  )
  => ToCamelOrPascalInterpreter (CCons (alpha letter) rest) out

class ToCamel (sym :: Symbol) (out :: Symbol) | sym -> out
instance toCamelEmpty :: ToCamel "" ""
else instance toCamelExists
  ::
  ( ParseSym sym clist
  , ToCamelOrPascalInterpreter clist rawOut
  -- make the head lowercase
  , Sym.Cons head tail rawOut
  , ParseSym head hClist
  , ToLowerInterpreter hClist headLower
  , Sym.Append headLower tail out
  ) => ToCamel sym out

class ToPascal (sym :: Symbol) (out :: Symbol) | sym -> out
instance toPascalEmpty :: ToPascal "" ""
else instance toPascalExists
  ::
  ( ParseSym sym clist
  , ToCamelOrPascalInterpreter clist rawOut
  -- make the head lowercase
  , Sym.Cons head tail rawOut
  , ParseSym head hClist
  , ToUpperInterpreter hClist headUpper
  , Sym.Append headUpper tail out
  ) => ToPascal sym out

toCamel :: ∀ sym out.
  ToCamel sym out =>
  SProxy sym -> SProxy out
toCamel _ = SProxy

toPascal :: ∀ sym out.
  ToPascal sym out =>
  SProxy sym -> SProxy out
toPascal _ = SProxy
