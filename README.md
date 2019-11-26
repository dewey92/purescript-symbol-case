# purescript-symbol-case

Functions for manipulating type level strings (symbols).

## Overview

I'm not even sure if this library is useful but imagine you have an external endpoint with undesirable label format (e.g in snake case) and you'd like to have'em in camel case.

```purs
import Record (rename)

type MovieResponse =
  { id :: MovieId
  , title :: String
  , poster_path :: Maybe String
  , adult :: Boolean
  , overview :: String
  , release_date :: String
  , popularity :: Number
  , vote_count :: Number
  , vote_average :: Number
  }

response :: MovieResponse

camelCaseKeys = rename (SProxy :: _ "poster_path") (SProxy :: _ "posterPath")
  >>> rename (SProxy :: _ "release_date") (SProxy :: _ "releaseDate")
  >>> rename (SProxy :: _ "vote_count") (SProxy :: _ "voteCount")
  >>> rename (SProxy :: _ "vote_average") (SProxy :: _ "voteAverage")

desired = camelCaseKeys response
```

Instead of converting them manually by hand, you can make use of this library to automatically convert them for you.

```purs
import Record.Case (toCamelRecord)

desired = toCamelRecord response
```

## Examples

You could navigate to [test/Main.purs](test/Main.purs) for more examples.
