namespace Thoth.Parser

module Operators =

    let inline (|=) a b = Parser.keep b a

    let inline (|.) a b = Parser.drop b a
