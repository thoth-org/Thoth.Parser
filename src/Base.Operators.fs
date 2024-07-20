namespace Thoth.Parser.Base

module Operators =

    let (|=) = Parser.keep

    let (|.) = Parser.drop
