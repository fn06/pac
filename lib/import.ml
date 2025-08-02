module Option = struct
  include Option

  module Syntax = struct
    let ( let* ) = Option.bind
  end
end

module Result = struct
  include Result

  module Syntax = struct
    let ( let> ) = Result.bind
  end
end
