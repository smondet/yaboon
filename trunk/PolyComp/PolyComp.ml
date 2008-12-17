
module Comparisons = struct
    let eqpoly = (=)
    let nepoly = (<>)
    let ltpoly = (<)
    let gtpoly = (>)
    let lepoly = (<=)
    let gepoly = (>=)

    let eqphy = (==)

    let eqi (a:int) (b:int)       = eqpoly a b
    let eqf (a:float) (b:float)   = eqpoly a b
    let nei (a:int) (b:int)       = nepoly a b
    let nef (a:float) (b:float)   = nepoly a b
    let lti (a:int) (b:int)       = ltpoly a b
    let ltf (a:float) (b:float)   = ltpoly a b
    let gti (a:int) (b:int)       = gtpoly a b
    let gtf (a:float) (b:float)   = gtpoly a b
    let lei (a:int) (b:int)       = lepoly a b
    let lef (a:float) (b:float)   = lepoly a b
    let gei (a:int) (b:int)       = gepoly a b
    let gef (a:float) (b:float)   = gepoly a b

    let eqs (a:string) (b:string) = (String.compare a b) = 0

    let minpoly = min
    let maxpoly = max
    let mini  (a:int) (b:int)      = minpoly a b
    let minf (a:float) (b:float)   = minpoly a b
    let maxi  (a:int) (b:int)      = maxpoly a b
    let maxf (a:float) (b:float)   = maxpoly a b
end
module NoPolyPhy = struct
    type shouldnt_be_used
    let (==) (x:shouldnt_be_used) (y:shouldnt_be_used) = false
    let (!=) (x:shouldnt_be_used) (y:shouldnt_be_used) = false
end

module CompAndNoPolyPhy = struct
    include Comparisons
    include NoPolyPhy
end

module CompAndOveridePoly = struct
    include CompAndNoPolyPhy
    let (=)  = eqi
    let (=.) = eqf
    let (<>)  = eqi
    let (<>.) = eqf
    let (<)  = lti
    let (<.) = ltf
    let (>)  = gti
    let (>.) = gtf
    let (<=) = lei
    let (<=.)= lef
    let (>=) = gei
    let (>=.)= gef
    let (=$=) = eqs
    let (=@=) = eqpoly
    let min = mini
    let max = maxi
end


