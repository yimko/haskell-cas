{-|
Definitions for mathematical expressions involving elementary operators and functions.
Includes definitions of their derivatives and a function for evaluation at a point.

Functionality for expansion and simplification.

Example numerical methods in the CAS:

* Series evaluation from start to end for a given f(n)

* Sequence generation for a given f(n)

* Taylor series expansion of nth degree around a for a given f.

* Fourier series expansion of n terms around a for a given f.

* Numerical differentiation via Finite Difference Method with a given epsilon precision.

* Numerical Integration via Simpson's rule for a given interval size d and epsilon precision.

* Root finding via Newton-Rhapson method with a given epsilon precision.

-}
module Functions where

data Expr = X
            | Const Double
            | Expr :+: Expr
            | Expr :/: Expr
            | Expr :*: Expr
            | Expr :^: Expr
            | Apply Func Expr
            deriving Eq

data Func = Sqrt
            | Cos
            | Sin
            | Tan
            | Log
            | Exp
            | Asin
            | Acos
            | Atan
            | Cosh
            | Sinh
            | Tanh
            | Acosh
            | Asinh
            | Atanh
            deriving (Show, Eq)

--Define order based on coefficients (for list sorting)
instance Ord Expr where
    compare (Const a) (Const b) = compare a b
    compare (Const a :*: f) (Const b :*: g) = compare a b
    compare (Const a :*: f) g = compare a 1
    compare g (Const a :*: f) = compare 1 a
    compare f g = EQ

infixl 5 :+:
infixl 6 :*:, :/:
infixr 7 :^:

instance Show Expr where
    showsPrec p e0 =
        case e0 of
            X -> showParen (p > 10) $ showString "x"
            Const a -> showParen (p > 10) $ showsPrec 11 a
            Apply f a -> showParen (p > 10) $ showString (show f) . showsPrec 11 a
            x :+: y -> showParen (p > 5) $ showsPrec 5 x . showString " + " . showsPrec 6 y
            x :*: y -> showParen (p > 6) $ showsPrec 6 x . showString " * " . showsPrec 7 y
            x :/: y -> showParen (p > 6) $ showsPrec 6 x . showString " / " . showsPrec 7 y
            x :^: y -> showParen (p > 7) $ showsPrec 8 x . showString " ^ " . showsPrec 7 y


{-|
    Evaluates an expression at a given double

        * Parameters:

            * The expression to be evaluated at (See Expr data type)

            * The value to be evaluated at (double)

        * Returns the result as a double
-}
eval::Expr->Double->Double
eval X a = a
eval (Const a) _ = a
eval (f :+: g) a = eval f a + eval g a
eval (f :/: g) a = eval f a / eval g a
eval (f :*: g) a = eval f a * eval g a
eval (f :^: g) a = eval f a ** eval g a
--functions
eval (Apply Sqrt f) a = sqrt (eval f a)
eval (Apply Log f) a = log (eval f a)
eval (Apply Exp f) a = exp (eval f a)
--trignometric functions
eval (Apply Cos f) a = cos (eval f a)
eval (Apply Sin f) a = sin (eval f a)
eval (Apply Tan f) a = tan (eval f a)
eval (Apply Asin f) a = asin (eval f a)
eval (Apply Acos f) a = acos (eval f a)
eval (Apply Atan f) a = atan (eval f a)
--hyperbolic functins
eval (Apply Cosh f) a = cosh (eval f a)
eval (Apply Tanh f) a = tanh (eval f a)
eval (Apply Sinh f) a = sinh (eval f a)
eval (Apply Asinh f) a = asinh (eval f a)
eval (Apply Acosh f) a = acosh (eval f a)
eval (Apply Atanh f) a = atanh (eval f a)

{-|
    1-step expand the given expression

        * Parameters: an expression

        * Returns the expanded expression
-}
expand::Expr->Expr
expand X = X
expand (Const a) = Const a
expand (Apply f a) = Apply f a
--Multiplication Distributive
expand (f :*: (g :+: h)) = (expand.c_rewrite $ (f :*: g)) :+: (expand.c_rewrite $ (f :*: h))
expand ((g :+: h) :*: f) = (expand.c_rewrite $ (f :*: g)) :+: (expand.c_rewrite $ (f :*: h))
--Division Distributive
expand ((g :+: h) :/: f) = (expand.c_rewrite $ (g :/: f)) :+: (expand.c_rewrite $ (h :/: f))
expand ((g :*: h) :/: f) = (expand.c_rewrite $ (g :/: f)) :*: (expand.c_rewrite $ (h :/: f))
--duplicate multiplication
expand (f :^: Const a)
    | a == 1 = f
    | otherwise = f :*: expand (f :^: Const (a-1))
expand (f :+: g) = expand f :+: expand g
expand (f :*: g) = expand f :*: expand g
expand (f :/: g) = expand f :/: expand g
expand (f :^: g) = expand f :^: expand g

{-|
    Fully expand the given expression into sums

        * Parameters: an expression

        * Returns the expanded expression
-}
c_expand::Expr->Expr
c_expand f = if expand f == f then simplify f else c_expand (expand f)

{-|
    Fully rewrite the given expression

        * Parameters: an expression

        * Returns the rewritten expression
-}
c_rewrite::Expr->Expr
c_rewrite f = if rewrite f == f then f else c_rewrite (rewrite f)

{-|
    Simplify the given expression

        * Parameters: an expression

        * Returns the simplified expression
-}
simplify::Expr->Expr
simplify (f :/: g) = c_rewrite $ simplify f :/: simplify g -- simplify to expanded/expanded form
simplify (f :*: g :*: h) = c_rewrite.group_mul $ (f :*: g :*: h)
simplify (f :*: (g :*: h)) = simplify (f :*: g :*: h)
simplify  (f :+: g :+: h) = c_rewrite.group_add $ (f :+: g :+: h)
simplify (f :+: (g :+: h)) = simplify (f :+: g :+: h)
simplify f = c_rewrite f

--group terms by product
group_mul::Expr->Expr
group_mul f = let xs = h_mul_group (unique.c_list $ clean) clean
                     where l = sort.tolist_mul $ f
                           clean = simplify <$> l 
              in prodterms xs

--product of terms by occurences
prodterms::[(Double,Expr)]->Expr
prodterms [(n, Const 1)] = Const n
prodterms [x]
    | fst x == 1 = snd x
    | otherwise = snd x :^: Const (fst x)
prodterms (x:xs)
    | fst x == 1 = snd x :*: prodterms xs
    | otherwise = (snd x :^: Const (fst x)) :*: prodterms xs

--group terms by sum
group_add::Expr->Expr
group_add f = let xs = h_add_group (unique.c_list $ clean) clean
                     where l = sort.tolist $ f
                           clean = simplify <$> l 
              in sumterms xs

--sum terms by occurences
sumterms::[(Double,Expr)]->Expr
sumterms [x]
    | fst x == 1 = snd x
    | otherwise = Const (fst x) :*: snd x
sumterms (x:xs)
    | fst x == 1 = snd x :+: sumterms xs
    | otherwise = Const (fst x) :*: snd x :+: sumterms xs

--frequency table
h_add_group::[Expr]->[Expr]->[(Double,Expr)]
h_add_group [] _ = []
h_add_group _ [] = []
h_add_group (y:ys) xs = (add_co y xs, y):h_add_group ys xs

h_mul_group::[Expr]->[Expr]->[(Double,Expr)]
h_mul_group [] _ = []
h_mul_group _ [] = []
h_mul_group (y:ys) xs = (mul_co y xs, y):h_mul_group ys xs

mul_co::Expr->[Expr]->Double
mul_co (Const a) (Const b:xs) = b*mul_co (Const a) xs
mul_co (Const a) (x:xs) = mul_co (Const a) xs
mul_co (Const a) [] = 1
mul_co n f = count_occur n f

add_co::Expr->[Expr]->Double
add_co (Const a) (Const b:xs) = b + add_co (Const a) xs
add_co (Const a) (x:xs) = add_co (Const a) xs
add_co (Const a) []  = 0
add_co n f = count_occur n f

count_occur::Expr->[Expr]->Double
count_occur (Const a :*: f) ((Const b :*: g):xs)
    | f == g = a+b + count_occur f xs
    | otherwise = count_occur f xs
count_occur (Const a :*: f) (g:xs)
    | f == g = a + count_occur f xs
    | otherwise = count_occur f xs
count_occur f ((Const a :*: g):xs)
    | f == g = a + count_occur f xs
    | otherwise = count_occur f xs
count_occur f (t:xs)
    | t == f = 1 + count_occur f xs
    | otherwise = count_occur f xs
count_occur f [] = 0

--removes duplicate terms
unique::[Expr]->[Expr]
unique [] = []
unique (x:xs) = x:unique (filter ( x /=) xs)

--get basis
c_list::[Expr]->[Expr]
c_list [] = []
c_list ((Const a):xs)
    | a /= 0 = Const 1:c_list xs
    | otherwise = c_list xs
c_list ((Const a :*: g):xs) = g:c_list xs
c_list (x:xs) = x:c_list xs

--sort list based on coefficients
sort::[Expr]->[Expr]
sort [] = []
sort (x:xs) = sort left ++ [x] ++ sort right
            where left = filter (<= x) xs
                  right = filter (> x) xs

tolist::Expr->[Expr]
tolist (f :+: h) = tolist f ++ tolist h
tolist f = [f]

tolist_mul::Expr->[Expr]
tolist_mul (f :*: h) = tolist_mul f ++ tolist_mul h
tolist_mul f = [f]


--Helper function to clear redundant terms in an expression
rewrite::Expr->Expr
rewrite (X :*: Const a) = Const a :*: X
rewrite (Apply f h :*: Const a) = Const a :*: Apply f h
-- Constants
rewrite (Const a :+: Const b)   = Const (a + b)
rewrite (Const a :*: Const b)   = Const (a * b)
rewrite (Const a :/: Const b)   = Const (a / b)
rewrite (Const a :^: Const b)   = Const (a ** b)
--Identity for product
rewrite (f :*: Const 1)  = rewrite f
rewrite (Const 1 :*: f)  = rewrite f
rewrite (f :*: Const 0)  = Const 0
rewrite (Const 0 :*: f)  = Const 0
--Identity for sum
rewrite (f :+: Const 0)  = rewrite f
rewrite (Const 0 :+: f)  = rewrite f
--Identity for division
rewrite (Const 0 :/: f)  = Const 0
rewrite (f :/: Const 1)  = rewrite f
--Identity for powers
rewrite (f :^: Const 1) = rewrite f
rewrite (f :^: Const 0) = Const 1
rewrite (Const 1 :^: f) = Const 1
--Inverses
rewrite (f :+: Const (-1) :*: g)
    | f == g = Const 0
--Functional inverses
rewrite (Apply Sqrt (f :^: Const 2) ) = rewrite f
rewrite ((Apply Sqrt f) :^: Const 2) = rewrite f
rewrite (Apply f (Apply g b)) = h_rewrite (Apply f (Apply g b))
rewrite (Apply f e) = Apply f (rewrite e)
--powers
rewrite ((f :^: Const b) :^: Const a) = f :^: Const (a+b)
rewrite (f :^: g) = rewrite f :^: rewrite g
--products
rewrite (Const (-1) :*: (Const (-1) :*: g)) = g
rewrite (f :*: g :^: Const a)
    | f == g = f :^: Const (a+1)
rewrite (g :^: Const a :*: f)
    | f == g = f :^: Const (a+1)
rewrite (g :^: f :*: h :^: k) = rewrite g :^: rewrite f :*: rewrite h :^: rewrite k
rewrite (g :^: h :*: f) = rewrite f :*: (rewrite g :^: rewrite h)
rewrite (f :*: g)
    | f == g = f :^: Const 2
    | otherwise = rewrite f :*: rewrite g
--division
rewrite (f :/: g) = rewrite f :/: rewrite g
--Sums
rewrite (f :+: Const b :*: g)
    | f == g = Const (b+1) :*: f
rewrite (Const b :*: g :+: f)
    | f == g = Const (b+1) :*: f
rewrite (f :+: g)
    | f == g = Const 2 :*: f
    | otherwise = rewrite f :+: rewrite g
rewrite f = f

--Inverses
h_rewrite::Expr->Expr
h_rewrite (Apply Exp  (Apply Log  f)) = rewrite f
h_rewrite (Apply Log  (Apply Exp  f)) = rewrite f
--trig 
h_rewrite (Apply Sin  (Apply Asin f)) = rewrite f
h_rewrite (Apply Cos  (Apply Acos f)) = rewrite f
h_rewrite (Apply Tan  (Apply Atan f)) = rewrite f
h_rewrite (Apply Asin (Apply Sin  f)) = rewrite f
h_rewrite (Apply Acos (Apply Cos  f)) = rewrite f
h_rewrite (Apply Atan (Apply Tan f))  = rewrite f
--hyperbolic
h_rewrite (Apply Sinh  (Apply Asinh f)) = rewrite f
h_rewrite (Apply Cosh  (Apply Acosh f)) = rewrite f
h_rewrite (Apply Tanh  (Apply Atanh f)) = rewrite f
h_rewrite (Apply Asinh (Apply Sinh  f)) = rewrite f
h_rewrite (Apply Acosh (Apply Cosh  f)) = rewrite f
h_rewrite (Apply Atanh (Apply Tanh f))  = rewrite f
h_rewrite f = f

{-|
    Returns a simplified first order derivative of the given expression

        * Parameters: an expression

        * Returns the first derivative
-}
clean_diff::Expr->Expr
clean_diff = simplify.diff

{-|
    Differentiate the given expression without simplification

        * Parameters: an expression

        * Returns the first derivative
-}
diff::Expr->Expr
diff (Const _) = Const 0
diff X = Const 1
diff (f :^: Const a) =  Const a :*: diff f :*: (f :^: Const (a-1))
diff (f :+: g) =  diff f :+: diff g
diff (f :*: g) =  f :*: diff g :+: g :*: diff f
diff (f :/: g) = (diff f :*: g :+: Const (-1) :*: f :*: diff g) :/: (g :^: Const 2)
diff (Apply f a) = h_diff (Apply f a) :*: diff a
--Functional derivatives
h_diff::Expr->Expr
h_diff (Apply Sqrt a) = Const (1/2) :*: (X :^: Const (-1/2))
h_diff (Apply Cos a) = Const (-1) :*: Apply Sin a
h_diff (Apply Sin a) = Apply Cos a
h_diff (Apply Tan a) = Const 1 :/: (Apply Cos a :^: Const 2)
h_diff (Apply Log a) = Const 1 :/: a
h_diff (Apply Exp a) = Apply Exp a
h_diff (Apply Asin a) = Const 1 :/: Apply Sqrt (Const 1 :+: Const (-1) :*: a :^: Const 2)
h_diff (Apply Acos a) = Const (-1) :*: Const 1 :/: Apply Sqrt (Const 1 :*: Const (-1) :*: a :^: Const 2)
h_diff (Apply Atan a) = Const 1 :/: (X:^:Const 2 :+: Const 1)
h_diff (Apply Cosh a) = Apply Sinh a
h_diff (Apply Sinh a) = Apply Cosh a
h_diff (Apply Tanh a) = Const 1 :/: (Apply Cosh a :^: Const 2)
h_diff (Apply Acosh a) = Const 1 :/: Apply Sqrt (a :^: Const 2 :+: Const (-1) :*: Const 1)
h_diff (Apply Asinh a) = Const 1 :/: Apply Sqrt (Const 1 :+: a :^: Const 2)
h_diff (Apply Atanh a) = Const 1 :/: ( Const 1 :+: Const (-1) :*: X:^:Const 2)

--Series and Sequences
{-|
    Calculate the series from start to end where the summand is the given expression in terms of X

        * Parameters:

            * Start of iteration (integer)

            * Terminal value of iteration (integer)

            * Summand function

        * Returns the sum as a double
-}
scal::Integer->Integer->Expr->Double
scal start end f
    | start == end = eval f (fromIntegral end)
    | otherwise = eval f (fromIntegral start) + scal (start+1) end f

{-|
    Generate a sequence of values where the nth term is defined by the given expression in terms of X

        * Parameters:

            * Start of iteration (integer)

            * Terminal value of iteration (integer)

            * Expression defining nth term

        * Returns the result as a double
-}
genseq::Integer->Integer->Expr->[Double]
genseq start end f
    | start == end = [eval f (fromIntegral start)]
    | otherwise = eval f (fromIntegral start):genseq (start+1) end f

--Numerical Tools
{-|
    Find the nth degree taylor polynomial around a for the given expression

        * Parameters:

            * the degree (integer)

            * evaluation point (double)

            * expression

        * Returns the taylor expansion
-}
taylor_exp::Integer->Double->Expr->Expr
taylor_exp n a f = simplify.sumterms $ h_taylor_exp n a f

--round to 3 decimals of precision
h_taylor_exp::Integer->Double->Expr->[(Double, Expr)]
h_taylor_exp 0 a f = [(1, Const(eval f a))]
h_taylor_exp n a f = (1 , simplify $ Const (eval (l_diff n f) a/factorial n 1) :*: ((X :+: Const (-1) :*: Const a) :^: Const (fromIntegral n)) ):h_taylor_exp (n-1) a f

--take f' until n = 0
l_diff::Integer->Expr->Expr
l_diff 0 f = f
l_diff n f = l_diff (n-1) (clean_diff f)

--product from k(including) to n/factorial if k =1
factorial::Integer->Integer->Double
factorial 0 _ = 1
factorial _ 0 = 1
factorial n k
    | n == k = fromIntegral n
    | otherwise = factorial (n-1) k * fromIntegral n

--combinations nCr
choose::Integer->Integer->Double
choose n r
    | n-r >= r = factorial n (n-r+1) / factorial r 1
    | otherwise = factorial n r / factorial (n-r) 1

--Fourier Expansion with estimated coefficients for a given function f around a for n terms
--Where L = p/2, for a given offset a (e.g from a to a+2L)
{-|
    Find the nth fourier expansion around a for a function of a period (2L)

        * Parameters:

            * Number of terms (inetger)

            * Period in terms of L, e.g L = p/2

            * Expression

        * Returns the fourier expansion with coefficients rounded to 3 decimal places
-}
fourier_exp::Integer->Double->Double->Expr->Expr
fourier_exp n l a f = simplify.sumterms $ h_fourier_exp n l a f

--Round to 3 decimals of precision, step size of 0.01
h_fourier_exp::Integer->Double->Double->Expr->[(Double, Expr)]
h_fourier_exp 0 l a f = [(1,Const (roundTo 3 $ 1/(2*l) * srule a (a+2*l) 0.01 f))]
h_fourier_exp n l a f = costerm:sinterm:h_fourier_exp (n-1) l a f
    where costerm = (1 , simplify $ Const (roundTo 3 $ 1/l * srule a (a+2*l) 0.01 (f :*: Apply Cos (Const (fromIntegral n * pi/l) :*: X))) :*: Apply Cos (Const (roundTo 3 $ fromIntegral n * pi/l) :*: X))
          sinterm = (1 , simplify $ Const (roundTo 3 $ 1/l * srule a (a+2*l) 0.01 (f :*: Apply Sin (Const (fromIntegral n * pi/l) :*: X))) :*: Apply Sin (Const (roundTo 3 $ fromIntegral n * pi/l) :*: X))

roundTo::Int -> Double -> Double
roundTo n x = fromInteger (round $ x * (10^n)) / 10.0^^n

--Numerical Algorithms
--Finite Difference Method; given an epsilon of precision at a for a given function f
{-|
    Approximate the derivative f'(a) via Finite Differences for given epsilon.

        * Parameters:

            * Value to be evaluated at (double)

            * epsilon threshold for convergence (double)

            * Expression

        * Returns the value of the derivative of the expression (if it exists) at a
-}
fdm::Double->Double->Expr->Double
fdm epsilon a f = (eval f (a + epsilon) - eval f a) / epsilon

{-|
    Approximate the integral from a to b of the expression for a given interval size

        * Parameters:

            * Start of integral (double)

            * End of integral (double)

            * Interval size (double)

            * Integrand function

        * Returns the sum as a double
-}
srule::Double->Double->Double->Expr->Double
srule a b d f = h/3 * (eval f a + 4*sum oddterms + 2*sum eventerms + eval f b)
                where n = if even (round $ (b-a)/d) then round $ (b-a)/d else (round $ (b-a)/d)+1
                      h = (b-a)/fromIntegral n
                      oddterms = [eval f (a + fromIntegral i * h) | i <- filter odd [1 .. n-1]]
                      eventerms = [eval f (a+fromIntegral i*h) | i<- filter even [1..n-2]]

{-|
    Perform Newton-Rhapson for the given expression at x0 with epsilon precision

        * Parameters:

            * The initial guess x0 (double)

            * epsilon threshold (double)

            * Expression

        * Returns the approximated root (if it exists)
-}
newtrhap::Double->Double->Expr->Double
newtrhap x0 epsilon f = h_newtrhap (1/0) x0 epsilon f (clean_diff f)

h_newtrhap::Double->Double->Double->Expr->Expr->Double
h_newtrhap x1 x0 e f f'
    | abs (x1 - x0) < e = x1
    | otherwise = if x1 == 1/0 then h_newtrhap (x0 - eval f x0/eval f' x0) x0 e f f' else h_newtrhap (x1 - eval f x1/eval f' x1) x1 e f f'