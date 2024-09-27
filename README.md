# Overview
A simple Computer Algebra System developed in Haskell.
Parsec is utilised for parsing mathematical expressions.

Current functionality include:
- Expansion and simplification of functions
- Series evaluation from start to end for a given f(n)
- Sequence generation for a given f(n)
- Taylor series expansion of nth degree around a for a given f.
- Fourier series expansion of n terms around a for a given f.
- Numerical differentiation via Finite Difference Method with a given epsilon precision.
- Numerical Integration via Simpson's rule for a given interval size d and epsilon precision.
- Root finding via Newton-Rhapson method with a given epsilon precision.

A cabal file is included for building.
- - - -
# Syntax
For expressions (of the Expr data type) f and g, 

**Addition**
```
f :+: g
```
**Multiplication**
```
f :*: g
```
**Subtraction**
(*Negated Addition*)
```
f :+: Const (-1) :*: g
```
**Division**
```
f :/: g
```
**Exponentiation**
```
f :^: g
```
- - - -
# Parsing
Similarly the syntax for parsing is as follows:

**Addition**
```
f + g
```
**Multiplication**
```
f * g
```
**Subtraction**
```
f - g
```
**Division**
```
f / g
```
**Exponentiation**
```
f ^ g
```
