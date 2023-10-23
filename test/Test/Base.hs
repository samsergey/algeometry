module Test.Base (testSuite) where

import Test.Tasty
import Test.Tasty.QuickCheck
import Algeometry.GeometricAlgebra
import Algeometry.Arbitrary

bladeTests = testGroup "Blades tests"
  [ testProperty "blades square to scalars" $
    \(Monom m) -> isScalar $ m*m

  , testGroup "Outer product"
    [ testProperty "unitary" $
      \(Monom x) -> 1 ∧ x == x && x ∧ 1 == x

    , testProperty "zero" $ 
      \(Monom x) -> 0 ∧ x == 0 && x ∧ 0 == 0

    , testProperty "commutative for scalars" $ 
      \(Monom x) s ->
        fromInteger s ∧ x == x ∧ fromInteger s

    , testProperty "associative" $ 
      \(Monom a) (Monom b) (Monom c) ->
        a ∧ (b ∧ c) == (a ∧ b) ∧ c

    , testProperty "anticommutative" $
      \(Monom x) (Monom y) ->
        x ∧ y == (-1)^(grade x*grade y) * (y ∧ x)

    , testProperty "via projections" $ 
      \(Monom x) (Monom y) ->
        x ∧ y == getGrade (grade x+grade y) (x * y)
    ]

  , testGroup "Geometric product"
    [ testProperty "unitary" $ 
      \(Monom x) -> 1 * x == x && x * 1 == x

    , testProperty "zero" $ 
      \(Monom x) -> 0 * x == 0 && x * 0 == 0

    , testProperty "commutative for scalars" $
      \(Monom x) s ->
        fromInteger s * x == x * fromInteger s

    , testProperty "associative" $
      \(Monom x) (Monom y) (Monom z) ->
        x * (y * z) == (x * y) * z

    , testProperty "anticommutative" $ 
      \(Monom x) (Monom y) ->
        x * y == (-1)^(grade x*grade y) * (y * x)  || x ∧ y == 0

    , testProperty "blade squares to scalar" $ 
      \(Monom x) -> isScalar (x*x)

    , testProperty "U test" $ 
      \(Monom a) (Monom b) (Monom c) ->
        a * e 9 * b * e 9 * c == (-1)^(grade b) * a * b * c

    , testProperty "Z test" $ property $
      \(Monom a) (Monom b) (Monom c) ->
        a * e 0 * b * e 0 * c == 0

    , testProperty "I test" $ property $
      \(Monom a) (Monom b) (Monom c) ->
        a * e (-9) * b * e (-9) * c == - (-1)^(grade b) * a * b * c
    ]
  , testGroup "Inner and scalar products"
    [ testProperty "with scalar" $ 
      \(Monom x) n -> fromInteger n `inner` x == fromInteger n * x
          
    , testProperty "reversible" $ 
      \(Monom x) (Monom y) -> x • y == rev y • rev x

    , testProperty "left contraction" $ 
      \(Monom a) (Monom b) (Monom c) ->
        (a ∧ b) • c == a • (b -| c)

    , testProperty "right contraction" $ 
      \(Monom a) (Monom b) (Monom c) ->
        c • (b ∧ a) == (c |- b) • a

    , testProperty "left and right contraction" $ 
      \(Monom a) (Monom b) ->
        rev (a |- b) == rev b -| rev a
    ]
  ]

multivectorTests = testGroup "Miltivector tests"
  [ testGroup "Outer product"
    [ testProperty "associative" $ 
      \(Multivector x) (Multivector y) (Multivector z) ->
        x ∧ (y ∧ z) == (x ∧ y) ∧ z
    , testProperty "commutative for scalars" $ 
      \(Multivector x) s ->
        fromInteger s ∧ x == x ∧ fromInteger s
        
    , testProperty "anticommutative for vectors" $ 
      \(Vector x) (Vector y) -> x ∧ y == - (y ∧ x)
      
    , testProperty "commutative for bivectors" $ 
      \(Bivector x) (Bivector y) -> x ∧ y == y ∧ x
      
    , testProperty "commutative for vectors and bivectors" $ 
      \(Vector x) (Bivector y) -> x ∧ y == y ∧ x
      
    , testProperty "outer product via geometric for vectors" $ 
      \(Vector a) (Vector b) -> 2 * (a ∧ b) == a*b - b*a

    , testProperty "via projections" $ 
      \(Multivector x) (Multivector y) ->
        x ∧ y == sum [ getGrade (i+k) (getGrade i x * getGrade k y)
                      | i <- [0..grade x], k <- [0..grade y] ]
    ]
  , testGroup "Geometric product"
    [ testProperty "commutative for scalars" $ 
      \(Multivector x) s -> fromInteger s * x == x * fromInteger s

    , testProperty "associative" $ 
      \(Multivector x) (Multivector y) (Multivector z)
      -> x * (y * z) == (x * y) * z

    , testProperty "reverse commutative for vectors" $ 
      \(Vector x) (Vector y) -> x * y == rev (y * x)
    
    , testProperty "reverse commutative for bivectors" $ 
      \(Bivector x) (Bivector y) -> x * y == rev (y * x)

    , testProperty "reverse commutative for vectors and bivectors" $ 
      \(Vector x) (Bivector y) -> x * y == - rev (y * x)

    , testProperty "vector squares to scalar" $ 
      \(Vector x) -> isScalar (x*x)

    , testProperty "bivector squares to scalar" $ 
      \(Bivector x) -> isScalar (x*x)

    , testProperty "trivector squares to scalar" $ 
      \(Trivector x) -> isScalar (x*x)

    , testProperty "homogeneous multivector inversion" $ 
      \(Homogeneous x) -> isInvertible x ==> x / x == 1

    , testProperty "geometric product decomposition for vectors" $ 
      \(Vector a) (Vector b) -> a*b == a ∙ b + a ∧ b

    , testProperty "geometric product decomposition for vector and bivector" $ 
      \(Vector a) (Multivector b) -> a*b == a -| b + a ∧ b

    , testProperty "geometric product decomposition for bivector and vector" $ 
      \(Multivector a) (Vector b) -> a*b == a |- b + a ∧ b
    ]
  ]

testSuite = testGroup "Base"
  [ bladeTests
  , multivectorTests ]
