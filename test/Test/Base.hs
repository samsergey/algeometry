module Test.Base (testSuite) where

import Test.Tasty
import Test.QuickCheck ( (==>), Positive (..) )
import Test.Tasty.QuickCheck ( testProperty )
import Algeometry.GeometricAlgebra
import Algeometry.GeometricNum 
import Algeometry.Arbitrary

bladeTests :: TestTree
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

multivectorTests :: TestTree
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

    , testProperty "reversion homomorphism" $
      \(Multivector x) (Multivector y) -> rev (x * y) == rev y * rev x
    ]
  , testGroup "Geometric product"
    [ testProperty "commutative for scalars" $ 
      \(Multivector x) s -> fromInteger s * x == x * fromInteger s

    , testProperty "left linearity" $ 
      \(Multivector x) (Multivector y) (Multivector z) a b
      -> (scale a x + scale b y) * z == scale a (x * z) + scale b (y * z)

    , testProperty "right linearity" $ 
      \(Multivector x) (Multivector y) (Multivector z) a b
      -> z * (scale a x + scale b y) == scale a (z * x) + scale b (z * y)

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

 --   , testProperty "scalar-outer relation" $ 
--      \(Homogeneous a) (Homogeneous b) -> a ∙ (lcompl b) == lcompl (a ∧ b)

    , testProperty "reversion homomorphism" $
      \(Multivector x) (Multivector y) -> rev (x ∧ y) == rev y ∧ rev x
    ]
  ]

numericTests :: TestTree
numericTests = testGroup "Numeric tests"
  [ testGroup "Exponent"
    [ testProperty "scalar values" $ 
      \x -> let x0 = scalar x :: Multivector
            in trace (exp x0) == exp (trace x)
    , testProperty "homogeneous multivectors: bulk" $ 
      \(Homogeneous x) -> let x' = bulk x
                          in exp x' == expSeries x'
    , testProperty "homogeneous multivectors: weight" $ 
      \(Homogeneous x) -> let x' = weight x
                          in exp x' == expSeries x'
    ]
  , testGroup "Log"
    [ testProperty "scalar values" $ 
      \(Positive x) -> let x0 = scalar (abs x) :: Multivector
            in trace (log x0) == log (trace x)
    , testProperty "inverse with exp" $ 
      \(Vector x) a -> let x' = scalar a + bulk x
                          in nonScalar (log (exp x')) == nonScalar x'
    ]
  ]

testSuite :: TestTree
testSuite = testGroup "Base"
  [ bladeTests
  , multivectorTests ]

scalout :: Homogeneous -> Homogeneous -> Bool
scalout (Homogeneous a) (Homogeneous b) =
  a `inner` dual b == -(-1)^(grade a `div` 2)*dual (a `outer` b)

main = defaultMain numericTests


