module ComplexNumbers
  ( Complex
  , conjugate
  , abs
  , exp
  , real
  , imaginary
  , mul
  , add
  , sub
  , div
  , complex
  ) where

import           Prelude hiding (abs, div, exp)
import qualified Prelude (exp)

-- Data definition -------------------------------------------------------------
type Complex a = (a, a)

complex :: (a, a) -> Complex a
complex (r, i) = (r, i)

-- unary operators -------------------------------------------------------------
conjugate :: Num a => Complex a -> Complex a
conjugate (r, i) = (r, -i)

abs :: Floating a => Complex a -> a
abs (r, i) = sqrt (r * r + i * i)

real :: Num a => Complex a -> a
real (r, _) = r

imaginary :: Num a => Complex a -> a
imaginary (_, i) = i

exp :: Floating a => Complex a -> Complex a
exp (r, i) = (Prelude.exp r * cos i, Prelude.exp r * sin i)

-- binary operators ------------------------------------------------------------
mul :: Num a => Complex a -> Complex a -> Complex a
mul (r1, i1) (r2, i2) = (r1 * r2 - i1 * i2, i1 * r2 + r1 * i2)

add :: Num a => Complex a -> Complex a -> Complex a
add (r1, i1) (r2, i2) = (r1 + r2, i1 + i2)

sub :: Num a => Complex a -> Complex a -> Complex a
sub (r1, i1) (r2, i2) = (r1 - r2, i1 - i2)

div :: Fractional a => Complex a -> Complex a -> Complex a
div (r1, i1) (r2, i2) = ((r1 * r2 + i1 * i2) / (r2 * r2 + i2 * i2), (i1 * r2 - r1 * i2) / (r2 * r2 + i2 * i2))
