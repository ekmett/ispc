{-# Language BlockArguments, QuasiQuotes, TemplateHaskell #-}

import Codec.Picture
import Data.Bits
import Data.Bool
import Data.String.QQ
import Foreign.C.Types
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import Language.ISPC

compile [] [s|

static inline int mandel(float c_re, float c_im, int count) {
  float z_re = c_re, z_im = c_im;
  int i;
  for (i = 0; i < count; ++i) {
    if (z_re * z_re + z_im * z_im > 4.)
      break;

    float new_re = z_re*z_re - z_im*z_im;
    float new_im = 2.f * z_re * z_im;
    unmasked {
      z_re = c_re + new_re;
      z_im = c_im + new_im;
    }
  }

  return i;
}

export void mandelbrot_ispc(
  uniform float x0, uniform float y0,
  uniform float x1, uniform float y1,
  uniform int width, uniform int height,
  uniform int maxIterations,
  uniform int output[]
) {
  float dx = (x1 - x0) / width;
  float dy = (y1 - y0) / height;
  for (uniform int j = 0; j < height; j++) {
    // Note that we'll be doing programCount computations in parallel,
    // so increment i by that much.  This assumes that width evenly
    // divides programCount.
    foreach (i = 0 ... width) {
      // Figure out the position on the complex plane to compute the
      // number of iterations at.  Note that the x values are
      // different across different program instances, since its
      // initializer incorporates the value of the programIndex
      // variable.
      float x = x0 + i * dx;
      float y = y0 + j * dy;

      int index = j * width + i;
      output[index] = mandel(x, y, maxIterations);
    }
  }
}

|]

mandelbrot :: (Pixel p, Num p) => IO (Image p)
mandelbrot = do
  let (x1,y1) = (-2,-1)
      (x2,y2) = (1,1)
      (w,h) = (768,512)
      iw = fromIntegral w
      ih = fromIntegral h
      iterations = 256
      band i = bool 240 20 (testBit i 1)
  allocaArray (iw*ih) \p -> do
    $(call "mandelbrot_ispc" [t| Float -> Float -> Float -> Float -> CInt -> CInt -> CInt -> Ptr CInt -> IO () |])
      x1 y1 x2 y2 w h iterations p
    withImage iw ih \ i j -> band <$> peekElemOff p (i + j * iw)
  
main :: IO ()
main = do
  img <- mandelbrot
  saveJpgImage 100 "mandelbrot.jpg" $ ImageY8 img
