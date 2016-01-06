{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module OpenBLAS1 where

import qualified Language.C.Inline as C
import qualified Data.Vector.Storable as VS
import           Foreign.C.Types
import           Data.Monoid ((<>))

C.context (C.baseCtx <> C.vecCtx)

C.include "<math.h>"
C.include "<string.h>"
C.include "atest.h"
C.include "cblas.h"

    
readAndSumW :: Int -> IO (Int)
readAndSumW n = do
  let n' = fromIntegral n
  x <- [C.exp| int { readAndSum( $(int n') ) } |]
  return $ fromIntegral x


cblas_copyW :: CInt -> VS.Vector CDouble -> CInt -> VS.Vector CDouble -> CInt -> IO ()
cblas_copyW n dx incx dy incy = do
  [C.block| void
   {
     cblas_dcopy(
         $(const int n),
         $vec-ptr:(const double* dx),
         $(const int incx),
         $vec-ptr:(double* dy),
         $(const int incy)
         );
   }
   |]

  
cblas_axpyW :: CInt -> CDouble -> VS.Vector CDouble -> CInt -> VS.Vector CDouble -> CInt -> IO ()
cblas_axpyW n da dx incx dy incy = do
  [C.block| void
   {
     cblas_daxpy(
         $(const int n),
         $(const double da),
         $vec-ptr:(const double* dx),
         $(const int incx),
         $vec-ptr:(double* dy),
         $(const int incy)
         );
   }
   |]



cblas_scalW :: CInt -> CDouble -> VS.Vector CDouble -> CInt -> IO ()
cblas_scalW n da dx incx = do
  [C.block| void
   {
     cblas_dscal(
         $(const int n),
         $(const double da),
         $vec-ptr:(double* dx),
         $(const int incx)
         );
   }
   |]

  
cblas_swapW :: CInt -> VS.Vector CDouble -> CInt -> VS.Vector CDouble -> CInt -> IO ()
cblas_swapW n dx incx dy incy = do
  [C.block| void
   {
     cblas_dswap(
         $(const int n),
         $vec-ptr:(double* dx),
         $(const int incx),
         $vec-ptr:(double* dy),
         $(const int incy)
         );
   }
   |]

  
-- --
cblas_dotW :: CInt -> VS.Vector CDouble -> CInt -> VS.Vector CDouble -> CInt -> IO (CDouble)
cblas_dotW n dx incx dy incy = do
  [C.block| double
   {
     return cblas_ddot(
         $(const int n),
         $vec-ptr:(const double* dx),
         $(const int incx),
         $vec-ptr:(const double* dy),
         $(const int incy)
         );
   }
   |]



cblas_nrm2W :: CInt -> VS.Vector CDouble -> CInt -> IO (CDouble)
cblas_nrm2W n dx incx = do
  [C.block| double
   {
     return cblas_dnrm2(
         $(const int n),
         $vec-ptr:(const double* dx),
         $(const int incx)
         );
   }
   |]





cblas_asumW :: CInt -> VS.Vector CDouble -> CInt -> IO (CDouble)
cblas_asumW n dx incx = do
  [C.block| double
   {
     return cblas_dasum(
         $(const int n),
         $vec-ptr:(const double* dx),
         $(const int incx)
         );
   }
   |]




cblas_iamaxW :: CInt -> VS.Vector CDouble -> CInt -> IO (CInt)
cblas_iamaxW n dx incx = do
  [C.block| int
   {
     return cblas_idamax(
         $(const int n),
         $vec-ptr:(const double* dx),
         $(const int incx)
         );
   }
   |]



