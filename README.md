# blash
Haskell implementation of the BLAS interface.

The main library is pure haskell implementation of the BLAS interface.

There are no FFI bindings in the main library - all FFI bindings are kept in the test sections.

The test library uses monadic quickcheck and inline-c to check that 
this haskell implementation reproduces the OpenBLAS BLAS implementation 
