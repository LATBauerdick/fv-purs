#ifndef Data_Number_FFI_HH
#define Data_Number_FFI_HH

#include "PureScript/PureScript.hh"

namespace Data_Number {
  using namespace PureScript;

  // foreign import fromStringImpl
  //   :: (forall a. a -> Maybe a)
  //   -> (forall a. Maybe a)
  //   -> String
  //   -> Maybe Double
  //
  auto fromStringImpl( const any&,
                       const any&,
                       const char *) -> any;

}

#endif // Data_Int_FFI_HH
