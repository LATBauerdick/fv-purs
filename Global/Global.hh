#ifndef Global_FFI_HH
#define Global_FFI_HH

#include "PureScript/PureScript.hh"
#include <cmath>

namespace Global {
  using namespace PureScript;

  const double nan = NAN;
  const double infinity = INFINITY;
  auto isNaN (const double) -> bool;
  auto isFinite (const double) -> bool;
  auto readInt (const int, const string& ) -> double;
  auto readFloat (const char *) -> double;
}

#endif // Global_FFI_HH

