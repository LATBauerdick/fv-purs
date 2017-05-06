#include "Global.hh"
#include <cmath>
#include <stdlib.h>
#include <bitset>
#include <sstream>
#include <iostream>

namespace Global {

  auto isNaN (const double x) -> bool {
    return x != x;
  }
  auto isFinite (const double x) -> bool {
    return isfinite (x);
  }
  auto readInt (const int radix, const string& s) -> double {
    double i;
    std::stringstream ss;
    switch (radix) {
      case 2:
        try {
          const auto n = std::bitset<32>(s).to_ulong();
          return n <= std::numeric_limits<int>::max() ? static_cast<int>(n) * 1.0 : 0.0;
        } catch (std::exception&) {
          return 0.0;
        }
        break;
      case 8:
        ss << std::oct << s;
        break;
      case 10:
        ss << std::dec << s;
        break;
      case 16:
        ss << std::hex << s;
        break;
      default:
        std::cerr << "fromStringAsImpl radix " << radix << " not supported" << std::endl;
        return 0.0;
        break;
    }
    ss >> i;
    return i;
  }

  auto readFloat (const char * a) -> double {
    return atof (a);
  }
}

