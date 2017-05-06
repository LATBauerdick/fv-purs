#include <cstdlib>
#include <sstream>
#include "Number.hh"

namespace Data_Number {
  auto fromStringImpl ( const any& just,
                        const any& nothing,
                        const char * a) -> any {
    std::string s = a;
    std::istringstream i(s);
    double x;
    i >> x;
    return i.fail() || !i.eof() ? nothing : just(x);
  }
}

