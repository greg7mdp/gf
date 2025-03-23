#include <re/re.hpp>

#include <algorithm>

#include <sstream>
#include <iostream>

#include <ctre.hpp>
using namespace ctre::literals;

template <ctll::fixed_string Pattern>
auto replace(std::string_view subject, std::string_view replacement) -> std::string {
    std::ostringstream os;

    auto rng = ctre::split<Pattern>(subject);

    auto it = rng.begin();
    const auto end = rng.end();

    assert(it != end);
    os << *it++;

    while (it != end) {
        os << replacement;
        os << *it++;
    }

    return std::move(os).str();
}


}