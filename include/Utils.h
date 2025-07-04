#ifndef CHOCOPY_UTILS_H
#define CHOCOPY_UTILS_H

#include "string"

namespace chocopy {
inline bool isListType(const std::string& type) {
  return type.find('[') != std::string::npos &&
         type.find(']') != std::string::npos;
}

inline bool isPrimitiveType(const std::string& type) {
  return type == "str" || type == "bool" || type == "int";
}

inline std::string getInnerType(const std::string& type) {
  size_t first = type.find('[');
  size_t last = type.rfind(']');
  if (first == std::string::npos || last == std::string::npos || last <= first)
    throw std::invalid_argument("Invalid list type format");
  return type.substr(0, first) +
         type.substr(first + 1, type.size() - first - 2);
}

} // namespace chocopy

#endif // CHOCOPY_UTILS_H
