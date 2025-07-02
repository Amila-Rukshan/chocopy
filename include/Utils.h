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

} // namespace chocopy

#endif // CHOCOPY_UTILS_H
