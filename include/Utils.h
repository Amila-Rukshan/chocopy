#ifndef CHOCOPY_UTILS_H
#define CHOCOPY_UTILS_H

#include "string"

namespace chocopy {
inline bool isListType(const std::string& type) {
  return type.find('[') != std::string::npos &&
         type.find(']') != std::string::npos;
}
} // namespace chocopy

#endif // CHOCOPY_UTILS_H
