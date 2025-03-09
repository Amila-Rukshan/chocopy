#include <memory>

namespace chocopy {

struct Location {
  std::shared_ptr<std::string> file;
  int line;
  int col;
};

} // namespace chocopy
