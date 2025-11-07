// Simple test file for clangd tests
// Do not modify line numbers!
#include <string>

struct MyStruct {
    std::string name;
    int value;
};

void target_function() {
    // Definition at line 9 (0-indexed)
}

void caller() {
    MyStruct obj;
    obj.name = "test";
    target_function();  // Call at line 16 (0-indexed: 15)
}
