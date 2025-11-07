#pragma once

#include "luigi.hpp"
#include <iostream>
#include <string>
#include <unistd.h>
#include <format>
#include <climits>
#include <cstring>

// ---------------------------------------------------------------------------
static inline bool is_executable_in_path(const std::string& path) {
   if (path.find('/') != std::string::npos) {
      // It's a path, check directly
      return access(path.c_str(), X_OK) == 0;
   }

   // It's a command name, search PATH
   const char* path_env = getenv("PATH");
   if (!path_env)
      return false;

   char        buffer[PATH_MAX];
   const char* start = path_env;
   const char* end;

   while ((end = strchr(start, ':')) != nullptr) {
      size_t dir_len = end - start;
      if (dir_len == 0) {
         start = end + 1;
         continue;
      }

      auto result = std::format_to_n(buffer, PATH_MAX - 1, "{}/{}", std::string_view(start, dir_len), path);
      buffer[result.size] = '\0';

      if (access(buffer, X_OK) == 0)
         return true;

      start = end + 1;
   }

   // Check last directory
   if (*start != '\0') {
      auto result = std::format_to_n(buffer, PATH_MAX - 1, "{}/{}", start, path);
      *result.out = '\0';
      return access(buffer, X_OK) == 0;
   }

   return false;
}
