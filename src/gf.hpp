#pragma once

#include <algorithm>
#include <cassert>
#include <condition_variable>
#include <csignal>
#include <cstdarg>
#include <cstddef>
#include <cstdint>
#include <cstdio>
#include <ctime>
#include <ctype.h>
#include <dirent.h>
#include <fcntl.h>
#include <fstream>
#include <memory>
#include <mutex>
#include <poll.h>
#include <queue>
#include <ranges>
#include <semaphore.h>
#include <sys/wait.h>
#include <spawn.h>
#include <sys/stat.h>
#include <thread>
#include <unordered_map>
#include <vector>
#include <iostream>
#include <format>


struct ControlPipe {
   static void* thread_proc(void*);
   static void  on_command(std::unique_ptr<std::string> input);
};