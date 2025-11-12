#include "gf.hpp"

// --------------------------------------------------------------------------------
// define global variables here so gf.cpp can be dynamically reloaded with jet-live
//           -- all are singletons --
// --------------------------------------------------------------------------------
GF_Config     gfc;
Context       ctx;
BreakpointMgr s_breakpoint_mgr;

// --------------------------------------------------------------------------------
int main(int argc, char** argv) {
   auto ui_ptr = ctx.gf_main(argc, argv);
   if (!ui_ptr)
      return 1;

   ui_ptr->message_loop();

   ctx.kill_gdb();
   ctx.save_user_info();

   return 0;
}