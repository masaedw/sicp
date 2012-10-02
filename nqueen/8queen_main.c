#include <stdio.h>
#include "8queen.h"

int main(int argc, char** argv)
{
  EightQueen* eq = eight_queen_Initialize();

  if (!eq) {
    fprintf(stderr, "initialize failed.\n");
    return 1;
  }

  eight_queen_MainLoop(eq);
  eight_queen_Free(eq);

  return 0;
}
