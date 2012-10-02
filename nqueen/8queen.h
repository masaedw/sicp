#ifndef eight_queen_visualiser_
#define eight_queen_visualiser_

struct EightQueen;
typedef struct EightQueen EightQueen;

EightQueen* eight_queen_Initialize(void);
void eight_queen_MainLoop(EightQueen*);
void eight_queen_Finalize(EightQueen*);

#endif
