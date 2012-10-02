#include <SDL.h>
#include <SDL_gfxPrimitives.h>
#include <stdlib.h>

////////////////////////////////////////////////////////////

struct Point
{
  int x, y;
};
typedef struct Point Point;

struct EightQueen
{
  SDL_Surface* screen;
  int size, nlist, index;
  Point* list;
};
typedef struct EightQueen EightQueen;

////////////////////////////////////////////////////////////

#define WINDOW_WIDTH 640
#define WINDOW_HEIGHT 480
#define BOARD_SIZE 400
#define METER_WIDTH 30
#define METER_HEIGHT BOARD_SIZE
#define POINTER_WIDTH 8

////////////////////////////////////////////////////////////

static int eight_queen_VideoInit(EightQueen*);
static int eight_queen_ModelInit(EightQueen*, FILE*);
static void eight_queen_DrawPattern(EightQueen*);
static void eight_queen_DrawBoard(EightQueen*, Uint32);
static void eight_queen_DrawQueen(EightQueen*, const Point*, Uint32);
static void eight_queen_Increment(EightQueen*);
static void eight_queen_Decrement(EightQueen*);

int eight_queen_VideoInit(EightQueen* eq)
{
  SDL_Surface* screen;

  if (SDL_Init(SDL_INIT_VIDEO) != 0)
    return -1;

  screen = SDL_SetVideoMode(WINDOW_WIDTH, WINDOW_HEIGHT,
                            32, SDL_SWSURFACE|SDL_ANYFORMAT);
  if (!screen)
    return -1;

  SDL_WM_SetCaption("n-queen visualiser", "");

  eq->screen = screen;

  return 0;
}

int eight_queen_ModelInit(EightQueen* eq, FILE* input)
{
  int i, j;

  if (fscanf(input, "%d%d", &eq->size, &eq->nlist) != 2)
    return -1;

  if (eq->size < 1 || eq->nlist < 1)
    return -1;

  eq->list = (Point*)malloc(sizeof(Point)*eq->size*eq->nlist);
  if (!eq->list)
    return -1;

  for (i = 0; i < eq->nlist; i++) {
    Point* p = eq->list+eq->size*i;
    for (j = 0; j < eq->size; j++) {
      if (fscanf(input, "%d", &p[j].y) != 1)
        return -1;
      p[j].x = j+1;
    }
  }

  eq->index = 0;

  return 0;
}

#define OFFSET_METER (2*METER_WIDTH)
#define OFFSET_X ((WINDOW_WIDTH-BOARD_SIZE-OFFSET_METER)/2+OFFSET_METER)
#define OFFSET_Y ((WINDOW_HEIGHT-BOARD_SIZE)/2)
#define LINE(n, i) (BOARD_SIZE*(i)/(n))

#define OFFSET_METER_X (OFFSET_X-OFFSET_METER)
#define OFFSET_METER_Y OFFSET_Y

void eight_queen_DrawBoard(EightQueen* eq, Uint32 color)
{
  int i;

  rectangleColor(eq->screen,
                 OFFSET_X, OFFSET_Y,
                 OFFSET_X+BOARD_SIZE, OFFSET_Y+BOARD_SIZE, color);

  for (i = 1; i < eq->size; i++) {
    hlineColor(eq->screen, OFFSET_X, OFFSET_X+BOARD_SIZE, OFFSET_Y+LINE(eq->size, i), color);
    vlineColor(eq->screen, OFFSET_X+LINE(eq->size, i), OFFSET_Y, OFFSET_Y+BOARD_SIZE, color);
  }

  for (i = 0; i < eq->nlist; i++) {
    hlineColor(eq->screen, OFFSET_METER_X, OFFSET_METER_X+METER_WIDTH,
               OFFSET_METER_Y+METER_HEIGHT/2/eq->nlist+METER_HEIGHT*i/eq->nlist, color);
  }
}

void eight_queen_DrawQueen(EightQueen* eq, const Point* p, Uint32 color)
{
  lineColor(eq->screen,
            OFFSET_X+LINE(eq->size, p->x-1),
            OFFSET_Y+LINE(eq->size, p->y-1),
            OFFSET_X+LINE(eq->size, p->x),
            OFFSET_Y+LINE(eq->size, p->y),
            color);
  lineColor(eq->screen,
            OFFSET_X+LINE(eq->size, p->x),
            OFFSET_Y+LINE(eq->size, p->y-1),
            OFFSET_X+LINE(eq->size, p->x-1),
            OFFSET_Y+LINE(eq->size, p->y),
            color);
}

void eight_queen_DrawPattern(EightQueen* eq)
{
  Uint32 bg, line;
  int i;
  Point* p;

  bg = SDL_MapRGB(eq->screen->format, 0xff, 0xff, 0xff);
  line = 0x606060ff;

  SDL_FillRect(eq->screen, NULL, bg);
  eight_queen_DrawBoard(eq, line);

  p = eq->list+eq->size*eq->index;
  for (i = 0; i < eq->size; i++) {
    eight_queen_DrawQueen(eq, p+i, line);
  }

  rectangleColor(eq->screen, OFFSET_METER_X+(METER_WIDTH-POINTER_WIDTH)/2,
                 OFFSET_METER_Y+METER_HEIGHT*(eq->index)/eq->nlist,
                 OFFSET_METER_X+(METER_WIDTH-POINTER_WIDTH)/2+POINTER_WIDTH,
                 OFFSET_METER_Y+METER_HEIGHT*(eq->index+1)/eq->nlist, line);

  SDL_Flip(eq->screen);
}

void eight_queen_Increment(EightQueen* eq)
{
  if (eq->index+1 < eq->nlist) eq->index++;
}

void eight_queen_Decrement(EightQueen* eq)
{
  if (eq->index != 0) eq->index--;
}

void eight_queen_Free(EightQueen* eq)
{
  if (eq->list)
    free(eq->list);

  if (eq->screen)
    SDL_Quit();

  free(eq);
}

EightQueen* eight_queen_Initialize(void)
{
  EightQueen* eq;

  eq = (EightQueen*)malloc(sizeof(EightQueen));
  eq->screen = 0;
  eq->size = eq->nlist = eq->index = 0;
  eq->list = 0;

  if (eight_queen_ModelInit(eq, stdin) == 0 &&
      eight_queen_VideoInit(eq) == 0) {
    // do nothing
  } else {
    eight_queen_Free(eq);
    eq = NULL;
  }

  return eq;
}

void eight_queen_MainLoop(EightQueen* eq)
{
  SDL_Event event;

  eight_queen_DrawPattern(eq);

  while (SDL_WaitEvent(&event)) {
    switch (event.type) {
    case SDL_KEYDOWN:
      switch (event.key.keysym.sym) {
      case SDLK_q:
      case SDLK_ESCAPE:
        return;

      case SDLK_n:
      case SDLK_RETURN:
        eight_queen_Increment(eq);
        eight_queen_DrawPattern(eq);
        break;

      case SDLK_p:
      case SDLK_BACKSPACE:
        eight_queen_Decrement(eq);
        eight_queen_DrawPattern(eq);
        break;
      }
      break;

    case SDL_QUIT:
      return;
    }
  }
}
