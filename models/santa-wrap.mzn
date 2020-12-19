% Use this editor as a MiniZinc scratch book

include "globals.mzn";

int: nShapes;
set of int: SHAPE = 1..nShapes;
int: nBlocks;
set of int: BLOCK = 1..nBlocks;

int: nDims = 2;
set of int: DIM = 1..2;
int: x_max;
int: y_max;

array[BLOCK,DIM] of int: rect_sizes;
array[BLOCK,DIM] of int: rect_offs;
array[SHAPE] of set of BLOCK: shapes;

int: nObjs;
set of int: OBJ = 1..nObjs;
array[OBJ,DIM] of var int: coordinates;
array[OBJ] of SHAPE: object_shapes;
constraint geost_bb(nDims, rect_sizes, rect_offs,
  shapes,
  coordinates,
  object_shapes,
  [0,0],
  [x_max, y_max]
  );