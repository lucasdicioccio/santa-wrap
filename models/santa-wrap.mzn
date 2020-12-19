% Use this editor as a MiniZinc scratch book

include "globals.mzn";

int: nShapes;
set of int: SHAPE = 1..nShapes;
int: nBlocks;
set of int: BLOCK = 1..nBlocks;

int: nDims = 2;
set of int: DIM = 1..2;

array[BLOCK,DIM] of int: rect_sizes;
array[BLOCK,DIM] of int: rect_offs;
array[SHAPE] of set of BLOCK: shapes;

int: nObjs;
set of int: OBJ = 1..nObjs;
array[OBJ,DIM] of var 0..100: coordinates;
array[OBJ] of SHAPE: object_shapes;
constraint geost(nDims, rect_sizes, rect_offs,
  shapes,
  coordinates,
  object_shapes);