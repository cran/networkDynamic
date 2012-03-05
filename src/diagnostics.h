#ifndef DIAGNOSTICS_H 
#define DIAGNOSTICS_H

#include <R.h>
#include "wtedgetree.h"

/* Function prototypes */
void DurationMatrix (int *n,  int *n_edges, int *edges, 
		     int *start, int *end,
		     int *n_changes, int *changes,
		     int *dmatrix);
void AddNewDurationRow (int *dmatrix, int row, int t, int h, int time, int offset, int def_end, int left_censored);
#endif

