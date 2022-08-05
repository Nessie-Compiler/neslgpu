#ifdef VCODE_PROFILE

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "config.h"
#include "program.h"
#include "symbol_table.h"
#include "vcode-profile.h"

extern void hash_table_record ();

TimeStackItem_t *TimeStack;
int		TSP;
int		TimeStackDepth;
FuncTiming_t	*FuncProfile;
int		NumFuncs;

void ProfileVCodeInit ()
{
    int i;
    FuncProfile = (FuncTiming_t *)malloc(NumFuncs * sizeof(FuncTiming_t));

    for (i = 0;  i < NumFuncs;  i++) {
	FuncProfile[i].time	= 0.0;
	FuncProfile[i].minTime	= 1.0e9;
	FuncProfile[i].maxTime	= 0.0;
	FuncProfile[i].cumTime	= 0.0;
	FuncProfile[i].name	= "<unknown>";
	FuncProfile[i].nCalls	= 0;
    }

  /* initialize the fucntion names from the symbol table */
    hash_table_record ();

  /* initialize the timing stack; note that TimeStack[0] is not used except to record
   * the child value for the call to MAIN.
   */
    TSP = 0;
    TimeStackDepth = 4096;
    TimeStack = (TimeStackItem_t *) malloc(TimeStackDepth * sizeof(TimeStackItem_t));
}

// grow the timing stack
void ProfileVCodeGrowStack ()
{
    int newDepth = 2 * TimeStackDepth;
    TimeStackItem_t *newStk = (TimeStackItem_t *) malloc(TimeStackDepth * sizeof(TimeStackItem_t));
    memcpy (newStk, TimeStack, TimeStackDepth * sizeof(TimeStackItem_t));
    free (TimeStack);
    TimeStackDepth = newDepth;
    TimeStack = newStk;
}
    
// callback to record function info in FuncProfile table */
void ProfileVCodeRecordFunc (struct hash_entry *entry)
{
    int func = program[entry->prog_num].func;
    if ((0 <= func) || (func < NumFuncs))
	FuncProfile[func].name = entry->fn_name;
}

/* comparison function that orders in decreasing exclusive execution time */
static int CMP (const void *a, const void *b)
{
    FuncTiming_t *p = &(FuncProfile[*(int *)a]);
    FuncTiming_t *q = &(FuncProfile[*(int *)b]);
    if ((p->nCalls > 0) && (q->nCalls > 0)) {
	if (p->time > q->time) return -1;
	else if (p->time < q->time) return 1;
	else return strcmp(p->name, q->name);
    }
    else if (p->nCalls > 0) return -1;
    else if (q->nCalls > 0) return 1;
    else return strcmp(p->name, q->name);
}

void ProfileVCodeReport (const char *srcFile)
{
    char *outFileName, *dotPos;
    size_t outFileLen;
    FILE *outFile;

  /* first we create an index map and sort the entries by decreasing exclusive execution time */
    int *order = (int *)malloc(NumFuncs * sizeof(int));
    int i;
    for (i = 0;  i < NumFuncs;  i++)
	order[i] = i;
    qsort (order, NumFuncs, sizeof(int), CMP);

  /* filename for output */
    outFileLen = strlen(srcFile) + strlen(".vprof") + 1;
    outFileName = (char *) malloc(outFileLen);
    strcpy(outFileName, srcFile);
    dotPos = strrchr (outFileName, '.');
    if ((dotPos != 0)
    && ((strcmp(dotPos, ".vcode") == 0) || (strcmp(dotPos, ".ucode") == 0) || (strcmp(dotPos, ".fcode") == 0))) {
	*dotPos = '\0';
    }
    strcat(outFileName, ".vprof");

  /* print the report */
    outFile = fopen(outFileName, "w");
    if (outFile == NULL) {
	fprintf(stderr, "unable to open \"%s\"\n", outFileName);
	free (order);
	return;
    }
    fprintf(stderr, "dumping profile information to %s\n", outFileName);
    fprintf(outFile, "# Calls     Time         Min.        Avg.        Max.    Cumulative   Name\n");
    fprintf(outFile, "-------  ----------  ----------  ----------  ----------  ----------   ---------------------\n");
    for (i = 0; i < NumFuncs;  i++) {
	FuncTiming_t *p = &(FuncProfile[order[i]]);
	if (p->nCalls > 0) {
	    fprintf(outFile,
		" %6d  %10.6f  %10.6f  %10.6f  %10.6f  %10.6f   %s\n",
		p->nCalls,
		p->time,
		p->minTime,
		p->time / (double)p->nCalls,
		p->maxTime,
		p->cumTime,
		p->name);
	}
	else {
	    fprintf(outFile,
		"      0     ----        ----        ----        ----        ----      %s\n",
		p->name);
	}
    }

    free (order);
    free (outFileName);
    return;
}

#endif /* VCODE_PROFILE */
