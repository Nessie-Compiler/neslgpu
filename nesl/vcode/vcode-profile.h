#ifndef _VCODE_PROFILE_H_
#define _VCODE_PROFILE_H_

#ifndef VCODE_PROFILE
#  error expected VCODE_PROFILE to be defined
#endif

typedef struct {
    double	start;		/* time at which this call started */
    double	child;		/* time spent in children calls */
    int		func;		/* fucntion index */
} TimeStackItem_t;

typedef struct {
    double	time;		/* time not including children */
    double	minTime;	/* min time not including children */
    double	maxTime;	/* max time not including children */
    double	cumTime;	/* time including children */
    int		nCalls;		/* number of calls */
    const char	*name;		/* the function name */
} FuncTiming_t;

extern TimeStackItem_t *TimeStack;
extern int		TSP;
extern int		TimeStackDepth;
extern FuncTiming_t	*FuncProfile;
extern int		NumFuncs;

void ProfileVCodeInit ();

void ProfileVCodeGrowStack ();

void ProfileVCodeReport (const char *srcFile);

// callback to record function info in FuncProfile table */
void ProfileVCodeRecordFunc (struct hash_entry *entry);

#endif /* !_VCODE_PROFILE_H_ */
