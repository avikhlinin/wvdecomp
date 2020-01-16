#include <stdio.h>
#ifdef HAVEREADLINE
#include <readline/readline.h>
#include <readline/history.h>
#endif

#include "readpar.h"
#include <string.h>

int get_cl_or_read_par (char *name, char *description, char *value)
{
  char *p, prompt[256];
  get_command_line_par (name,value);
  if (defined(value)) return 0;

  strcpy (prompt,name);
  strcat (prompt," (");
  strcat (prompt,description);
  strcat (prompt,"): ");

#ifdef HAVEREADLINE
  p = readline (prompt);
  strcpy (value,p);
#else
  printf ("%s",prompt);
  scanf ("%s",value);
#endif

  return 1;
}
  
