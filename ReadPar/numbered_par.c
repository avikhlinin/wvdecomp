#include <string.h>
#include <stdio.h>
#include "readpar.h"

int get_numbered_par (char *key, int n, char *value)
{
  char parname[1024];
  
  sprintf (parname,"%s%d",key,n);
  get_command_line_par (parname,value);
  if (defined(value)) return 1;

  sprintf (parname,"%s%2.2d",key,n);
  get_command_line_par (parname,value);
  if (defined(value)) return 1;

  sprintf (parname,"%s%3.3d",key,n);
  get_command_line_par (parname,value);
  if (defined(value)) return 1;

  sprintf (parname,"%s%4.4d",key,n);
  get_command_line_par (parname,value);
  if (defined(value)) return 1;
  
  return 0;
}
