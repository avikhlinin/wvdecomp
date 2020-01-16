#include <string.h>
#include "readpar.h"

int defined (char *string)
{
  if (strcmp(string,RP_UNDEFINED)==0)
    return 0;
  else
    return 1;
}

int RP_defined (char *string) {
  return defined (string);
}


int yespar (char *name)
{
  char value[200];
  get_command_line_par (name,value);
  lcase (value);
  return ( 
	  strcmp(value,"y")==0   ||
	  strcmp(value,"yes")==0 ||
	  strcmp(value,"да")==0 );
}

int nopar (char *name)
{
  char value[200];
  get_command_line_par (name,value);
  lcase (value);
  return ( 
	  strcmp(value,"n")==0   ||
	  strcmp(value,"no")==0 ||
	  strcmp(value,"нет")==0 );
}

int defpar (char *name)
{
  char value[200];
  get_command_line_par (name,value);
  return defined (value);
}

