#include "readpar.h"
#include <stdio.h>
#include <string.h>

int RP_Parse_Group_String (char *line, 
			   char *name, 
			   char *comment)
{
  char *s,*sold;
  char buff[1000];
  int i;
  
  s = line + 6; /* Skip "@Data{"*/
  sscanf (s,"%s",name);
  i = strlen(name);

  sold = s+i;
  s = RP_Rem_Lead_Blanks(s);
  strcpy(comment,s);
  
  if (name[i-1]==',') name[i-1]='\0'; /* Strip trailing comma from name*/
  return 1;
}
