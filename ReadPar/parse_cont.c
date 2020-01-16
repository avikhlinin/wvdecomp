#include "readpar.h"
#include <string.h>

int RP_Parse_Cont_String (char *line, 
			  char *value, 
			  char *comment,
			  int  *valuecont)
{
  char *s,*sold;
  char buff[1000];
  int i;
   
  s = RP_Rem_Lead_Blanks(line);
  i=0;
  while ( *s != '\0' && *s != '\n' ) {
    if ( *s == '}' ) {
      *valuecont = *valuecont - 1;
      if ( *valuecont == 0 ) {
	s++; break;
      }
    }
    if ( *s == '{' ) *valuecont = *valuecont + 1;
    if ( *s == '\\' ) {
      if (s[1] == '{' || s[1] == '}') s++;
    }
    buff[i]=*s; i++;
    s++;
  }
  buff[i]='\0';
  strcpy(value,buff);

  if (RP_InGroup() && *s==',') s++; /* skip possible comma after the closing
                                       bracket in the data group */
  s = RP_Rem_Lead_Blanks(s);
  strcpy(comment,s); RP_chomp(comment);
  return 1;
}


