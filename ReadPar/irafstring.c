#include <ctype.h>
#include <string.h>
#include "readpar.h"

int RP_Parse_IRAF_String (char *line, char *name, char *value, char *comment)
{
  char *s, *sold;
  char searchchar;
  int stype;

  s = RP_Rem_Lead_Blanks(line);
  sold = s;

  s = strchr(sold,','); /*first coma delimits par name*/
  strncpy(name,sold,s-sold); name[s-sold]='\0';

  s++; sold=s;
  if (strncmp(s,"s,",2)==0) 
    stype = 1;  /* determine if the parameter value will be of string type */
  else         
    stype = 0;
  
  s = strchr(sold,','); s++; sold=s; /*second coma delimits par type*/
  s = strchr(sold,','); s++; sold=s; /*third  coma delimits par mode*/

  s = RP_Rem_Lead_Blanks(sold);
  sold = s;
  if (*s=='"') { /* par value is opened by a quote */
    sold = s+1;
    searchchar = '"';
  }
  else {
    sold = s;
    searchchar=',';
  }
  s = strchr (sold,searchchar);
  if (s==NULL) {
    *value='\0';
    *comment='\0';
    return 1;
  }
  strncpy(value,sold,s-sold); value[s-sold]='\0';
  *comment='\0';
  return 1;
}


int   RP_IRAFString (char *string)
{
  /* test:
     parname,datatype,partype,....
  */
  char *s;
  int stype;
  
  s = string;
  while (isalpha(*s)) s++; /* parameter name is made of letters*/
  if (*s != ',') return 0; s++; /* name should be followed by a comma */
  
  if ( !islower(*s) ) return 0; /* data type is low-case*/
  if (strncmp(s,"s,",2)==0) 
    stype = 1;  /* determine if the parameter value will be of string type */
  else         
    stype = 0;
  
  while (islower(*s)) s++;
  if (*s != ',') return 0; s++;
  
  if ( !islower(*s) ) return 0; /* parameter type is low-case*/
  while (islower(*s)) s++;
  if (*s != ',') return 0;

  return 1;
}

