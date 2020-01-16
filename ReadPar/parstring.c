#include <string.h>
#include <ctype.h>

int RP_ParString (char *string)
{
  /* letter 
     followed by any number of alphanumeric chars 
     followed by zero or more spaces
     followed by an equal sign
     is a parameter string */
  char *s;

  s = string;
  while (isalnum(*s) || *s == '_' || *s == '.') s++;
  if ( ! ( *s == '=' || *s == ' ' || *s == '\t') ) return 0;
  
  if ( *s == '=' ) 
    return 1;

  while ( *s == ' ' || *s == '\t' ) s++;
  
  return ( *s == '=' );
}
  
