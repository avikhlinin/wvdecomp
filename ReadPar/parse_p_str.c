#include "readpar.h"
#include <string.h>
#include <ctype.h>

int RP_Parse_Par_String (char *line, 
			 char *name, 
			 char *value, 
			 char *comment,
			 int  *valuecont)
{
  char *s,*sold;
  char buff[1000]; int i;
  
  s = RP_Rem_Lead_Blanks(line);

  /* find name */
  sold = s;
  while (isalnum(*s) || *s == '_' || *s == '.') s++;
  strncpy(name,sold,s-sold); name[s-sold]='\0';
  

  s = RP_Rem_Lead_Blanks(s); /* *s must now be the equal sign */
  s++;
  s = RP_Rem_Lead_Blanks(s);

  if ( *s != '{' ) *valuecont = 0;

  if ( *s == 0 ) { /* parameter is set, but its value not defined */
    strcpy(value,"");
    strcpy(comment,"");
    return 1;
  }
  
  sold = s;
  if ( *s != '\'' && *s != '{' ) { /* simple, one-word value */
    while (*s&&(!isspace(*s))) s++;
    strncpy(value,sold,s-sold); value[s-sold]='\0';
    /* if we are in the data group, simple values may be followed by a comma;
     strip this comma */
    if ( RP_InGroup() ) {
      if (value[s-sold-1] == ',') value[s-sold-1] = '\0';
    }
    s = RP_Rem_Lead_Blanks(s);
    strcpy(comment,s); RP_chomp(comment);
    return 1;
  }

  if ( *s == '\'') {/* Parameter value should be terminated by a 
		       single quote. However, the value can contain quotes 
		       that are either escaped or doubled */

    s++;
    i = 0;
    while ( *s != '\0' && *s != '\n' ) {
      if ( *s == '\'') {
	if (s[1] != '\'') {
	  s++; break;  /* on normal exit, *s will be past the closing quote */
	}
	else s++;
      }
      if ( *s == '\\') {
	if (s[1] == '\'') s++;
      }
      
      buff[i]=*s; i++;
      s++;
    }
    buff[i]='\0';
    strcpy(value,buff);

    if (RP_InGroup() && *s==',') s++; /* skip possible comma after the closing
				       quote in the data group */
    s = RP_Rem_Lead_Blanks(s);
    strcpy(comment,s); RP_chomp(comment);
    return 1;
  }

  if ( *s == '{' ) { /* Parameter value should be terminated by the closing
			bracket; continuations on the next line are possible;
			valuecont > 0 indicates how many brackets need to be 
			closed on the continuation lines;
		     */
    s++;
    *valuecont = 1;
    i = 0;
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
  return 1;
}

  
