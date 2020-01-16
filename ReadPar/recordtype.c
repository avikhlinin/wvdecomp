#include "readpar.h"
#include <string.h>
#include <ctype.h>

int RP___InGroup;
int RP___PAR_Continued;

int RP_Parse_at_Line (char *line);

int RP_RecordType (char *line)
{
  char c;
  char *s;

  if (RP_PAR_Continued()) return RP_PARCONT;

  
  s = line;
  c = *s;

  if ( c == '@' ) {
    if (! RP_InGroup()) return RP_Parse_at_Line (line);
    else              return RP_COMMENT;
  }

  if ( RP_InGroup() )
    s = RP_Rem_Lead_Blanks (s);
  c = *s;
  if ( RP_InGroup() )
    if ( c == '}' ) return RP_ENDGROUP;
  
  if (!isalpha(c)) return RP_COMMENT;
  if (RP_ParString (s)) return RP_PARSTRING;
  if (RP_IRAFString (s)) return RP_IRAFSTRING;
  return RP_COMMENT;
}


/* ------------------------------------------------------------ */
int RP_Parse_at_Line (char *line) {

  if (strncmp(line,"@include{",9)==0)
    return RP_INCLSTRING;
  
  if (strncmp(line,"@Data{",6)==0)
    return RP_BEGGROUP;
  
  return RP_COMMENT;
}

    
int RP_InGroup() {  return RP___InGroup; }
int RP_PAR_Continued() {return RP___PAR_Continued;}
int RP_IsInGroup (int yes) {RP___InGroup = yes; return 1;}
int RP_IsPAR_Continued (int yes) {RP___PAR_Continued = yes; return 1;}


