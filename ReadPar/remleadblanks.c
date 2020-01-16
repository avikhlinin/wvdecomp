#include <stdlib.h>

char *RP_Rem_Lead_Blanks (char *line)
{
  char *s;
  s=line;
  while (*s==' ' || *s=='\t' || *s == '\n' ) s++;
  return s;
}
