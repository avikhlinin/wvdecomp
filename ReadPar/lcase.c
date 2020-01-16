#include <ctype.h>
#include <string.h>

int lcase (char *string)
{
  register int i,n;
  n = strlen (string);
  for (i=0;i<n;i++)
    string[i]=tolower(string[i]);
  return 0;
}
