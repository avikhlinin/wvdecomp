#include <string.h>

int RP_chomp (char *string) 
{
  int n;
  n = strlen (string);
  if (string[n-1]=='\n') string[n-1]='\0';
  return 1;
}
