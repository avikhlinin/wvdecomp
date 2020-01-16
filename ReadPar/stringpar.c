#include <string.h>
#include "readpar.h"

int RP_LAST_STRING_ARG;
int RP_LAST_STRING_VAL;

char *RP_WORDS[8192];
int RP_NWORDS;

int get_string_par (char *string, char *parname, char *parvalue)
{
  int f;
  char buffer[8192];
  strcpy (buffer, string);
  
  RP_NWORDS = 0;
  RP_WORDS[RP_NWORDS] = strtok (buffer," \t\n");
  while ( RP_WORDS[RP_NWORDS] != NULL ) {
    RP_NWORDS++;
    RP_WORDS[RP_NWORDS] = strtok (NULL," \t\n");
  }

  f = get_string_par_work (parname,parvalue);
  return f;
}

#define get_command_line_par_work get_string_par_work
#define RP_LAST_ARG RP_LAST_STRING_ARG
#define RP_LAST_VAL RP_LAST_STRING_VAL
#define getarg string_getarg
#define iargc string_iargc
#include "commandline_code.h"

int last_string_par_position ()
{
  return RP_LAST_ARG;
}

int last_string_parameter_val_position ()
{
  return RP_LAST_VAL;
}

int string_iargc (){
  return RP_NWORDS;
}

int string_getarg (int i, char *arg) {
  arg = RP_WORDS[i-1];
  return 1;
}
