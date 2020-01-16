#include <string.h>
#include "readpar.h"
#include <stdlib.h>

int RP_LAST_ARG;
int RP_LAST_VAL;

static char *defname[1024], *defval[1024];
static int ndefpar=0;
static char RP_EXEC_NAME[1024] = "";

int get_command_line_par (char *parname, char *parvalue)
{
  int f;
  char *pfile,*s;
  char pname[1024], work[1024];
  int i, qcase;
  int qexpand;

  qexpand = RP_q_expand_vars();

  f = get_command_line_par_work (parname,parvalue);
  if (!defined(parvalue)) {
    pfile = getenv(RP_PAR_FILE_ENV);
    if (pfile != NULL) {
      f = get_command_line_par_work ("rp_exec_name",work);
      if (!defined(work)) {
	if (RP_EXEC_NAME[0]!='\0') {
	  strcpy (work,RP_EXEC_NAME);
	} else {
	  getarg (0,work);
	}
      }
      s=strrchr(work,'/');
      if (s==NULL) {
	s = work;
      }
      else {
	s++;
      }
      strcpy (pname,s);
      strcat (pname,".");
      strcat (pname,parname);
      f = get_parameter_file_par (pfile,pname,parvalue);
      if (defined(parvalue)) {
	RP_LAST_ARG=0;
	RP_LAST_VAL=0;
      } 
    }
  }

  if (!defined(parvalue)) {
    if (parname[0]=='^') {  qcase = 1; strcpy (pname,parname+1); }
    else { qcase = 0; strcpy (pname,parname); lcase(pname); }
    for (i=ndefpar-1;i>=0; i--) { /* go backwards so that last changes are most
				   important */
      if (qcase) {
	strcpy (work,defname[i]);
	lcase(work);
	s = work;
      } else {
	s = defname[i];
      }
      if (strcmp(pname,s)==0) {
	strcpy (parvalue,defval[i]);
	RP_LAST_ARG=0;
	RP_LAST_VAL=0;
	break;
      }
    }
  }

  if (qexpand)
    if (defined(parvalue)) {
      RP_expand_variables(parvalue);
    }
  return f;
}

#include "commandline_code.h"

int last_command_line_par_position ()
{
  return RP_LAST_ARG;
}

int last_parameter_val_position ()
{
  return RP_LAST_VAL;
}

int set_default_par_value (char *name, char *value)
{
  int n;

  if (strcmp(name,"rp_exec_name")==0) {
    strcpy(RP_EXEC_NAME,value);
    return 0;
  }

  defname[ndefpar]=(char *)malloc(sizeof(char)*(strlen(name)+1));
  if (defname[ndefpar]==NULL) {
    perror ("cannot allocate memory for default par name");
    exit(1);
  }
  
  defval[ndefpar]=(char *)malloc(sizeof(char)*(strlen(value)+1));
  if (defval[ndefpar]==NULL) {
    perror ("cannot allocate memory for default par value");
    exit(1);
  }
  
  strcpy (defname[ndefpar],name);
  strcpy (defval[ndefpar],value);
  ndefpar++;

  return 0;
}

void set_default_par_value_ (char *name, char *value)
{
  set_default_par_value (name,value);
}
