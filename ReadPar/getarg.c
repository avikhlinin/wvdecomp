#include <string.h>
#include <stdlib.h>
#include <stdio.h>

static char **RPargv;
static int RPargc = 0;

int Ini_ReadPar (int argc, char **argv)
{
  RPargc=argc;
  RPargv=argv;
  return 0;
}

int IniReadPar (int argc, char **argv)
{
  return Ini_ReadPar(argc,argv);
}

int getarg (int iarg, char *arg)
{
  if (iarg>=0 && iarg<RPargc) {
    strcpy(arg,RPargv[iarg]);
    return 0;
  }
  else {
    *arg='\0';
    return 1;
  }
}

int iargc()
{
  return RPargc;
}

int AllocRPargv (int npar)
{
  RPargc = npar+1;

  /* 2) Allocate memory for RPargv; */
  RPargv = (char **)malloc(sizeof(char **)*(RPargc+1));
  if (RPargv==NULL) {
    perror("malloc0");
    exit(1);
  }
  return 0;
}

int SetRPargv (int ipar, char *value)
{
  RPargv[ipar] = (char *)malloc(sizeof(char)*(strlen(value)+1));
  if (RPargv[ipar]==NULL) {
    perror("malloc");
    exit(1);
  }
  strcpy(RPargv[ipar],value);
  return 1;
}
