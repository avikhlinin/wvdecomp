#include <stdlib.h>
#include <string.h>
#include <sys/param.h>

void realpath_ (char *filename, char *pathname)
{

#ifdef LINUX
  char resolvedname[PATH_MAX];
#else
  char resolvedname[MAXPATHLEN];
#endif

  if (realpath(filename,resolvedname)==NULL)
    {
      perror(filename);
      exit(-1);
    }
  strcpy(pathname,resolvedname);
}
