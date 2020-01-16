/* 
   C->F77 wrappers around ReadPar library
*/
#include "readpar.h"

void rp_get_cl_par_ (char *parname, char *parvalue);
void rp_get_pf_par_  (char *file, char *parname, char *parvalue);

int rp_defined_ (char *string);

/*------------------------------------------------------------------*/
void rp_get_cl_par_ (char *parname, char *parvalue)
{
  get_command_line_par (parname, parvalue);
}

/*------------------------------------------------------------------*/
void rp_get_numbered_par_ (char *parname, int *n, char *parvalue)
{
  get_numbered_par (parname, *n, parvalue);
}

/*------------------------------------------------------------------*/
void rp_get_string_par_ (char *string, char *parname, char *parvalue)
{
  get_string_par (string, parname, parvalue);
}

/*------------------------------------------------------------------*/
void rp_get_pf_par_  (char *file, char *parname, char *parvalue)
{
  get_parameter_file_par (file, parname, parvalue);
}

/*------------------------------------------------------------------*/
void rp_get_cl_par_position_ (int *position)
{
  *position = last_command_line_par_position();
}

/*------------------------------------------------------------------*/
void rp_get_par_val_position_ (int *position)
{
  *position = last_parameter_val_position();
}

/*------------------------------------------------------------------*/

void ini_readpar_ ()
{ /* This is Ini_ReadPar for F77 interface */

  static int readpar_initialized = 0;
  
  int npar, ipar;
  char work[1024];

  if ( readpar_initialized ) return;

  /* 1) Find the number of arguments */
  rp_iargc_(&npar); /* F77's iargc() */

  /* 2) Allocate memory for RPargv; */
  AllocRPargv (npar); /* AllocRPargv sets RPargc and allocates memory for
			 RPargv */

  /* 3) get pars */
  for (ipar=0;ipar<npar+1;ipar++) {
    rp_getarg_ (&ipar,work);
    SetRPargv (ipar,work); /* SetRPargv allocates memory for RPargv[ipar]
			      and sets it to the value of `work' */
  }


  
  readpar_initialized = 1;
}
  

/*-----------------------------------------------------------------------*/
void rp_expand_variables_c_ (char *str)
{
  RP_expand_variables (str);
}
