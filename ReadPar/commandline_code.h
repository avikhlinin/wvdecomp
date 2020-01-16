/* the very same code can be used for parsing strings so I moved it to 
   a separate file */

int get_command_line_par_work (char *parname, char *parvalue)
{
  /* if parnames starts with an '^' character, name matching is    */
  /*   case-sensitive (and ^ is discarded)                         */
  
  int qcase;
  char name[2048], arg[2048], argw[2048], arg2[2048];
  int narg,i,nname;
  char *p;

  
  if (parname[0]=='^') {
    qcase = 1;
    strcpy(name,parname+1);
  }
  else {
    qcase = 0;
    strcpy (name,parname);
    lcase (name);
  }
  nname=strlen(name);

  narg = iargc ();

  for (i=1; i<narg; i++) {
    getarg (i,arg);
    RP_LAST_ARG = i;
    RP_LAST_VAL = i;
    if (arg[0]=='@') {
      get_parameter_file_par (arg+1,parname,parvalue);
      if (defined(parvalue)) return 1;
    }
    

    strcpy(argw,arg); if (!qcase) lcase(argw);
    if ( ( p=strchr(argw,'=') ) != NULL ) { /* there is an '=' char in arg */
      if ( (p-argw==nname) && (strncmp(name,argw,p-argw) == 0) ) {
	/* this is par=... string */
	p++; /* skip the = sign */
	if (*p=='\0') { /* -> par= value */
	  RP_LAST_VAL = i+1;
	  return getarg (i+1,parvalue);
	}
	else {          /* -> par=value */
	  strcpy(parvalue,arg+(p-argw));
	  return 1;
	}
      }
      else { /* this is an argument with a parameter name; we need to skip 
		its value */
	p++; /* skip the = sign */
	if ( *p=='\0') {
	  i++;
	}
      }
    }
    else { /* there is no = sign in the arg; it must match the name */
      if (strcmp(name,argw)==0) { /* then check that the next word starts
				     starts with an = sign; */
	getarg (i+1,arg2);
	if (arg2[0]=='=') { /* this is our value */
	  p=arg2+1; /* skip = */
	  if (*p!='\0') {
	    RP_LAST_VAL =i+1;
	    strcpy(parvalue,p);
	    return 0;
	  }
	  else {/* get the next argument and return */
	    RP_LAST_VAL =i+2;
	    return getarg (i+2,parvalue);
	  }
	}
      }
      else { /* finally, consider -par value case */
	if (argw[0]=='-' && strcmp(name,argw+1) == 0 ){ /* -[our par] value */
	  RP_LAST_VAL =i+1;
	  return getarg (i+1,parvalue);
	}
      }
    }
  }
  
  strcpy(parvalue,RP_UNDEFINED); 
  return 1;
}
