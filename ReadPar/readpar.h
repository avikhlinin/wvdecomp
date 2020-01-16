#define RP_COMMENT     1
#define RP_PARSTRING   2
#define RP_IRAFSTRING  3
#define RP_PARCONT     4
#define RP_ENDGROUP    5
#define RP_INCLSTRING  6
#define RP_BEGGROUP    7

#define RP_UNDEFINED "undefined"
#define RP_PAR_FILE_ENV "MY_READPAR_FILE"

extern char* RP_Rem_Lead_Blanks (char *string);
extern int   RP_ParString (char *string);
extern int   RP_IRAFString (char *string);

extern int RP_InGroup();
extern int RP_PAR_Continued();

extern int IniReadPar(int argc,int **argv);
extern int get_command_line_par (char *name, char *value);
extern int get_cl_or_read_par (char *name, char *description, char *value);
extern int get_parameter_file_par (char *file, char *name, char *value);
extern int last_command_line_par_position ();
extern int last_parameter_val_position ();
extern int RP_q_expand_vars();
extern int RP_set_expand_vars (int flag);
extern int defined(char *arg);
extern int yespar (char *name);
extern int nopar (char *name);
extern int defpar (char *name);
extern int set_default_par_value (char *name, char *value);
extern int RP_chomp (char *string);
