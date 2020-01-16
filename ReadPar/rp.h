char *RP_Rem_Lead_Blanks (char *line);

/* recordtype.c */
extern int RP_RecordType (char *line);
extern int RP_Parse_at_Line (char *line);

/* parstring.c */
extern int RP_ParString (char *string);

/* parse_p_str.c */
extern int RP_Parse_Par_String (char *line, 
				char *name, 
				char *value, 
				char *comment,
				int  *valuecont);

/* parse_group.c */
extern int RP_Parse_Group_String (char *line, 
				  char *name, 
				  char *comment);

/* parse_cont.c */
extern int RP_Parse_Cont_String (char *line, 
				 char *value, 
				 char *comment,
				 int  *valuecont);



extern char* RP_Rem_Lead_Blanks (char *string);
extern int   RP_IRAFString (char *string);

