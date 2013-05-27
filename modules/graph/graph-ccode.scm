;; graph c code snippets
(c-declare  #<<end-of-c-declare

#include <stdio.h>
#include <string.h>
#include <math.h>

#define TRUE 1
#define FALSE 0
#define DOWN    0
#define UP      1
#define EVEN    2

static char *graph_wc_linecomp(char *line, int Ept){
int count;
    if (line[Ept-1] == '.'){     /* ERASE THE DECIMAL POINT */
        count=Ept-1;
        --Ept;
        while (line[count] != '\0'){  /* Shift the string back by one */
            line[count]= line[count+1];
            ++count; 
        }
    }    
    if (line[Ept+1] == '+'){     /* ERASE the PLUS sign AFTER the E */
        count=Ept+1;
        --Ept;
        while (line[count] != '\0'){  /* Shift the string back by one */
            line[count]= line[count+1];
            ++count; 
        }
    }    
    while (line[Ept+2] == '0'){     /* ERASE ANY UNNECESSARY ZEROES */
        count=Ept+2;
        while (line[count] != '\0'){  /* Shift the string back by one */
            line[count]= line[count+1];
            ++count;
        }
    }
    return( line );
}

static float graph_wc_ybottom(int multiplier, float  cgymi, float cgldecy){
  float bottom;
  bottom= cgldecy*((float)multiplier)/10.;
  while (bottom < cgymi) bottom *= 10.;
  return(bottom);
}

static float graph_wc_xbottom(int multiplier, float cgxmi, float cgldecx){
  float bottom;
  bottom= cgldecx*((float)multiplier)/10.;
  while (bottom < cgxmi) bottom *= 10.;
  return(bottom);
}

static float graph_wc_trunc(float fvalue, int updown){
  float decade,result;
  double dublog;
  dublog= log10(fvalue);
  dublog= floor(dublog);
  decade= pow(10.0, dublog);
  result= fvalue/decade;
  if (updown == UP)
    result = ceil(result);
  else if (updown == DOWN)
    result = floor(result);
  else result= floor(result + 0.5);
  result *= decade;
  return(result);
}

static int graph_findsigdec(float min, float range, int numsep, float ticks, int cgprecision, int cglinnumoff){
  float var;
  int expon,count,decpt,sigdec;
  char line[30];
  sigdec=0;
  for (var= cglinnumoff*ticks;var<=range;var += numsep*ticks) {
    if((var +min) < pow(10.0,(float)cgprecision))
       sprintf(line, "%.*g", cgprecision, var+min);
    else sprintf(line, "%.*e", cgprecision, var+min);

    expon= FALSE;
    count=0;
    decpt=0;
    while (line[count] != '\0') {
            if (line[++count] == 'e' ) {        /* search for exponent marker */
                expon=TRUE;
            }
            if (line[count] == '.' )   /* locate decimal point */
                    decpt=count;
        }
        if (decpt == 0) decpt = count - 1;      /* decpt is end of number */
            sigdec=((count-decpt-1)>sigdec)?(count-decpt-1):sigdec;
     } /* end for() */
     return(sigdec);
}

static char *graph_formaxnum(float value, int sigdec, int cgprecision){
    static char line[30];
    int expon, count, decpt, ept=0;

    sprintf(line, "%#.*g", cgprecision, value);

    /* convert number to string form */

    expon= FALSE;
    count=0;
    decpt=0;
    while (line[count] != '\0') {
        if (line[++count] == 'e' ) {
            expon=TRUE;
            ept=count;
        }
        if (line[count] == '.' ) 
            decpt=count;
    }
    if (expon == TRUE) {  // need to clean up gcvt's exp notation
        graph_wc_linecomp(line,ept);
        count= -1;
        while (line[++count] != '\0'); 
    }
    if(decpt) {
        if (sigdec) {
          if (sigdec<=cgprecision||expon) { 
            line[sigdec+decpt+1]='\0';     // original
          } else {
            line[cgprecision+decpt+1]='\0'; 
          }
        }
        else line[decpt]='\0';
    }
    return line; 
}

static char *graph_fix_string(char *text,int fix_minus){
    char *sptr, *dptr;
    char ch;
    static char out[128];
    sptr = text;
    dptr = out;
    while( (ch = *sptr++) != '\0'){
        if( ch == '-'){
            if(fix_minus){
               *dptr++ = '\\';      /* subst with octal 261 */
               *dptr++ = '2';       /* "-" --> "\261" */
               *dptr++ = '6';
               *dptr++ = '1';
        }
    }
    else if( ch == '\\' || ch == '(' || ch == ')' ){
        *dptr++ = '\\';
        *dptr++ = ch;
    }
    else                            /* normal characters */
        *dptr++ = ch;           /* just copy */
    }
    *dptr++ = '\0';         /* string terminator */
    return(out);    /* return ptr to the fixed string */
}

end-of-c-declare
)

(define graphaux:findsigdec (c-lambda (float float int float int int) int "graph_findsigdec"))
(define graphaux:formaxnum (c-lambda (float int int) char-string "graph_formaxnum"))
(define graphaux:fixstring (c-lambda (char-string int) char-string "graph_fix_string"))
(define graphaux:wctrunc (c-lambda (float int) float "graph_wc_trunc"))
(define graphaux:wcxbottom (c-lambda (int float float) float "graph_wc_xbottom"))
(define graphaux:wcybottom (c-lambda (int float float) float "graph_wc_ybottom"))

;; eof