/*
LambdaNative - a cross-platform Scheme framework
Copyright (c) 2009-2013, University of British Columbia
All rights reserved.

Redistribution and use in source and binary forms, with or
without modification, are permitted provided that the
following conditions are met:

* Redistributions of source code must retain the above
copyright notice, this list of conditions and the following
disclaimer.

* Redistributions in binary form must reproduce the above
copyright notice, this list of conditions and the following
disclaimer in the documentation and/or other materials
provided with the distribution.

* Neither the name of the University of British Columbia nor
the names of its contributors may be used to endorse or
promote products derived from this software without specific
prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR
CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

// simple parameter substitution tool with support for file inclusion

//#define DEBUG_SUBTOOL 1

#define PARAM_MAXLEN 32

#ifdef DEBUG_SUBTOOL
#define DMSG(fmt...) (fprintf(stderr,"SUBTOOL: " fmt),fprintf(stderr,"\n"))
#else
#define DMSG(fmt...)
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

struct pattern {
  char *in;
  char *out;
  struct pattern *nxt;
};

struct pattern *fst=0, *lst=0;

void load_pattern(char *fname)
{
  char buf[1024];
  FILE *fd = fopen(fname,"r");
  int i,l,n=0;
  if (fd) {
    while (1) {
      buf[0]=0; 
      fgets(buf,1024,fd);
      if (feof(fd)||ferror(fd)||buf[0]==0) break;
      l=strlen(buf);
      for (i=0;i<l;i++) { if (buf[i]<32) buf[i]=0; }
      for (i=1;i<l;i++) { if (buf[i]==buf[0]) break; }
      buf[i]=0;
      struct pattern *tmp = (struct pattern*)malloc(sizeof(struct pattern));
      tmp->in = strdup((char*)&buf[1]);
      tmp->out = strdup((char*)&buf[i+1]);
      DMSG("%s -> [%s]", tmp->in, tmp->out);
      tmp->nxt=0;
      if (!fst) {
        fst=lst=tmp;
      } else {
        lst->nxt=tmp;
        lst=tmp;
      } 
      n++;
    } 
    fclose(fd);
    DMSG("%i patterns loaded",n);
  } 
}   

void save_pattern(char *fname)
{
  FILE *fd = fopen(fname,"w");
  if (fd) {
    struct pattern *tmp = fst;
    while (tmp) {
      fprintf(fd,":%s:%s\n",tmp->in,tmp->out);
      tmp=tmp->nxt;
    }
    fclose(fd);
  } else {
    fprintf(stderr,"ERROR: error accessing pattern file: %s\n",fname);
    exit(1);
  }
} 

char *match_pattern(char*);

struct parser {
  char buf[1024];
  int n, in_at;
};
 
char *parse_char(struct parser *p, char c)
{
  if (p->in_at&&c==':') {
    fprintf(stderr,"ERROR: illegal character in substitution parameter\n");
    exit(1);
  }
  // bail out of possible parameter impossible length, or <=32 ascii
  if (p->in_at) {
    if (c<=32||p->n>PARAM_MAXLEN) {
      DMSG("parse_char bailed on c=%i n=%i", (int)c, p->n);
      p->buf[p->n]=0;
      printf("@%s%c", p->buf,c);
      p->in_at=0;
      p->buf[0]=0;
      p->n=0;
      return 0;
    }
  }
  if (c=='@') {
    if (p->in_at) { p->buf[p->n]=0; match_pattern(p->buf); p->n=0; }
    p->in_at=1-p->in_at;
    return 0;
  } else { 
    p->buf[p->n]=c;
    if (p->in_at) { p->n++; return 0; }
  }
  p->buf[p->n+1]=0;
  return p->buf;
}

void parse_file(FILE *fd)
{
  DMSG("parse_file(%p)", fd);
  struct parser *p = (struct parser *)malloc(sizeof(struct parser));
  p->n=0; p->in_at=0; p->buf[0]=0;
  while (1) {
    char c = fgetc(fd);
    if (feof(fd)) break;
    if (ferror(fd)) break;
    char *data = parse_char(p,c);
    if (data) printf("%s",data);
  }
  free(p);
}

char *match_pattern(char *buf)
{
  DMSG("match_pattern(%s)", buf);
  struct pattern *tmp = fst;
  int match=0;
  while (tmp) {
    if (!strcmp(buf,tmp->in)) { 
      match=1; 
      if (tmp->out[0]=='@') {
        FILE *fd = fopen(&tmp->out[1],"r");
        if (fd) {
          parse_file(fd);
          fclose(fd);
        } else {
          fprintf(stdout,"%s",tmp->out); 
        }
      } else {
        fprintf(stdout,"%s",tmp->out); 
      }
    }
    tmp=tmp->nxt;
  }
  if (!match) {
    fprintf(stderr,"ERROR: no match found for @%s@\n", buf);
    exit(1);
  }
  return buf; 
}

void usage()
{
  printf("usage: subtool <pattern db> [<pattern> <substitution>]\n");
  exit(1);
}

int main(int argc, char *argv[])
{
  if (argc==1||argc>4) usage();
  load_pattern(argv[1]);
  if (argc==4) {
    struct pattern *tmp = (struct pattern*)malloc(sizeof(struct pattern));
    tmp->in = strdup(argv[2]);
    tmp->out = strdup(argv[3]);
    tmp->nxt=0;
    if (!fst) {
      fst=lst=tmp;
    } else {
      lst->nxt=tmp;
      lst=tmp;
    } 
    save_pattern(argv[1]);
  } else {
    parse_file(stdin);
  }
  return 0;
}

// eof  
