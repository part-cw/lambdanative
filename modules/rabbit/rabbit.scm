;; Rabbit stream cipher 
;; written by Martin Boesgaard, Mette Vesterager, Thomas Christensen and Erik Zenner
;; public domain
;; key is 128 bit == 16 characters
;; iv is 64 bit = 8 characters

;; repackaged for lambdanative (minimal wrapper)

(define rabbit:debuglevel 0)
(define (rabbit:log level . x)
   (if (>= rabbit:debuglevel level) (apply log-system (append (list "rabbit: ") x))))

(c-declare  #<<end-of-c-declare

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define U32TO32_LITTLE(v) (v)
#define u8 unsigned char
#define u32 unsigned int
#define U32C(v) (v##U)
#define U32V(v) ((u32)(v) & U32C(0xFFFFFFFF))
#define ROTL32(v, n) (U32V((v) << (n)) | ((v) >> (32 - (n))))
#define U8TO32_LITTLE(p) U32TO32_LITTLE(((u32*)(p))[0])

typedef struct
{
   u32 x[8];
   u32 c[8];
   u32 carry;
} RABBIT_ctx;

typedef struct
{
   RABBIT_ctx master_ctx;
   RABBIT_ctx work_ctx;
} ECRYPT_ctx;


static u32 RABBIT_g_func(u32 x)
{
   u32 a, b, h, l;
   a = x&0xFFFF;
   b = x>>16;
   h = (((U32V(a*a)>>17) + U32V(a*b))>>15) + b*b;
   l = x*x;
   return U32V(h^l);
}

static void RABBIT_next_state(RABBIT_ctx *p_instance)
{
   u32 g[8], c_old[8], i;
   for (i=0; i<8; i++) c_old[i] = p_instance->c[i];
   p_instance->c[0] = U32V(p_instance->c[0] + 0x4D34D34D + p_instance->carry);
   p_instance->c[1] = U32V(p_instance->c[1] + 0xD34D34D3 + (p_instance->c[0] < c_old[0]));
   p_instance->c[2] = U32V(p_instance->c[2] + 0x34D34D34 + (p_instance->c[1] < c_old[1]));
   p_instance->c[3] = U32V(p_instance->c[3] + 0x4D34D34D + (p_instance->c[2] < c_old[2]));
   p_instance->c[4] = U32V(p_instance->c[4] + 0xD34D34D3 + (p_instance->c[3] < c_old[3]));
   p_instance->c[5] = U32V(p_instance->c[5] + 0x34D34D34 + (p_instance->c[4] < c_old[4]));
   p_instance->c[6] = U32V(p_instance->c[6] + 0x4D34D34D + (p_instance->c[5] < c_old[5]));
   p_instance->c[7] = U32V(p_instance->c[7] + 0xD34D34D3 + (p_instance->c[6] < c_old[6]));
   p_instance->carry = (p_instance->c[7] < c_old[7]);
   for (i=0;i<8;i++) g[i] = RABBIT_g_func(U32V(p_instance->x[i] + p_instance->c[i]));
   p_instance->x[0] = U32V(g[0] + ROTL32(g[7],16) + ROTL32(g[6], 16));
   p_instance->x[1] = U32V(g[1] + ROTL32(g[0], 8) + g[7]);
   p_instance->x[2] = U32V(g[2] + ROTL32(g[1],16) + ROTL32(g[0], 16));
   p_instance->x[3] = U32V(g[3] + ROTL32(g[2], 8) + g[1]);
   p_instance->x[4] = U32V(g[4] + ROTL32(g[3],16) + ROTL32(g[2], 16));
   p_instance->x[5] = U32V(g[5] + ROTL32(g[4], 8) + g[3]);
   p_instance->x[6] = U32V(g[6] + ROTL32(g[5],16) + ROTL32(g[4], 16));
   p_instance->x[7] = U32V(g[7] + ROTL32(g[6], 8) + g[5]);
}

void rabbit_keysetup(ECRYPT_ctx* ctx, const u8* key)
{
   u32 k0, k1, k2, k3, i;
   k0 = U8TO32_LITTLE(key+ 0);
   k1 = U8TO32_LITTLE(key+ 4);
   k2 = U8TO32_LITTLE(key+ 8);
   k3 = U8TO32_LITTLE(key+12);
   ctx->master_ctx.x[0] = k0;
   ctx->master_ctx.x[2] = k1;
   ctx->master_ctx.x[4] = k2;
   ctx->master_ctx.x[6] = k3;
   ctx->master_ctx.x[1] = U32V(k3<<16) | (k2>>16);
   ctx->master_ctx.x[3] = U32V(k0<<16) | (k3>>16);
   ctx->master_ctx.x[5] = U32V(k1<<16) | (k0>>16);
   ctx->master_ctx.x[7] = U32V(k2<<16) | (k1>>16);
   ctx->master_ctx.c[0] = ROTL32(k2, 16);
   ctx->master_ctx.c[2] = ROTL32(k3, 16);
   ctx->master_ctx.c[4] = ROTL32(k0, 16);
   ctx->master_ctx.c[6] = ROTL32(k1, 16);
   ctx->master_ctx.c[1] = (k0&0xFFFF0000) | (k1&0xFFFF);
   ctx->master_ctx.c[3] = (k1&0xFFFF0000) | (k2&0xFFFF);
   ctx->master_ctx.c[5] = (k2&0xFFFF0000) | (k3&0xFFFF);
   ctx->master_ctx.c[7] = (k3&0xFFFF0000) | (k0&0xFFFF);
   ctx->master_ctx.carry = 0;
   for (i=0; i<4; i++) RABBIT_next_state(&(ctx->master_ctx));
   for (i=0; i<8; i++) ctx->master_ctx.c[i] ^= ctx->master_ctx.x[(i+4)&0x7];
   for (i=0; i<8; i++) {
      ctx->work_ctx.x[i] = ctx->master_ctx.x[i];
      ctx->work_ctx.c[i] = ctx->master_ctx.c[i];
   }
   ctx->work_ctx.carry = ctx->master_ctx.carry;
}

void rabbit_ivsetup(ECRYPT_ctx* ctx, const u8* iv)
{
   u32 i0, i1, i2, i3, i;
   i0 = U8TO32_LITTLE(iv+0);
   i2 = U8TO32_LITTLE(iv+4);
   i1 = (i0>>16) | (i2&0xFFFF0000);
   i3 = (i2<<16) | (i0&0x0000FFFF);
   ctx->work_ctx.c[0] = ctx->master_ctx.c[0] ^ i0;
   ctx->work_ctx.c[1] = ctx->master_ctx.c[1] ^ i1;
   ctx->work_ctx.c[2] = ctx->master_ctx.c[2] ^ i2;
   ctx->work_ctx.c[3] = ctx->master_ctx.c[3] ^ i3;
   ctx->work_ctx.c[4] = ctx->master_ctx.c[4] ^ i0;
   ctx->work_ctx.c[5] = ctx->master_ctx.c[5] ^ i1;
   ctx->work_ctx.c[6] = ctx->master_ctx.c[6] ^ i2;
   ctx->work_ctx.c[7] = ctx->master_ctx.c[7] ^ i3;
   for (i=0; i<8; i++) ctx->work_ctx.x[i] = ctx->master_ctx.x[i];
   ctx->work_ctx.carry = ctx->master_ctx.carry;
   for (i=0; i<4; i++) RABBIT_next_state(&(ctx->work_ctx));
}

void rabbit_process_bytes(ECRYPT_ctx* ctx, const u8* input, u8* output, u32 msglen)
{
   u32 i;
   u8 buffer[16];
   while (msglen >= 16)
   {
      RABBIT_next_state(&(ctx->work_ctx));
      *(u32*)(output+ 0) = *(u32*)(input+ 0) ^ U32TO32_LITTLE(ctx->work_ctx.x[0] ^
                (ctx->work_ctx.x[5]>>16) ^ U32V(ctx->work_ctx.x[3]<<16));
      *(u32*)(output+ 4) = *(u32*)(input+ 4) ^ U32TO32_LITTLE(ctx->work_ctx.x[2] ^ 
                (ctx->work_ctx.x[7]>>16) ^ U32V(ctx->work_ctx.x[5]<<16));
      *(u32*)(output+ 8) = *(u32*)(input+ 8) ^ U32TO32_LITTLE(ctx->work_ctx.x[4] ^ 
                (ctx->work_ctx.x[1]>>16) ^ U32V(ctx->work_ctx.x[7]<<16));
      *(u32*)(output+12) = *(u32*)(input+12) ^ U32TO32_LITTLE(ctx->work_ctx.x[6] ^ 
                (ctx->work_ctx.x[3]>>16) ^ U32V(ctx->work_ctx.x[1]<<16));
      input += 16;
      output += 16;
      msglen -= 16;
   }
   if (msglen)
   {
      RABBIT_next_state(&(ctx->work_ctx));
      *(u32*)(buffer+ 0) = U32TO32_LITTLE(ctx->work_ctx.x[0] ^
                (ctx->work_ctx.x[5]>>16) ^ U32V(ctx->work_ctx.x[3]<<16));
      *(u32*)(buffer+ 4) = U32TO32_LITTLE(ctx->work_ctx.x[2] ^ 
                (ctx->work_ctx.x[7]>>16) ^ U32V(ctx->work_ctx.x[5]<<16));
      *(u32*)(buffer+ 8) = U32TO32_LITTLE(ctx->work_ctx.x[4] ^ 
                (ctx->work_ctx.x[1]>>16) ^ U32V(ctx->work_ctx.x[7]<<16));
      *(u32*)(buffer+12) = U32TO32_LITTLE(ctx->work_ctx.x[6] ^ 
                (ctx->work_ctx.x[3]>>16) ^ U32V(ctx->work_ctx.x[1]<<16));
      for (i=0; i<msglen; i++) output[i] = input[i] ^ buffer[i];
   }
}

// --------------
// primitive glue 

ECRYPT_ctx *_rabbit_make(void *key, int l) {
  ECRYPT_ctx *ctx=(ECRYPT_ctx *)malloc(sizeof (ECRYPT_ctx));
  if (ctx) { rabbit_keysetup(ctx, key); }
  return ctx;
}

void _rabbit_destroy(ECRYPT_ctx *ctx)
{
  free(ctx);
}

void _rabbit_encode(ECRYPT_ctx *ctx, void *src, int l) {
  unsigned char iv[8]={0,0,0,0,0,0,0,0};
  unsigned char *tmpdst = (unsigned char*)malloc(l); 
  iv[0]=(l>>8)&0xff; iv[1]=(l>>1)&0xff; 
  iv[2]=(l>>4)&0xff; iv[3]=(l>>3)&0xff; 
  iv[4]=(l>>6)&0xff; iv[5]=(l>>5)&0xff; 
  iv[6]=(l>>2)&0xff; iv[7]=(l>>7)&0xff; 
  if (tmpdst) {
    rabbit_ivsetup(ctx, iv);
    rabbit_process_bytes(ctx, src, tmpdst, l);
    memcpy(src,tmpdst,l);
    free(tmpdst);
  }
}

end-of-c-declare
)

;; --------------
;; minimal ffi

;; key is 128 bit == 16 characters
;; iv is 64 bit = 8 characters
(define (rabbit-make key)  ;; key must be at least 24 bytes
  (rabbit:log 1 "rabbit-make " key)
  ((c-lambda (scheme-object int) (pointer void)
     "___result=_rabbit_make(___CAST(void*,___BODY_AS(___arg1,___tSUBTYPED)), ___arg2);") 
         key (u8vector-length key)))

(define (rabbit-destroy ctx)
  (rabbit:log 1 "rabbit-destroy " ctx)
  ((c-lambda ((pointer void)) void "_rabbit_destroy") ctx)
)

(define (rabbit-encode ctx u8v)
  (rabbit:log 2 "rabbit-encode/decode " ctx " " u8v)
  (if (u8vector? u8v) (begin
    ((c-lambda ((pointer void) scheme-object int) void 
       "_rabbit_encode(___arg1,___CAST(void*,___BODY_AS(___arg2,___tSUBTYPED)), ___arg3);")
           ctx u8v (u8vector-length u8v)) u8v) #f))

(define rabbit-decode rabbit-encode)

;; ----

(unit-test "rabbit" "1000 random vectors" 
  (lambda () (let loop ((n 1000))
      (if (fx= n 0) #t (if (let* (
           (keylen (+ (random-integer 10) 24)) 
           (key (random-u8vector keylen))
           (datalen (random-integer 100000)) 
           (data (random-u8vector datalen))
           (ctx (rabbit-make key)))
          (let ((res (not (equal? data (rabbit-decode ctx (rabbit-encode ctx data))))))
            (rabbit-destroy ctx) res)
        ) #f (loop (fx- n 1)))))))

;; eof
