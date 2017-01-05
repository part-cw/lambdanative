/*
 * Copyright (c) 2007 Alexey Vatchenko <av@bsdua.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 */

#include <sys/types.h>
#include <errno.h>
#include <limits.h>
#include <string.h>

#include "defs.h"

#include "portable_endian.h"

/*
 * Implementation of UTF-8 (RFC3629), UTF-16 (RFC2781), UCS-4, UCS-2. Also
 * little-endian, big-endian variants are implemented.
 */

#define _NXT	0x80
#define _SEQ2	0xc0
#define _SEQ3	0xe0
#define _SEQ4	0xf0
#define _SEQ5	0xf8
#define _SEQ6	0xfc

static int __wchar_forbitten(WIDECHAR sym);
static int __utf8_forbitten(u_char octet);

int	utf8_mbtowc(struct conv_state *cs, const char *in, size_t insize,
	    WIDECHAR *out, size_t *inused);
int	utf8_wctomb(struct conv_state *cs, WIDECHAR in, char *out,
	    size_t outsize, size_t *outused);

int	utf16_mbtowc(struct conv_state *cs, const char *in, size_t insize,
	    WIDECHAR *out, size_t *inused);
int	utf16_wctomb(struct conv_state *cs, WIDECHAR in, char *out,
	    size_t outsize, size_t *outused);

int	utf16le_mbtowc(struct conv_state *cs, const char *in, size_t insize,
	    WIDECHAR *out, size_t *inused);
int	utf16le_wctomb(struct conv_state *cs, WIDECHAR in, char *out,
	    size_t outsize, size_t *outused);

int	utf16be_mbtowc(struct conv_state *cs, const char *in, size_t insize,
	    WIDECHAR *out, size_t *inused);
int	utf16be_wctomb(struct conv_state *cs, WIDECHAR in, char *out,
	    size_t outsize, size_t *outused);

int	ucs4_mbtowc(struct conv_state *cs, const char *in, size_t insize,
	    WIDECHAR *out, size_t *inused);
int	ucs4_wctomb(struct conv_state *cs, WIDECHAR in, char *out,
	    size_t outsize, size_t *outused);

int	ucs4le_mbtowc(struct conv_state *cs, const char *in, size_t insize,
	    WIDECHAR *out, size_t *inused);
int	ucs4le_wctomb(struct conv_state *cs, WIDECHAR in, char *out,
	    size_t outsize, size_t *outused);

int	ucs4be_mbtowc(struct conv_state *cs, const char *in, size_t insize,
	    WIDECHAR *out, size_t *inused);
int	ucs4be_wctomb(struct conv_state *cs, WIDECHAR in, char *out,
	    size_t outsize, size_t *outused);

int	ucs2_mbtowc(struct conv_state *cs, const char *in, size_t insize,
	    WIDECHAR *out, size_t *inused);
int	ucs2_wctomb(struct conv_state *cs, WIDECHAR in, char *out,
	    size_t outsize, size_t *outused);

int	ucs2le_mbtowc(struct conv_state *cs, const char *in, size_t insize,
	    WIDECHAR *out, size_t *inused);
int	ucs2le_wctomb(struct conv_state *cs, WIDECHAR in, char *out,
	    size_t outsize, size_t *outused);

int	ucs2be_mbtowc(struct conv_state *cs, const char *in, size_t insize,
	    WIDECHAR *out, size_t *inused);
int	ucs2be_wctomb(struct conv_state *cs, WIDECHAR in, char *out,
	    size_t outsize, size_t *outused);


static int
__wchar_forbitten(WIDECHAR sym)
{

	/* Surrogate pairs */
	if (sym >= 0xd800 && sym <= 0xdfff)
		return (-1);

	return (0);
}

static int
__utf8_forbitten(u_char octet)
{

	switch (octet) {
	case 0xc0:
	case 0xc1:
	case 0xf5:
	case 0xff:
		return (-1);
	}

	return (0);
}

int
utf8_mbtowc(struct conv_state *cs, const char *in, size_t insize, WIDECHAR *out,
    size_t *inused)
{
	u_int i, n, n_bits;
	u_char *p;
	WIDECHAR high;

	p = (u_char *)in;

	if (__utf8_forbitten(*p) != 0)
		return (EILSEQ);

	/*
	 * Get number of bytes for one wide character.
	 */
	if ((*p & 0x80) == 0) {
		n = 1;
		high = (WIDECHAR)*p;
	} else if ((*p & 0xe0) == _SEQ2) {
		n = 2;
		high = (WIDECHAR)(*p & 0x1f);
	} else if ((*p & 0xf0) == _SEQ3) {
		n = 3;
		high = (WIDECHAR)(*p & 0x0f);
	} else if ((*p & 0xf8) == _SEQ4) {
		n = 4;
		high = (WIDECHAR)(*p & 0x07);
	} else if ((*p & 0xfc) == _SEQ5) {
		n = 5;
		high = (WIDECHAR)(*p & 0x03);
	} else if ((*p & 0xfe) == _SEQ6) {
		n = 6;
		high = (WIDECHAR)(*p & 0x01);
	} else
		return (EILSEQ);

	/* does the sequence header tell us truth about length? */
	if (n > insize)
		return (EINVAL);

	/*
	 * Validate sequence.
	 * All symbols must have higher bits set to 10xxxxxx
	 */
	if (n > 1) {
		for (i = 1; i < n; i++) {
			if ((p[i] & 0xc0) != _NXT)
				break;
		}

		if (i != n)
			return (EILSEQ);
	}

	*out = 0;
	n_bits = 0;
	for (i = 1; i < n; i++) {
		*out |= (WIDECHAR)(p[n - i] & 0x3f) << n_bits;
		n_bits += 6;		/* 6 low bits in every byte */
	}
	*out |= high << n_bits;

	if (__wchar_forbitten(*out) != 0)
		return (EILSEQ);

	*inused = n;

	/* Skip leading BOM symbol */
	if (cs->fbo == ICONV_BO_UNDEF) {
		cs->fbo = ICONV_BO_HOST;
		if (*out == 0xfeff)
			return (EAGAIN);
	}

	return (0);
}

int
utf8_wctomb(struct conv_state *cs, WIDECHAR in, char *out, size_t outsize,
    size_t *outused)
{
	WIDECHAR ch;
	u_int n;
	u_char *p, *oc;

	if (in <= 0x0000007f)
		n = 1;
	else if (in <= 0x000007ff)
		n = 2;
	else if (in <= 0x0000ffff)
		n = 3;
	else if (in <= 0x001fffff)
		n = 4;
	else if (in <= 0x03ffffff)
		n = 5;
	else /* if (in <= 0x7fffffff) */
		n = 6;

	if (n > outsize)
		return (E2BIG);

	/* make it work under different endians */
	ch = htonl(in);
	oc = (u_char *)&ch;

	p = (u_char *)out;

	switch (n) {
	case 1:
		*p = oc[3];
		break;

	case 2:
		p[1] = _NXT | (oc[3] & 0x3f);
		p[0] = _SEQ2 | (oc[3] >> 6) | ((oc[2] & 0x07) << 2);
		break;

	case 3:
		p[2] = _NXT | (oc[3] & 0x3f);
		p[1] = _NXT | (oc[3] >> 6) | ((oc[2] & 0x0f) << 2);
		p[0] = _SEQ3 | ((oc[2] & 0xf0) >> 4);
		break;

	case 4:
		p[3] = _NXT | (oc[3] & 0x3f);
		p[2] = _NXT | (oc[3] >> 6) | ((oc[2] & 0x0f) << 2);
		p[1] = _NXT | ((oc[2] & 0xf0) >> 4) | ((oc[1] & 0x03) << 4);
		p[0] = _SEQ4 | ((oc[1] & 0x1f) >> 2);
		break;

	case 5:
		p[4] = _NXT | (oc[3] & 0x3f);
		p[3] = _NXT | (oc[3] >> 6) | ((oc[2] & 0x0f) << 2);
		p[2] = _NXT | ((oc[2] & 0xf0) >> 4) | ((oc[1] & 0x03) << 4);
		p[1] = _NXT | (oc[1] >> 2);
		p[0] = _SEQ5 | (oc[0] & 0x03);
		break;

	case 6:
		p[5] = _NXT | (oc[3] & 0x3f);
		p[4] = _NXT | (oc[3] >> 6) | ((oc[2] & 0x0f) << 2);
		p[3] = _NXT | (oc[2] >> 4) | ((oc[1] & 0x03) << 4);
		p[2] = _NXT | (oc[1] >> 2);
		p[1] = _NXT | (oc[0] & 0x3f);
		p[0] = _SEQ6 | ((oc[0] & 0x40) >> 6);
		break;
	}

	*outused = n;
	return (0);
}

int
utf16_mbtowc(struct conv_state *cs, const char *in, size_t insize,
    WIDECHAR *out, size_t *inused)
{
	u_char *p;
	size_t used;
	u_int16_t w1, w2;
	int fbo;

	if (insize < 2)
		return (EINVAL);

	p = (u_char *)in;
	used = 0;
	fbo = cs->fbo;
	if (fbo == ICONV_BO_UNDEF) {
		if (*p == 0xfe && *(p + 1) == 0xff) {
			fbo = ICONV_BO_BIG;
			used = 2;
			p += 2;
		} else if (*p == 0xff && *(p + 1) == 0xfe) {
			fbo = ICONV_BO_LITTLE;
			used = 2;
			p += 2;
		} else {
			/* 4.3 - default endian is Big-Endian */
			fbo = ICONV_BO_BIG;
		}
	}

	if (insize < 2 + used)
		return (EINVAL);

	w1 = (fbo == ICONV_BO_BIG) ? be16toh(*(u_int16_t *)p) :
	    le16toh(*(u_int16_t *)p);

	if (w1 < 0xd800 || w1 > 0xdfff) {
		*out = w1;
		*inused = 2 + used;
		cs->fbo = fbo;
		return (0);
	}

	if (w1 < 0xd800 || w1 > 0xdbff)
		return (EILSEQ);

	if (insize < 4 + used)
		return (EINVAL);	/* W2 is missing */
	p += 2;
	w2 = (fbo == ICONV_BO_BIG) ? be16toh(*(u_int16_t *)p) :
	    le16toh(*(u_int16_t *)p);
	if (w2 < 0xdc00 || w2 > 0xdfff)
		return (EILSEQ);

	*out = (((WIDECHAR)(w1 & 0x3ff) << 10) | (w2 & 0x3ff)) + 0x10000;
	*inused = 4 + used;
	cs->fbo = fbo;
	return (0);
}

int
utf16_wctomb(struct conv_state *cs, WIDECHAR in, char *out, size_t outsize,
    size_t *outused)
{
	WIDECHAR u;
	u_int16_t w1, w2;
	size_t used;
	int tbo;

	if (in > 0x10ffff)
		return (EILSEQ);	/* 2.1 in rfc */

	if (outsize < 2)
		return (E2BIG);

	used = 0;
	tbo = cs->tbo;
	if (tbo == ICONV_BO_UNDEF) {
		tbo = ICONV_BO_BIG;	/* Internet likes Big-Endian :) */
		*(u_char *)out = 0xfe;
		*((u_char *)out + 1) = 0xff;
		used = 2;
	}

	if (in < 0x10000) {
		if (outsize < 2 + used)
			return (E2BIG);
		*(u_int16_t *)(out + used) = (tbo == ICONV_BO_BIG) ?
		    htobe16(in) : htole16(in);
		*outused = 2 + used;
		cs->tbo = tbo;
		return (0);
	}

	if (outsize < 4 + used)
		return (E2BIG);
	u = in - 0x10000;
	w1 = 0xd800 | ((u >> 10) & 0x3ff);
	w2 = 0xdc00 | (u & 0x3ff);
	*(u_int16_t *)(out + used) = (cs->tbo == ICONV_BO_BIG) ? htobe16(w1) :
	    htole16(w1);
	*((u_int16_t *)(out + used) + 1) = (cs->tbo == ICONV_BO_BIG) ?
	    htobe16(w2) : htole16(w2);
	*outused = 4 + used;
	cs->tbo = tbo;
	return (0);
}

int
utf16le_mbtowc(struct conv_state *cs, const char *in, size_t insize,
    WIDECHAR *out, size_t *inused)
{
	cs->fbo = ICONV_BO_LITTLE;
	cs->mbtowc = utf16_mbtowc;
	return utf16_mbtowc(cs, in, insize, out, inused);
}

int
utf16le_wctomb(struct conv_state *cs, WIDECHAR in, char *out, size_t outsize,
    size_t *outused)
{
	cs->tbo = ICONV_BO_LITTLE;
	cs->wctomb = utf16_wctomb;
	return utf16_wctomb(cs, in, out, outsize, outused);
}

int
utf16be_mbtowc(struct conv_state *cs, const char *in, size_t insize,
    WIDECHAR *out, size_t *inused)
{
	cs->fbo = ICONV_BO_BIG;
	cs->mbtowc = utf16_mbtowc;
	return utf16_mbtowc(cs, in, insize, out, inused);
}

int
utf16be_wctomb(struct conv_state *cs, WIDECHAR in, char *out, size_t outsize,
    size_t *outused)
{
	cs->tbo = ICONV_BO_BIG;
	cs->wctomb = utf16_wctomb;
	return utf16_wctomb(cs, in, out, outsize, outused);
}

int
ucs4_mbtowc(struct conv_state *cs, const char *in, size_t insize, WIDECHAR *out,
    size_t *inused)
{
	if (insize < 4)
		return (EINVAL);

	if (cs->fbo == ICONV_BO_UNDEF) {
		/*
 		 * Determine byte order. If we get BOM, we switch mbtowc handler
 		 * to the exact big/little mbtowc implementation. Otherwise
 		 * we use host byte order.
 		 */
		if (*(u_char *)in == 0xff && *((u_char *)in + 1) == 0xfe &&
		    *((u_char *)in + 2) == 0 && *((u_char *)in + 3) == 0)
			cs->mbtowc = ucs4le_mbtowc;
		else if (*(u_char *)in == 0 && *((u_char *)in + 1) == 0 &&
		    *((u_char *)in + 2) == 0xfe && *((u_char *)in + 3) == 0xff)
			cs->mbtowc = ucs4be_mbtowc;
		else
			cs->fbo = ICONV_BO_HOST;

		if (cs->fbo != ICONV_BO_HOST) {
			/*
			 * We need to restart mbtowc because it might have been
			 * changed.
			 */
			*inused = 4;
			return (EAGAIN);
		}
	}

	*out = *(WIDECHAR *)in;
	*inused = 4;

	if (__wchar_forbitten(*out) != 0)
		return (EILSEQ);

	return (0);
}

int
ucs4_wctomb(struct conv_state *cs, WIDECHAR in, char *out, size_t outsize,
    size_t *outused)
{
	if (outsize < 4)
		return (E2BIG);

	*(WIDECHAR *)out = in;
	*outused = 4;
	return (0);
}

int
ucs4le_mbtowc(struct conv_state *cs, const char *in, size_t insize,
    WIDECHAR *out, size_t *inused)
{
	if (insize < 4)
		return (EINVAL);

	/*
	 * Nothing special with BOM. As long as endian is specified we treat it
	 * as "ZERO WIDTH NO-BREAK SPACE".
 	 */

	*out = le32toh(*(WIDECHAR *)in);
	*inused = 4;

	if (__wchar_forbitten(*out) != 0)
		return (EILSEQ);

	return (0);
}

int
ucs4le_wctomb(struct conv_state *cs, WIDECHAR in, char *out, size_t outsize,
    size_t *outused)
{
	if (outsize < 4)
		return (E2BIG);

	*(WIDECHAR *)out = htole32(in);
	*outused = 4;
	return (0);
}

int
ucs4be_mbtowc(struct conv_state *cs, const char *in, size_t insize,
    WIDECHAR *out, size_t *inused)
{
	if (insize < 4)
		return (EINVAL);

	/*
	 * Nothing special with BOM. As long as endian is specified we treat it
	 * as "ZERO WIDTH NO-BREAK SPACE".
 	 */

	*out = be32toh(*(WIDECHAR *)in);
	*inused = 4;

	if (__wchar_forbitten(*out) != 0)
		return (EILSEQ);

	return (0);
}

int
ucs4be_wctomb(struct conv_state *cs, WIDECHAR in, char *out, size_t outsize,
    size_t *outused)
{
	if (outsize < 4)
		return (E2BIG);

	*(WIDECHAR *)out = htobe32(in);
	*outused = 4;
	return (0);
}

int
ucs2_mbtowc(struct conv_state *cs, const char *in, size_t insize, WIDECHAR *out,
    size_t *inused)
{
	if (insize < 2)
		return (EINVAL);

	if (cs->fbo == ICONV_BO_UNDEF) {
		/*
 		 * Determine byte order. If we get BOM, we switch mbtowc handler
 		 * to the exact big/little mbtowc implementation. Otherwise
 		 * we use host byte order.
 		 */
		if (*(u_char *)in == 0xff && *((u_char *)in + 1) == 0xfe)
			cs->mbtowc = ucs2le_mbtowc;
		else if (*(u_char *)in == 0xfe && *((u_char *)in + 1) == 0xff)
			cs->mbtowc = ucs2be_mbtowc;
		else
			cs->fbo = ICONV_BO_HOST;

		if (cs->fbo != ICONV_BO_HOST) {
			/*
			 * We need to restart mbtowc because it might have been
			 * changed.
			 */
			*inused = 2;
			return (EAGAIN);
		}
	}

	*out = *(u_short *)in;
	*inused = 2;

	if (__wchar_forbitten(*out) != 0)
		return (EILSEQ);

	return (0);
}

int
ucs2_wctomb(struct conv_state *cs, WIDECHAR in, char *out, size_t outsize,
    size_t *outused)
{
	if (outsize < 2)
		return (E2BIG);

	if (in > USHRT_MAX || in < 0)
		return (EILSEQ);

	*(u_short *)out = in;
	*outused = 2;
	return (0);
}

int
ucs2le_mbtowc(struct conv_state *cs, const char *in, size_t insize,
    WIDECHAR *out, size_t *inused)
{
	if (insize < 2)
		return (EINVAL);

	/*
	 * Nothing special with BOM. As long as endian is specified we treat it
	 * as "ZERO WIDTH NO-BREAK SPACE".
 	 */

	*out = le16toh(*(u_short *)in);
	*inused = 2;

	if (__wchar_forbitten(*out) != 0)
		return (EILSEQ);

	return (0);
}

int
ucs2le_wctomb(struct conv_state *cs, WIDECHAR in, char *out, size_t outsize,
    size_t *outused)
{
	if (outsize < 2)
		return (E2BIG);

	if (in > USHRT_MAX || in < 0)
		return (EILSEQ);

	*(u_short *)out = htole16((u_short)in);
	*outused = 2;
	return (0);
}

int
ucs2be_mbtowc(struct conv_state *cs, const char *in, size_t insize,
    WIDECHAR *out, size_t *inused)
{
	if (insize < 2)
		return (EINVAL);

	/*
	 * Nothing special with BOM. As long as endian is specified we treat it
	 * as "ZERO WIDTH NO-BREAK SPACE".
 	 */

	*out = be16toh(*(u_short *)in);
	*inused = 2;

	if (__wchar_forbitten(*out) != 0)
		return (EILSEQ);

	return (0);
}

int
ucs2be_wctomb(struct conv_state *cs, WIDECHAR in, char *out, size_t outsize,
    size_t *outused)
{
	if (outsize < 2)
		return (E2BIG);

	if (in > USHRT_MAX || in < 0)
		return (EILSEQ);

	*(u_short *)out = htobe16((u_short)in);
	*outused = 2;
	return (0);
}
