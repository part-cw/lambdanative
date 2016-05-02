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
#include <stdlib.h>
#include <string.h>

#include "iconv.h"
#include "defs.h"
#include "tbl.h"

iconv_t iconv_open(const char *to, const char *from)
//iconv_t iconv_open(char *to, char *from)
{
	struct conv_state *cd, ocd;
	struct conv_tbl *tbl;
	char *xto, *xfrom, *p;

	ocd.fbo = ocd.tbo = ICONV_BO_UNDEF;
	ocd.flags = 0;
	ocd.mbtowc = NULL;
	ocd.wctomb = NULL;

	if (from == NULL || *from == '\0') {
		/* XXX - must be fixed to current locale */
		from = "us-ascii";
	}
	xfrom = strdup(from);
	if (xfrom == NULL) {
		errno = ENOMEM;
		return ((iconv_t)-1);
	}

	if (to == NULL || *to == '\0') {
		/* XXX - must be fixed to current locale */
		to = "us-ascii";
	}
	xto = strdup(to);
	if (xto == NULL) {
		free(xfrom);
		errno = ENOMEM;
		return ((iconv_t)-1);
	}

	p = strstr(xto, "//");
	if (p != NULL) {
		if (strcmp(p + 2, "IGNORE") == 0)
			ocd.flags = ICONV_IGNORE_TO;
		*p = '\0';	/* other modificators are not supported */
	}

	tbl = iconv_conv_tbl_search(xfrom);
	if (tbl != NULL) {
		ocd.mbtowc = tbl->mbtowc;
		tbl = iconv_conv_tbl_search(xto);
		if (tbl != NULL)
			ocd.wctomb = tbl->wctomb;
	}

	free(xto);
	free(xfrom);

	if (ocd.mbtowc == NULL || ocd.wctomb == NULL) {
		errno = EINVAL;
		return ((iconv_t)-1);
	}

	cd = (struct conv_state *)malloc(sizeof(*cd));
	if (cd == NULL) {
		errno = ENOMEM;
		return ((iconv_t)-1);
	}

	memcpy(cd, &ocd, sizeof(*cd));
	errno = 0;
	return (cd);
}

int
iconv_close(iconv_t cd)
{

	free(cd);
	return (0);
}

//size_t iconv(iconv_t cd, const char **inbuf, size_t *inbytes, char **outbuf, size_t *outbytes)
size_t iconv(iconv_t cd, char **inbuf, size_t *inbytes, char **outbuf, size_t *outbytes)
{
	struct conv_state *cs;
	WIDECHAR wch;
	size_t inused, outused;
	int error;

	cs = (struct conv_state *)cd;

	if (inbuf != NULL && *inbuf != NULL && outbuf != NULL &&
	    *outbuf != NULL) {
		while (*inbytes > 0) {
			inused = 0;
			error = cs->mbtowc(cs, *inbuf, *inbytes, &wch, &inused);
			if (error == EILSEQ) {
				if ((cs->flags &ICONV_IGNORE_TO) == 0) {
					errno = EILSEQ;
					return ((size_t)-1);
				}
				inused = 1;	/* XXX - skip one byte */
			} else if (error != 0 && error != EAGAIN) {
				errno = error;
				return ((size_t)-1);
			} else if (error == 0 && *outbytes == 0) {
				errno = E2BIG;
				return ((size_t)-1);
			}

			*inbytes -= inused;
			*inbuf += inused;

			if (error == EAGAIN || error == EILSEQ)
				continue;

			outused = 0;
			error = cs->wctomb(cs, wch, *outbuf, *outbytes,
			    &outused);
			if (error == 0) {
				*outbytes -= outused;
				*outbuf += outused;
			} else if (error != EILSEQ ||
			    (cs->flags & ICONV_IGNORE_TO) == 0) {
				errno = error;
				return ((size_t)-1);
			}
		}

		errno = 0;
		return (0);
	} else if (inbuf == NULL || *inbuf == NULL) {
		cs->fbo = cs->tbo = ICONV_BO_UNDEF;
		errno = 0;
		return (0);
	}

	errno = EFAULT;
	return ((size_t)-1);
}
