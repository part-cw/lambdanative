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

#ifndef _LIBICONV_DEFS_H_
#define _LIBICONV_DEFS_H_

#include <sys/types.h>

/*
 * Internally we use our own UNICODE character to be independent on
 * size of ``wchar_t''.
 */
typedef int32_t WIDECHAR;

struct conv_state;

/*
 * These functions process one wide character at a time.
 * in, out, inused, outret can't be NULL. insize can't be zero.
 * Length of multibyte string must be specified in bytes.
 *
 * mbtowc must return the following error codes:
 *	0			- OK
 *	EAGAIN			- mbtowc should be restarted
 *	EINVAL			- incomplete multibyte sequence
 *	EILSEQ			- illegal symbol
 * *inused value is honored for 0, EAGAIN error codes.
 *
 * wctomb must return the following error codes:
 *	0			- OK
 *	E2BIG			- not enough space in out buffer
 *	EILSEQ			- illegal symbol
 * *outused value is honored for 0 error code.
 */
typedef int (*conv_mbtowc)(struct conv_state *cs, const char *in, size_t insize,
	    WIDECHAR *out, size_t *inused);
typedef int (*conv_wctomb)(struct conv_state *cs, WIDECHAR in, char *out,
	    size_t outsize, size_t *outused);

#define ICONV_IGNORE_TO		1

/*
 * Convertion context. Pointer to it - iconv_t. It's allocated by iconv_open()
 * and must be closed by iconv_close().
 */
struct conv_state {
	conv_mbtowc	mbtowc;
	conv_wctomb	wctomb;
	int		flags;
	int		fbo;	/* "from" byte order */
	int		tbo;	/* "to" byte order */
#define ICONV_BO_UNDEF		-1
#define ICONV_BO_HOST		0
#define ICONV_BO_BIG		1
#define ICONV_BO_LITTLE		2
};

/*
 * Convertor descriptor.
 */
struct conv_tbl {
	conv_mbtowc	mbtowc;
	conv_wctomb	wctomb;
};

#endif /* !_LIBICONV_DEFS_H_ */
