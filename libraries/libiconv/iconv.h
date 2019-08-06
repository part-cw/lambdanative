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

#ifndef _LIBICONV_ICONV_H_
#define _LIBICONV_ICONV_H_

#include <sys/types.h>

typedef void * iconv_t;

#ifdef _WIN32
# ifndef __BEGIN_DECLS
#  ifdef	__cplusplus
#   define __BEGIN_DECLS	extern "C" {
#   define __END_DECLS }
#  else
#   define __BEGIN_DECLS
#   define __END_DECLS
#  endif
# endif

#endif

__BEGIN_DECLS
iconv_t	iconv_open(const char *to, const char *from);
//iconv_t	iconv_open(char *to, char *from);
int	iconv_close(iconv_t cd);
//size_t	iconv(iconv_t cd, const char **inbuf, size_t *inbytes, char **outbuf, size_t *outbytes);
size_t	iconv(iconv_t cd, char **inbuf, size_t *inbytes, char **outbuf, size_t *outbytes);
__END_DECLS

#endif /* !_LIBICONV_ICONV_H_ */
