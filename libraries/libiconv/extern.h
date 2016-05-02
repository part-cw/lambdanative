#include <sys/types.h>

#include "defs.h"

int	cp1251_mbtowc(struct conv_state *cs, const char *in, size_t insize,
	    WIDECHAR *out, size_t *inused);
int	cp1251_wctomb(struct conv_state *cs, WIDECHAR in, char *out,
	    size_t outsize, size_t *outused);

int	cp866_mbtowc(struct conv_state *cs, const char *in, size_t insize,
	    WIDECHAR *out, size_t *inused);
int	cp866_wctomb(struct conv_state *cs, WIDECHAR in, char *out,
	    size_t outsize, size_t *outused);

int	iso8859_5_mbtowc(struct conv_state *cs, const char *in, size_t insize,
	    WIDECHAR *out, size_t *inused);
int	iso8859_5_wctomb(struct conv_state *cs, WIDECHAR in, char *out,
	    size_t outsize, size_t *outused);

int	koi8r_mbtowc(struct conv_state *cs, const char *in, size_t insize,
	    WIDECHAR *out, size_t *inused);
int	koi8r_wctomb(struct conv_state *cs, WIDECHAR in, char *out,
	    size_t outsize, size_t *outused);

int	koi8u_mbtowc(struct conv_state *cs, const char *in, size_t insize,
	    WIDECHAR *out, size_t *inused);
int	koi8u_wctomb(struct conv_state *cs, WIDECHAR in, char *out,
	    size_t outsize, size_t *outused);

int	mac_cyrillic_mbtowc(struct conv_state *cs, const char *in,
	    size_t insize, WIDECHAR *out, size_t *inused);
int	mac_cyrillic_wctomb(struct conv_state *cs, WIDECHAR in, char *out,
	    size_t outsize, size_t *outused);

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
