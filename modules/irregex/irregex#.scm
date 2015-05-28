(##namespace (""

   ;;; Data structures
   
   irregex-tag
   make-irregex
   irregex?
   irregex-dfa
   irregex-dfa/search
   irregex-dfa/extract
   irregex-nfa
   irregex-flags
   irregex-num-submatches
   irregex-length
   irregex-names
   irregex-new-matches
   irregex-reset-matches!
   irregex-copy-matches
   irregex-match-tag
   irregex-match-data?
   make-irregex-match
   irregex-match-num-submatches
   irregex-match-chunker
   irregex-match-names
   irregec-match-chuncker-set!
   %irregex-match-start-chunk
   %irregex-match-start-index
   %irregex-match-end-chunk
   %irregex-match-end-index
   %irregex-match-fail
   %irregex-match-fail-set!
   irregex-match-start-chunk
   irregex-match-start-index
   irregex-match-end-chunk
   irregex-match-end-index
   irregex-match-start-chunk-set!
   irregex-match-start-index-set!
   irregex-match-end-chunk-set!
   irregex-match-end-index-set!
   irregex-match-index
   %irregex-match-valid-index?
   irregex-match-valid-index?
   irregex-match-substring
   irregex-match-subchunk
   make-irregex-chunker
   chunker-get-next
   chunker-get-str
   chunker-get-start
   chunker-get-end
   chunker-get-substring
   chunker-get-subchunk
   chunker-prev-chunk
   chunker-prev-char
   chunker-next-char
   chunk-before?
   
   ;;; String utilities

   *all-chars*
   *allow-utf8-mode?*
   string-scan-char
   string-scan-char-escape
   string-scan-pred
   string-split-char
   char-alphanumeric?
   %substring=?
   %%string-copy!
   string-cat-reverse
   string-cat-reverse/aux
   
   ;;; List utilities

   zero-to
   take-up-to
   find
   find-tail
   last
   any
   every
   fold
   filter
   remove

   ;;; Flags

   bit-shr
   bit-shl
   bit-not
   bit-ior
   bit-and
   integer-log
   flag-set?
   flag-join
   flag-clear
   ~none
   ~searcher?
   ~consumer?

   ;;;

   close-token
   dot-token
   with-read-from-string
   ~save?
   ~case-insensitive?
   ~multi-line?
   ~single-line?
   ~ignore-space?
   ~utf8?
   symbol-list->flags
   maybe-string->sre
   string->sre
   posix-escape-sequences
   char-altcase
   char-mirror
   string-parse-hex-escape
   string-parse-cset

   ;;; UTF-8
   
   high-char?
   utf8-start-char->length
   utf8-string-ref
   utf8-backup-to-initial-char
   utf8-lowest-digit-of-length
   utf8-highest-digit-of-length
   char->utf8-list
   unicode-range->utf8-pattern
   unicode-range-helper
   unicode-range-up-from
   unicode-range-up-to
   unicode-range-climb-digits
   unicode-range-middle
   cset->utf8-pattern
   sre-adjust-utf8

   ;;; Compilation

   irregex
   string->irregex
   sre->irregex
   sre-empty?
   sre-any?
   sre-repeater?
   sre-searcher?
   sre-consumer?
   sre-has-submatches?
   sre-count-submatches
   sre-length-ranges

   ;;; SRE manipulation

   sre-sequence
   sre-alternate
   sre-strip-submatches
   sre-flatten-ranges
   sre-names
   sre-sequence-names
   sre-remove-initial-bos

   ;;; Basic matching

   irregex-basic-string-chunker
   irregex-search
   irregex-search/chunked
   irregex-search/matches
   irregex-search/backtrack
   irregex-match
   irregex-match/chunked
   irregex-match?

   ;;; DFA matching

   dfa-init-state
   dfa-next-state
   dfa-final-state?
   dfa-match/shortest
   dfa-match/longest

   ;;; Named definition
   
   sre-named-definitions

   ;;; SRE->NFA compilation

   *nfa-presize*
   *nfa-num-fields*
   nfa-num-states
   nfa-start-state
   nfa-get-state-trans
   nfa-set-state-trans!
   nfa-push-state-trans!
   nfa-get-epsilons
   nfa-set-epsilons!
   nfa-add-epsilon!
   nfa-get-state-closure
   nfa-set-state-closure!
   nfa-get-closure
   nfa-add-closure!
   sre->nfa

   ;;;

   nfa-multi-state-hash
   make-nfa-multi-state
   nfa-state->multi-state
   nfa-multi-state-copy
   nfa-multi-state-contains?
   nfa-multi-state-add!
   nfa-multi-state-add
   nfa-multi-state-union!
   nfa-multi-state-union
   nfa-multi-state-fold

   ;;; NFA->DFA compilation

   nfa->dfa
   dfa-renumber
   nfa-state-transitions
   nfa-join-transitions!
   char-range
   split-char-range
   intersect-char-ranges
   nfa-cache-state-closure!
   nfa-state-closure-internal
   nfa-closure-internal
   nfa-closure

   ;;; Match extraction

   sre-match-extractor

   ;;; Closure compilation

   sre->procedure

   ;;; Character set

   sre-cset->procedure
   plist->alist
   alist->plist
   sre->cset
   cset-contains?
   cset-range
   char-ranges-overlap?
   char-ranges-union
   cset-union
   cset-difference
   cset-intersection
   cset-complement
   cset-case-insensitive

   ;;; Match and Replace utilities
   
   irregex-fold/fast
   irregex-fold
   irregex-fold/chunked/fast
   irregex-fold/chunked
   irregex-replace
   irregex-replace/all
   irregex-apply-match
   irregex-extract
   irregex-split))

