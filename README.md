# Bencode encoder / decoder for Emacs Lisp

This package provides a strict and robust [bencode][bencode] encoder
and decoder. Encoding is precise, taking into account character
encoding issues. As such, the encoder always returns unibyte data
intended to be written out as raw binary data without additional
character encoding. When encoding strings and keys, UTF-8 is used by
default. The decoder strictly valides its input, rejecting invalid
inputs.

Neither the encoder nor decoder is recursive, so it's safe to parse
very deeply nested inputs.

```el
(bencode-encode OBJECT &key (CODING-SYSTEM 'utf-8))
;; Return a unibyte string encoding OBJECT with bencode.

(bencode-encode-to-buffer OBJECT &key (CODING-SYSTEM 'utf-8))
;; Like `bencode-encode' but to the current buffer at point.

(bencode-decode STRING &key (CODING-SYSTEM 'utf-8)
                            (LIST-TYPE 'list)
                            (DICT-TYPE 'plist))
;; Decode bencode data from STRING.

(bencode-decode-from-buffer &key (CODING-SYSTEM 'utf-8)
                                 (LIST-TYPE 'list)
                                 (DICT-TYPE 'plist))
;; Like `bencode-decode' but from the current buffer starting at point.
```

## Examples

Encoding:

```el
(bencode-encode '(1 2 3 "alpha" "beta"))
;; => "li1ei2ei3e5:alpha4:betae"

(bencode-encode '(:foo (1 2 3) :bar ("alpha" "beta")))
;; => "d3:barl5:alpha4:betae3:fooli1ei2ei3eee"
```

Decoding:

```el
(bencode-decode "li1ei2ei3e5:alpha4:betae")
;; => (1 2 3 "alpha" "beta")

(bencode-decode "d3:barl5:alpha4:betae3:fooli1ei2ei3eee")
;; => (:bar ("alpha" "beta") :foo (1 2 3))

(bencode-decode "d3:barl5:alpha4:betae3:fooli1ei2ei3eee"
                :dict-type 'hash-table
                :list-type 'vector)
;; => #s(hash-table test equal data ("foo" [1 2 3] "bar" ["alpha" "beta"]))
```

Run the test suite with `make check` and benchmark with `make bench`.

## Character encoding

Why is character encoding so important? Bencoded data are byte
strings, not codepoint. It's generally not possible to decide string
encoding after encoding with bencode. For example:

```el
(bencode-encode "naïvety")
;; => "8:na\xc3\xafvety"  (default UTF-8 encoding)

(bencode-encode "naïvety" :coding-system 'latin-1)
;; => "7:na\xefvety"  (using ISO 8859-1)
```

String encoding depends on the character encoding, and may even affect
dictionary key ordering.


[bencode]: https://en.wikipedia.org/wiki/Bencode
