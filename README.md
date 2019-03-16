# Bencode encoder / decoder for Emacs Lisp

This package provides a strict and robust [bencode][bencode] encoder
and decoder. Encoding is done precisely, carefully taking into account
character encoding issues. As such, the encoder always returns unibyte
data. When encoding strings and keys, UTF-8 is used by default for
both encoding and decoding, but this is configurable.

Neither the encoder nor decoder is recursive, so it's safe to parse
very deeply nested inputs.

```el
(bencode-encode OBJECT)
;; Return a unibyte string encoding OBJECT with bencode.

(bencode-encode-to-buffer OBJECT)
;; Like `bencode-encode' but to the current buffer at point.

(bencode-decode STRING)
;; Decode bencode data from STRING.

(bencode-decode-from-buffer)
;; Like `bencode-decode' but from the current buffer starting at point.

bencode-dictionary-type [variable]
;; Selects the dictionary representation when decoding. (:plist, :hash-table)

bencode-list-type [variable]
;; Selects the list representation when decoding. (:list, :vector)
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

(let ((bencode-dictionary-type :hash-table)
      (bencode-list-type :vector))
  (bencode-decode "d3:barl5:alpha4:betae3:fooli1ei2ei3eee"))
;; => #s(hash-table test equal data ("foo" [1 2 3] "bar" ["alpha" "beta"]))
```

## Character encoding

Why is character encoding important? Bencode strings are bytestrings,
and it's not possible to choose string encoding later. For example:

```el
(bencode-encode "naïvety")
;; => "8:na\xc3\xafvety"  (default UTF-8 encoding)

(let ((coding-system-for-write 'latin-1))
  (bencode-encode "naïvety"))
;; => "7:na\xefvety"  (using ISO 8859-1)
```

String encoding depends on the character encoding, which may even
affect dictionary key ordering.


[bencode]: https://en.wikipedia.org/wiki/Bencode
