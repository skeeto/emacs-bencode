# Bencode encoder / decoder for Emacs Lisp

This package provides a strict and robust [bencode][bencode] encoder
and decoder. Encoding is done precisely, carefully taking into account
character encoding issues. As such, the encoder always returns unibyte
data. When encoding strings and keys, UTF-8 is used by default for
both encoding and decoding, but this is configurable.

Neither the encoder nor decoder is recursive, so it's safe to parse
arbitrarily nested inputs.

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
