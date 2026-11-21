;;; umbel.schema — record encoding and the table catalog.
;;
;; A schema is a vector of fields {:name :type :size}. Supported types:
;; :int32 :uint32 :int64 :uint64 :string (fixed width, zero padded).
;; Records are packed little endian with string.pack.
;;
;; The catalog encodes a schema onto a page so tables persist their own shape:
;;
;;   magic "UMBL" | version u16 | field-count u16
;;   then per field: type-code u8 | size u16 | name-len u8 | name bytes

(local page (require :umbel.page))
(local pack string.pack)
(local unpack string.unpack)
(local MAGIC :UMBL)

(local type-codes {:int32 1 :uint32 2 :int64 3 :uint64 4 :string 5})
(local code-types {})
(each [k v (pairs type-codes)] (tset code-types v k))
(local fixed-sizes {:int32 4 :uint32 4 :int64 8 :uint64 8})

(fn field-size [f]
  (or (. fixed-sizes f.type) f.size))

(fn record-size [schema]
  "Packed byte size of one record."
  (var n 0)
  (each [_ f (ipairs schema)]
    (set n (+ n (field-size f))))
  n)

(fn check-value [f v]
  (if (= f.type :string)
      (do
        (when (> (length v) f.size)
          (error (.. "schema: string value too long for field " f.name)))
        v)
      (do
        (when (not= (type v) :number)
          (error (.. "schema: field " f.name " expects a number")))
        v)))

(fn encode-field [f v]
  (case f.type
    :int32 (pack :<i4 v)
    :uint32 (pack :<I4 v)
    :int64 (pack :<i8 v)
    :uint64 (pack :<I8 v)
    :string (pack (.. :c f.size) v)))

(fn encode [schema rec]
  "Pack a table of {name = value} into record bytes."
  (var buf "")
  (each [_ f (ipairs schema)]
    (local v
           (or (. rec f.name) (error (.. "schema: missing value for field "
                                         f.name))))
    (set buf (.. buf (encode-field f (check-value f v)))))
  buf)

(fn decode-field [f buf pos]
  (case f.type
    :int32 (values (unpack :<i4 buf pos))
    :uint32 (values (unpack :<I4 buf pos))
    :int64 (values (unpack :<i8 buf pos))
    :uint64 (values (unpack :<I8 buf pos))
    :string (let [(raw next) (unpack (.. :c f.size) buf pos)
                  trimmed (raw:gsub "\000+$" "")]
              (values trimmed next))))

(fn decode [schema buf]
  "Unpack record bytes into {name = value}."
  (local out {})
  (var pos 1)
  (each [_ f (ipairs schema)]
    (let [(v next) (decode-field f buf pos)]
      (tset out f.name v)
      (set pos next)))
  out)

(fn catalog-to-page [schema]
  "Encode a schema into its catalog page."
  (var buf (.. MAGIC (pack :<I2I2 1 (length schema))))
  (each [_ f (ipairs schema)]
    (set buf (.. buf (pack :<BI2B
                           (or (. type-codes f.type)
                               (error (.. "schema: bad type " (tostring f.type))))
                           (field-size f) (length f.name))
                 f.name)))
  (.. buf (string.rep "\000" (- page.BLKSZ (length buf)))))

(fn catalog-from-page [data]
  "Decode a catalog page back into a schema vector."
  (when (not= (data:sub 1 4) MAGIC)
    (error "schema: not an umbel catalog page"))
  (let [(version cnt) (unpack :<I2I2 data 5)]
    (when (not= version 1)
      (error (.. "schema: unknown catalog version " version)))
    (local schema [])
    (var pos 9)
    (for [i 1 cnt]
      (let [(code size name-len next) (unpack :<BI2B data pos)
            name (data:sub next (- (+ next name-len) 1))
            f (case code
                5 {: name :type :string : size}
                _ (let [ty (or (. code-types code)
                               (error (.. "schema: bad field code " code)))]
                    {: name :type ty}))]
        (table.insert schema f)
        (set pos (+ next name-len))))
    schema))

{: type-codes
 : fixed-sizes
 : field-size
 : record-size
 : encode
 : decode
 : catalog-to-page
 : catalog-from-page}
