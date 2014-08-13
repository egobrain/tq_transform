0.3.3 Changelog
====

Features
---

- New ext_name option allows to set extarnal field name, that uses in *ext* functions.

Breaking Changes
---

- ```binary_key``` option renamed to ```ext_key```

0.3.2 Changelog
====

Breaking Changes
---

- case field is null all converters and validators for this field will be ignore.
- no more needed to handle nulls in from/to converters.
- required validators ignores 'undefined' atom.

0.3.1 Changelog
====

Features
---

- null's in from/to converters allowed

Breaking Changes
---

- null instead of undefined on empty model creation

0.3.0 Changelog
=====

Features:
---

 - Now you can set from/to type converters and validators via **converter_rules** option in parse_transmorm/4 call
	(see /priv/converter.map)

Breaking Changes
---
 - plugin callback ```create_model/1``` moved to ```create_model/2```
 - plugin callback ```normalize_fields/1``` moved to ```normalize_fields/2```
