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