-module(db_must_compile_1).

-compile({parse_transform, tq_record_transform}).

-field({rw,
        [
         {type, integer},
         {mode, rw},
         {record, false},
         {get, false},
         {set, false}
        ]}).
