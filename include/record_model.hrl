-type hook() :: atom() | {atom(), atom()} | {atom(), list()} | {atom(), atom(), list()}.

-record(record_field,{
          name :: atom(), % Field name which will be used to access property
          ext_name :: binary(),

          is_required = false :: boolean(),

          mode = #access_mode{} :: #access_mode{}, % From 'r | w | rw | sr | sw | srsw | rsw | srw'
          getter = true :: true | false | custom, % create getter
          setter = true :: true | false | custom, % create setter

          stores_in_record = true :: boolean(), % Set to true if field value stores in state record
          type = binary :: field_type(), % Record type. Usefull for dializer
          to_ext :: hook(),
          from_ext :: hook(),
          default :: any(),

          validators = [] :: [hook()]
         }).

-record(record_model, {
          module :: atom(),
          fields = [] :: [#record_field{}],
          validators = [] :: [hook()],
          converter_rules :: [ {atom(), hook() | none, hook() | none, [hook()]} ]
         }).

-type field_type() :: {atom(), atom()} | integer | non_neg_integer | binary.
