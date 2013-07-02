-record(access_mode,{
		  r = true :: boolean(),
		  sr = true :: boolean(),
		  w = true :: boolean(),
		  sw = true :: boolean()
		 }).

-record(record_options,{
		  type = any :: field_type(), % Record type. Usefull for dializer
		  type_constructor :: atom() | {atom(), atom()},
		  default_value :: any()
		 }).

-record(field,{
		  name :: binary(), % Field name which will be used to access property

		  is_index = false :: boolean(),
		  is_required = false :: boolean(),

		  mode = #access_mode{} :: #access_mode{}, % From 'r | w | rw | sr | sw | srsw | rsw | srw'
		  getter = true :: true | false, % create getter
		  setter = true :: true | false, % create setter
		  init = false :: true | false, % Init function required

		  stores_in_record = true :: boolean(), % Set to true if field value stores in state record
		  record_options = #record_options{} :: #record_options{} % Editional record options
		 }).

-record(model, {
		  module :: atom(),
		  fields = [] :: [#field{}],
		  init_fun :: atom() | {atom(), atom()}
		 }).

-type field_type() :: {atom(),atom()} | integer | non_neg_integer | binary.
