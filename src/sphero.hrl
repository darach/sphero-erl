-ifndef(sphero_hrl).
-define(sphero_hrl,iontach).

-record(sphero_bt, {
    bt_device = undefined :: undefined | tuple(),
    bt_channel = undefined :: undefined | non_neg_integer(), 
    comm_port = undefined :: undefined | string(),
    comm_baud = 115200 :: pos_integer(),
    serial = undefined :: undefined | pid()
}).

-type sphero_bt() :: #sphero_bt{}.

-endif.
