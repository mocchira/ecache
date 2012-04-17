-author('Yoshiyuki Kanno').

%% records.
-record(stats, {
    get_op   = 0 :: integer(),
    set_op   = 0 :: integer(),
    del_op   = 0 :: integer(),
    hit_cnt  = 0 :: integer(),
    rec_num  = 0 :: integer(),
    rec_size = 0 :: integer()
}).
