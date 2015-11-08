-define(CHILD(I),
    {I, {I, start_link, []}, permanent, 5000, worker, [I]}).

-define(CHILD(I, Type),
    {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

-define(CHILD(I, Args, Type),
    {I, {I, start_link, [Args]}, permanent, 5000, Type, [I]}).