; from return's perspective, (unquote 'v) is simply data, so it just returns the symnbol
(let
    (assign 'v (* '6 '7))
    (return (unquote 'v))
)
