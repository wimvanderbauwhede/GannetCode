; a way of storing expressions for later evaluation
; the quoted labed actually only quotes the expression at the position of the label
; so the label must be quoted to use call. But then, is there a good reason
; for using call? We can simply use the unquoted label.
(begin
    '(label a (* '6 '7))
     (call 'a)
)
