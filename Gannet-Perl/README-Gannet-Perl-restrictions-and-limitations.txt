Apart from the many Perl 5 features that are not (yet) implemented in Gannet-Perl, there are other features that Gannet-Perl will simply not support:

* Gannet-Perl assumes "strict", there is no support for "no strict"
* Gannet-Perl does not support some expressions that are valid in Perl 5
    - only the 3-argument version of open() is supported
    - bare filehandles, you must use a scalar
    - barewords in hashes
    - for to mean foreach
    - implicit iterators in for/foreach
    - no support for "local"
    - an implicit assignment to <...> in a while() creates a new lexical scope for $_, i.e. it is my $_ = <...>, not $_ = <...>
* an assignment in Gannet-Perl returns true or false, not the assigned value
* Gannet-Perl uses the PCRE library, not Perl's own regexes

