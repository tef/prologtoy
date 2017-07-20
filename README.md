
```
$ swipl
Welcome to SWI-Prolog (threaded, 64 bits, version 7.4.2)
SWI-Prolog comes with ABSOLUTELY NO WARRANTY. This is free software.
Please run ?- license. for legal details.

For online help and background, visit http://www.swi-prolog.org
For built-in help, use ?- help(Topic). or ?- apropos(Word).

?- [toy].
true.

?- eval_list([[define,square,[lambda,[x],[mul,x,x]]],[square,4]],X).

X = [t, 16] 

Yes

[debug]  ?- eval_list([[square,8]],X).
X = [64] .

```
