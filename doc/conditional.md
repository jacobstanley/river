
```
if gt %flags then
  %n = 1; go
else
  %n = 0; go

==>

  setg %n
  jmp go
go:
  ...
```

```
if le %flags then
  %n = 27; go
else
  go

==>

  cmovleq $27, %n
  jmp go
go:
  ...
```
