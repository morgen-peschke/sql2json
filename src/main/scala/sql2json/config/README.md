Config Format
=============

The config has a very simple format, so I could roll my own parser in an afternoon.

Example
-------
```
[postgres]
username: frank
password:1234
jdbc-url: jdbc:postgresql://localhost:5432/dev

[mysql]
username: frank
password:1234
jdbc-url: jdbc:postgresql://localhost:3306/dev
```

Grammar
-------
```
config-file
  : block ( "\n" empty-line "\n" block )* 
  ;

block 
  : section "\n" setting ( "\n" setting )* 
  ;

empty-line 
  : " "* 
  ;

section     
  : "[" section-name "]"
  ;

section-name 
  : (section-char - "-") section-chars*
  ;

section-char
  : non-whitespace - bracket-chars
  ;

bracket-chars
  : "[" | "]"
  ;

setting          
  : user-setting 
  | password-setting 
  | jdbc-url-setting
  ;

user-setting     
  : "username:" space* non-empty-string
  ;

password-setting 
  : "password:" space* non-empty-string
  ;

jdbc-url-setting 
  : "jdbc-url:" space* "jdbc:" non-empty-string
  ;

non-empty-string 
  : non-whitespace+
  ;
non-whitespace
  : \S
  ;
space 
  : \s
  ;
```