# sql2json

Bulk loads and unloads are a bit of a pain, as most databases have clients which only emit CSV so 
delimiter choices are a really big headache.

This project aims to allow emitting these same records as [JSON Lines](http://jsonlines.org/), either 
as a series of JSON arrays or JSON objects.

Full disclosure: this is also an excuse to play around a bit with Dotty

# Usage

Currently, this needs to be run via `sbt`, and invocation may look something like this:
```bash
$ sbt -Dconfig.file=test.conf -warn 'run -d postgres' <<< 'select * from testing limit 5'
[0,"aa",true]
[1,"ab",false]
[2,"ac",true]
[3,"ad",false]
[4,"ae",true]
```

Two variants of headers may also be emitted, depending on the settings:
```bash
$ sbt -Dconfig.file=test.conf -warn 'run -d postgres -h' <<< 'select * from testing limit 5'
["id","text","flag"]
[0,"aa",true]
[1,"ab",false]
[2,"ac",true]
[3,"ad",false]
[4,"ae",true]
$ sbt -Dconfig.file=test.conf -warn 'run -d postgres -h -v' <<< 'select * from testing limit 5'
[{"label": "id","type": "int4"},{"label": "text","type": "varchar"},{"label": "flag","type": "bool"}]
[0,"aa",true]
[1,"ab",false]
[2,"ac",true]
[3,"ad",false]
[4,"ae",true]
```

It's also possible to emit the rows as objects:
```bash
$ sbt -Dconfig.file=test.conf -warn 'run -d postgres -f object' <<< 'select * from testing limit 5'
{"id": 0,"text": "aa","flag": true}
{"id": 1,"text": "ab","flag": false}
{"id": 2,"text": "ac","flag": true}
{"id": 3,"text": "ad","flag": false}
{"id": 4,"text": "ae","flag": true}
```

## Current Help Text
```
Usage: sql2json [options]

 Runs a SQL expression and returns it as lines of JSON.

 The query is read from standard input. Because I was not about to write anything close to a SQL
 parser, if multiple statements are needed, they must be separate by a line which contains exactly
 a single ';'.

 All but the last statement are assumed to be commands, and their result sets are ignored. The final
 statement is assumed to be a query, and it's result set is processed and returned. If the final
 statement is not a query, you are using the wrong tool.

 Configuration of credentials & dependecies is handled via HOCON config files.

 Options
   -d --database  name   Selects a database from the config file
                         If omitted, and the config only contains a single entry, it'll chose that.
                         Otherwise, you'll get an error.

   -f --format    fmt    Selects the output format, can be 'object' or 'array' (the default)

   -h --header           Enables emitting a leading header in 'array' format (disabled by default)

   -v --verbose          Enables extra information in the header, when enabled (disabled by default)

   --help                Print this help message, then exit
   ```