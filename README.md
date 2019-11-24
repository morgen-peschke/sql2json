# sql2json

Bulk loads and unloads are a bit of a pain, as most databases have clients which only emit CSV so 
delimiter choices are a really big headache.

This project aims to allow emitting these same records as [JSON Lines](http://jsonlines.org/), either 
as a series of JSON arrays or JSON objects.

Full disclosure: this is also a chance to play around a bit with Dotty