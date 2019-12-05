Config Format
=============

The config has a very simple format, using [TypeSafe Config](https://github.com/lightbend/config)

Example
-------
```HOCON
databases {
  postgres {
    username: frank
    password:1234
    jdbc-url: "jdbc:postgresql://localhost:5432/dev"
    driver: "org.postgresql.Driver"
  }
  mysql {
    username: frank
    password:1234
    jdbc-url: jdbc:mysql://localhost:3306/dev
    driver: "com.mysql.cj.jdbc.Driver"
  }
}
```