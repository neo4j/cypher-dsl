This module provides a Java DSL for the Cypher query language. Using the DSL you can either build up an expression that can be stringified, or you can access the internal query model.

There are two main modes: using static methods or Java instance initialization blocks.

Example usage with statics:
```java
assertEquals( "START n=node(1) RETURN n",
              start( node( "n", 1 ) ).returns( nodes( "n" ) ).toString() );
```

Example usage with Java instance initialization block:
```java
assertEquals( "START john=node(0) RETURN john", new CypherQuery()
{{
    starts( node( "john", 0 ) ).returns( nodes( "john" ) );
}}.toString() );
```

Once you have the constructed query string this can be sent to the Cypher execution engine.
Example:
```java
Execute q = start(node("john", john)).
            match(path().from("john").out("friend").link().out("friend").to("fof")).
            returns(properties("john.name", "fof.name"));
System.out.println( engine.execute( q.toString() ).toString() );
```
