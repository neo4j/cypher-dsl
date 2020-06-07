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
ExecutionResult result = engine.execute( q.toString() ).toString();
```

QueryDSL integration
====================
It is possible to use the QueryDSL library and predicates with this DSL. Here's an example:

```java
QPerson person = QPerson.person;
Assert.assertEquals( "START person=node(1,2,3) WHERE person.firstName=\"P\" and person.age>25 RETURN person",
                     CypherQueryDSL.start( node( "person", 1, 2, 3 ) )
                         .where( person.firstName.eq( "P" ).and( person.age.gt( 25 ) ) )
                         .returns( nodes( "person" ) )
                         .toString() );
```

Apart from using QueryDSL for the where-expression, it is also possible to create the Lucene index lookups using QueryDSL.

```java
QPerson person = QPerson.person;
Assert.assertEquals( "START person=node:node_auto_index(\"firstName:rickard\") RETURN person",
                     CypherQueryDSL.start( LuceneStartExpression.query("person", "node_auto_index", person.firstName.eq("Rickard")) )
                         .returns( nodes( "person" ) )
                         .toString() );
```


See the POM and tests of this module for further examples and how to use these features.
