name := "rasshot"

version := "0.1"

scalaVersion := "2.11.2"

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

libraryDependencies ++= Seq(
	"org.twitter4j" % "twitter4j-core" % "4.0.2" ,
	"org.twitter4j" % "twitter4j-stream" % "4.0.2",
	"com.typesafe.akka" %% "akka-actor" % "2.3.6",
	"org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.2"
)
