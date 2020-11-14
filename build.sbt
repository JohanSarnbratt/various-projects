name := "trianglepeg"

version := "0.1"

scalaVersion := "2.13.2"

val akkaVersion = "2.6.8"

val akkaHttpVersion = "10.2.1"

libraryDependencies ++= Seq(
  "org.julienrf" %% "play-json-derived-codecs" % "7.0.0",
// akka streams
"com.typesafe.akka" %% "akka-stream" % akkaVersion,
"com.typesafe.akka" %% "akka-http" % akkaHttpVersion,
"com.typesafe.akka" %% "akka-http-spray-json" % akkaHttpVersion,
)