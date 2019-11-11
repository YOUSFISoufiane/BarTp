name := "BarTp"

version := "0.1"
lazy val akkaVersion = "2.5.26"
libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-actor" % akkaVersion
)
scalaVersion := "2.13.1"
