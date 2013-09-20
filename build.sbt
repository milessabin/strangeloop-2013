scalaVersion := "2.10.2"

name := "strangeloop"

resolvers ++= Seq(
  Classpaths.typesafeSnapshots,
  "snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/"
)

// Shapeless
libraryDependencies ++= Seq(
  "org.scalaz"  %% "scalaz-core" % "7.0.3",
  "com.chuusai" %  "shapeless"   % "2.0.0-M1" cross CrossVersion.full
)

//scalacOptions ++= Seq("-feature", "-Xlog-implicits")
scalacOptions ++= Seq("-feature")

initialCommands in console := "import shapeless._, strangeloop._"
