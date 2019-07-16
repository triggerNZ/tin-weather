scalaVersion := "2.12.8"

name := "cba-weather"

libraryDependencies ++= Seq(
  "org.scalaz" %% "scalaz-core" % "7.2.28", // For typeclasses,
  "org.scalaz" %% "scalaz-zio" % "1.0-RC5", // For functional IO
  "com.lihaoyi" %% "utest" % "0.7.1" % Test, // For testing
  "com.github.japgolly.nyaya" %% "nyaya-test" % "0.9.0-RC1" % Test // For property tests
)

testFrameworks += new TestFramework("utest.runner.Framework")

fork in run in Test := true