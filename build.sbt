//seq(slickSettings: _*)

//seq(oldLwjglSettings: _*)

version := "1.0.0-SNAPSHOT"

scalaVersion := "2.10.0"

name := "Smartender"

organization := "com.jr.intel.smartender"

//libraryDependencies += "javax.transaction" % "jta" % "1.1"

libraryDependencies += "org.scalamock" %% "scalamock-scalatest-support" % "3.0"// cross CrossVersion.full

libraryDependencies += "org.scalatest" %% "scalatest" % "2.0.M5b"// cross CrossVersion.full

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.10.1" % "test"

libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.0.0-M7"// cross CrossVersion.full

//libraryDependencies += "com.github.scala-incubator.io" %% "scala-io-core" % "0.4.1"

//libraryDependencies += "com.github.scala-incubator.io" %% "scala-io-file" % "0.4.1"

//libraryDependencies += "org.scalaj" % "scalaj-time_2.10.0-M7" % "0.6"

//javaOptions <<= resourceManaged map { r => Seq("-Djava.library.path=" + r + "/main/lwjgl-resources/osx/macosx") }

//seq(ScctPlugin.instrumentSettings : _*)

//scalacOptions ++= Seq("-feature","-deprecation","-language:postfixOps")

//addSbtPlugin("com.github.mpeltonen" % "sbt-idea" % "1.4.0")



