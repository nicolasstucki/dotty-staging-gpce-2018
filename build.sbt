val dottyVersion = "0.9.0-RC1"

lazy val root = project
  .in(file("."))
  .settings(
    name := "dotty-staging-gpce-2018",
    version := "0.1.0",

    scalaVersion := dottyVersion,

    libraryDependencies ++= Seq(
      "ch.epfl.lamp" % "dotty_0.9" % dottyVersion,
      "ch.epfl.lamp" % "dotty_0.9" % dottyVersion % "test->runtime",
      "com.novocode" % "junit-interface" % "0.11" % "test"
    ),

    javaOptions ++= Seq("-Xms6g", "-Xmx6g", "-Xss4m",
			   "-XX:+CMSClassUnloadingEnabled",
			   "-XX:ReservedCodeCacheSize=256m",
			   "-XX:-TieredCompilation", "-XX:+UseNUMA"
    )  
  ).enablePlugins(JmhPlugin)
