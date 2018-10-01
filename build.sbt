val dottyVersion = "0.10.0-bin-20180925-561c06a-NIGHTLY"

lazy val root = project
  .in(file("."))
  .settings(
    name := "dotty-staging-gpce-2018",
    version := "0.1.0",

    scalaVersion := dottyVersion,

    libraryDependencies ++= Seq(
      "ch.epfl.lamp" % "dotty_0.10" % dottyVersion,
      "ch.epfl.lamp" % "dotty_0.10" % dottyVersion % "test->runtime",
      "com.novocode" % "junit-interface" % "0.11" % "test"
    )
  )

lazy val bench = project
  .in(file("bench"))
  .dependsOn(root)
  .settings(
    name := "dotty-staging-gpce-2018-bench",
    version := "0.1.0",

    scalaVersion := dottyVersion,

    libraryDependencies ++= Seq(
      "ch.epfl.lamp" % "dotty_0.10" % dottyVersion,
      "ch.epfl.lamp" % "dotty_0.10" % dottyVersion % "test->runtime"
    ),

    javaOptions ++= Seq("-Xms6g", "-Xmx6g", "-Xss4m",
			   "-XX:+CMSClassUnloadingEnabled",
			   "-XX:ReservedCodeCacheSize=256m",
			   "-XX:-TieredCompilation", "-XX:+UseNUMA"
    )
  ).enablePlugins(JmhPlugin)
