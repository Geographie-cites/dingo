val scala3Version = "3.3.1"
val molaVersion = "0.1.0"


lazy val model = project
  .in(file("model"))
  .settings(
    name := "model",
    version := molaVersion,

    scalaVersion := scala3Version,

    libraryDependencies += "com.github.scopt" %% "scopt" % "4.0.1" cross(CrossVersion.for3Use2_13),
    libraryDependencies += "com.github.pathikrit" %% "better-files" % "3.9.1" cross(CrossVersion.for3Use2_13),
    libraryDependencies += "org.apache.commons" % "commons-math3" % "3.6.1",

    libraryDependencies ++= Seq(
      "dev.optics" %% "monocle-core"  % "3.2.0",
      "dev.optics" %% "monocle-macro" % "3.2.0",
    ),

    //resolvers += "osgeo" at "https://repo.osgeo.org/repository/release/",
    //libraryDependencies += "org.geotools" % "gt-shapefile" % "29.2" exclude("javax.media", "jai_core"),
    libraryDependencies += "com.outr" %% "scribe" % "3.12.2",
    //libraryDependencies += "org.virtuslab" %% "scala-yaml" % "0.0.8"
    libraryDependencies += "io.circe" %% "circe-yaml" % "0.14.2",
    libraryDependencies ++= Seq(
      "io.circe" %% "circe-core",
      "io.circe" %% "circe-generic",
      "io.circe" %% "circe-parser",
      //"io.circe" %% "circe-generic-extras"
    ).map(_ % "0.14.6"),

    libraryDependencies += "org.openmole" %% "byte-pack" % "0.1"
    //containerBuildImage := Some("ghcr.io/graalvm/jdk-community:21")

  )

lazy val input = project
  .in(file("input"))
  .settings(
      name := "matrix",
      version := molaVersion,
      scalaVersion := scala3Version,
      libraryDependencies += "com.google.code.externalsortinginjava" % "externalsortinginjava" % "0.6.1"
  ) dependsOn(model)

