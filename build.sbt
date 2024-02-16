val root = project
  .in(file("."))
  .settings(
    scalaVersion := "3.3.1",
    organization := "dev.vgerasimov",
    name := "md4s",
    version := "0.1.0",
    githubOwner := "wlad031",
    githubRepository := "md4s",
    scalacOptions ++= Seq(
      "-rewrite",
      "-source", "future"
    ),
    libraryDependencies ++= {
      val munitVersion = "0.7.29"
      Seq(
        "org.scalameta" %% "munit"            % munitVersion % Test,
        "org.scalameta" %% "munit-scalacheck" % munitVersion % Test
      )
    },
  )
  