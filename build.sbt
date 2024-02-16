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
      "-source", "future",
      "-encoding", "utf8",
    ),
    testOptions += Tests.Argument(
      framework = Some(new TestFramework("munit.Framework")),
      args = List("-oSD")
    ),
    resolvers ++= Seq(
      "jitpack" at "https://jitpack.io",
      Resolver.githubPackages("wlad031"),
    ),
    libraryDependencies ++= {
      val munitVersion = "0.7.29"
      Seq(
        "dev.vgerasimov" %% "slowparse"        % "0.1.3",
        "com.lihaoyi"    %% "pprint"           % "0.7.0",
        "org.scalameta"  %% "munit"            % munitVersion % Test,
        "org.scalameta"  %% "munit-scalacheck" % munitVersion % Test
      )
    },
  )
 