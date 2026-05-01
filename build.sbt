val root = project
  .in(file("."))
  .settings(
    scalaVersion := "3.8.3",
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
      val munitVersion = "1.0.0-M3"
      Seq(
        "dev.vgerasimov" %% "common-scala"     % "0.1.0",
        "dev.vgerasimov" %% "slowparse"        % "0.2.1",
        "com.lihaoyi"    %% "pprint"           % "0.7.0",
        "com.lihaoyi"    %% "upickle"          % "3.2.0",
        "org.scalameta"  %% "munit"            % munitVersion % Test,
        "org.scalameta"  %% "munit-scalacheck" % munitVersion % Test
      )
    },
    publishLocalConfiguration := publishLocalConfiguration.value.withOverwrite(true),
    publishM2Configuration := publishM2Configuration.value.withOverwrite(true),
  )
