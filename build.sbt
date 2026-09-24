val giteaBase = sys.env.getOrElse("GITEA_URL", "https://gitea.local.vgerasimov.dev").stripSuffix("/")
val giteaMaven = s"$giteaBase/api/packages/wlad031/maven"
val giteaHost = new java.net.URI(giteaBase).getHost

val root = project
  .in(file("."))
  .settings(
    scalaVersion := "3.8.3",
    organization := "dev.vgerasimov",
    name := "md4s",
    version := "0.1.0",
    resolvers ++= Seq(
      Resolver.mavenLocal,
      "Gitea Maven (wlad031)" at giteaMaven,
    ),
    publishTo := Some("Gitea Maven" at giteaMaven),
    publishMavenStyle := true,
    credentials ++= (for {
      user <- sys.env.get("GITEA_USER")
      token <- sys.env.get("GITEA_TOKEN")
    } yield Credentials("Gitea Package API", giteaHost, user, token)).toSeq,
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
