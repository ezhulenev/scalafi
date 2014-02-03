import sbt._, Keys._
import scala.language.postfixOps

object ScalaFIBuildSettings {

  val buildOrganization   =  "scalafi"
  val buildVersion        =  "0.0.1"
  val buildScalaVersion   =  "2.10.3"

  val buildSettings = Defaults.defaultSettings ++ Seq (
    organization          := buildOrganization,
    scalaVersion          := buildScalaVersion,
    shellPrompt           := ShellPrompt.buildShellPrompt,
    maxErrors             := 5,
    exportJars            := true,
    scalacOptions         += "-unchecked",
    scalacOptions         += "-deprecation",
    scalacOptions         += "-feature",
    libraryDependencies   += "org.scala-lang" % "scala-reflect" % scalaVersion.value
  )

  val buildResolvers = resolvers ++= Seq(
    "Typesafe Repo"       at "http://repo.typesafe.com/typesafe/releases/",
    "Sonatype Snapshots"  at "http://oss.sonatype.org/content/repositories/snapshots",
    "Sonatype Releases"   at "http://oss.sonatype.org/content/repositories/releases"
  )
}

object ScalaFIBuild extends Build {

  import ScalaFIBuildSettings._

  val release           = settingKey[Boolean]("Perform release")
  val gitHeadCommitSha  = settingKey[String]("current git commit SHA")

  private def getVersion(release: Boolean, sha: String) = {
    import java.text.SimpleDateFormat
    import java.util.{Calendar, TimeZone}
    val utcTz = TimeZone.getTimeZone("UTC")
    val cal = Calendar.getInstance(utcTz)
    val sdf = new SimpleDateFormat("yyyyMMdd'T'HHmmss'Z'")
    sdf.setCalendar(cal)
    if (!release) s"$buildVersion-${sdf.format(cal.getTime)}-$sha" else buildVersion
  }

  def ScalaFiProject(name: String) = {
    Project(id = name, base = file(name)).
      // Apply global settings
      settings(buildSettings:_*).
      settings(buildResolvers:_*).

      // Dependency graph
      settings(net.virtualvoid.sbt.graph.Plugin.graphSettings: _*).

      // Versioning settings
      settings(release                       := sys.props("scalafi-release") == "true").
      settings(gitHeadCommitSha in ThisBuild := Process("git rev-parse --short HEAD").lines.head).
      settings(version in ThisBuild          := getVersion(release.value, gitHeadCommitSha.value)).

      // Publishing settings (disable publishing)
      settings(publish := ())

  }

  lazy val root = Project("scalafi", file(".")).
    settings(publish :=()).
    settings(publishLocal :=()).
    aggregate(example, core)

  lazy val example = ScalaFiProject("example").
    settings(libraryDependencies ++= Dependencies.example).
    dependsOn(core)

  lazy val core = ScalaFiProject("core").
    settings(libraryDependencies ++= Dependencies.core)

}

object Dependencies {

  object Versions {
    // Logging
    val Slf4j              = "1.7.5"
    val Logback            = "1.0.13"

    // Joda
    val JodaTime           = "2.3"
    val JodaConvert        = "1.5"

    // Scalaz
    val Scalaz             = "7.0.3"
    val ScalazStream       = "0.1"

    // Math libraries
    val Breeze             = "0.5.2"
    val CommonsMath        = "2.2"

    // Test libraries
    val ScalaMock          = "3.1.RC1"
    val ScalaTest          = "2.0"
  }

  object Compile {
    import Dependencies.{Versions => V}

    // Logging
    val slf4jApi            =   "org.slf4j"                  % "slf4j-api"                   % V.Slf4j
    val logback             =   "ch.qos.logback"             % "logback-classic"             % V.Logback

    // Joda
    val jodaTime            =   "joda-time"                  % "joda-time"                   % V.JodaTime
    val jodaConvert         =   "org.joda"                   % "joda-convert"                % V.JodaConvert

    // Scalaz
    val scalazCore          =  "org.scalaz"                 %% "scalaz-core"                 % V.Scalaz
    val scalazEffect        =  "org.scalaz"                 %% "scalaz-effect"               % V.Scalaz
    val scalazIteratee      =  "org.scalaz"                 %% "scalaz-iteratee"             % V.Scalaz
    val scalazStream        =  "org.scalaz.stream"          %% "scalaz-stream"               % V.ScalazStream

    // Math libraries
    val breeze              =  "org.scalanlp"               %% "breeze"                      % V.Breeze
    val commonsMath         =   "org.apache.commons"         % "commons-math"                % V.CommonsMath
  }

  object Test {
    import Dependencies.{Versions => V}

    val scalaMock         =   "org.scalamock"               %% "scalamock-scalatest-support" % V.ScalaMock     % "test"
    val scalaTest         =   "org.scalatest"               %% "scalatest"                   % V.ScalaTest     % "test"
  }

  // Projects dependencies



  val example = Seq()

  val core = Seq(Compile.slf4jApi, Compile.logback, Compile.breeze, Compile.commonsMath, Compile.scalazCore)

}

object ShellPrompt {
  object devnull extends ProcessLogger {
    def info (s: => String) {}
    def error (s: => String) {}
    def buffer[T] (f: => T): T = f
  }

  val current = """\*\s+([\w-/]+)""".r

  def gitBranches = ("git branch --no-color" lines_! devnull).mkString

  val buildShellPrompt = {
    (state: State) => {
      val currBranch =
        current findFirstMatchIn gitBranches map (_ group 1) getOrElse "-"
      val currProject = Project.extract (state).currentProject.id
      "%s:%s> ".format (
        currProject, currBranch
      )
    }
  }
}