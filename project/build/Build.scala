import sbt._

class Build(info: ProjectInfo) extends DefaultProject(info) with ProguardProject  {

  val junit= "junit" % "junit" % "4.7"        
  val graph = "gsd" % "graph" % "1.0.1-SNAPSHOT"
  val kiama = "com.googlecode" %% "kiama" % "1.1.0"
  val paulp = "com.github.paulp_optional" % "optional" % "0.0.1"
  val scalatest = "org.scalatest" % "scalatest_2.8.1" % "1.5.1"
  val javacup = "gsd" % "java-cup" % "11a"
  val imlparser = "gsd" % "iml-parser" % "dev_1.0"
  val xcdl_analysis = "gsd" %% "xcdl-analysis" % "dev_1.2"

  val mavenLocalRepo = "LocalMavenRepo" at "file://" + Path.userHome + "/.m2/repository"
  val scalaToolsRepo = "Scala tools" at "http://www.scala-tools.org/repo-releases"
//  val snapshotsRepo = "gsd-snapshots" at "http://bielsko.uwaterloo.ca:8081/nexus/content/repositories/snapshots"
//  val thirdPartyRepo = "gsd-third-party" at "http://bielsko.uwaterloo.ca:8081/nexus/content/repositories/thirdparty"


  override def mainScalaSourcePath = "src" / "main" / "scala"

//  override def mainClass = Some("gsd.cdl2clafer.CDL2ClaferRun")

  Credentials(Path.userHome / ".ivy2" / ".credentials", log)


  override def proguardOptions = List(
//    "-keep class MyClass { myMethod; }",
    proguardKeepMain("gsd.cdl2clafer.CDL2ClaferRun")
  )
  
  override def proguardInJars = super.proguardInJars +++ scalaLibraryPath
}
