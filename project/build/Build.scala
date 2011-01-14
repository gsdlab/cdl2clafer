import sbt._

class Build(info: ProjectInfo) extends DefaultProject(info) {

  val scalatest = "org.scalatest" % "scalatest" % "1.0"
  val junit= "junit" % "junit" % "4.7"        
  val cdlTools = "gsd" % "CDLTools" %  "0.0.1-SNAPSHOT"        
  val graph = "gsd" % "graph" % "1.0.1-SNAPSHOT"
  val kiama = "com.googlecode" % "kiama" % "0.9.0"
  val paulp = "com.github.paulp_optional" % "optional" % "0.0.1"

  val mavenLocalRepo = "LocalMavenRepo" at "file://" + Path.userHome + "/.m2/repository"
  val scalaToolsRepo = "Scala tools" at "http://www.scala-tools.org/repo-releases"
  val snapshotsRepo = "gsd-snapshots" at "http://bielsko.uwaterloo.ca:8081/nexus/content/repositories/snapshots"
  val thirdPartyRepo = "gsd-third-party" at "http://bielsko.uwaterloo.ca:8081/nexus/content/repositories/thirdparty"

  Credentials(Path.userHome / ".ivy2" / ".credentials", log)
}
