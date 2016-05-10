import java.io._
import scala.tools.nsc.ScalaDoc

class ScaladocSuite extends CompilerSuite {
  val resourceDir = new File(System.getProperty("sbt.paths.tests.scaladoc") + File.separatorChar + "resources")
  val testDirs = resourceDir.listFiles().filter(_.isDirectory).filter(_.listFiles().nonEmpty).filter(!_.getName().endsWith("_disabled"))
  testDirs.foreach(testDir => test(testDir.getName)(runTool(testDir, options => ScalaDoc.main(options))))
}
