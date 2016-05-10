import org.scalatest._
import java.io._
import java.security.Permission
import scala.compat.Platform.EOL
import scala.io.Source

class CompilerSuite extends FunSuite {
  private def virtualizedPopen(body: => Unit): (Int, String) = {
    val outputStorage = new ByteArrayOutputStream()
    val outputStream = new PrintStream(outputStorage)
    case class SystemExitException(exitCode: Int) extends SecurityException
    val manager = System.getSecurityManager()
    System.setSecurityManager(new SecurityManager {
      override def checkPermission(permission: Permission): Unit = ()
      override def checkPermission(permission: Permission, context: AnyRef): Unit = ()
      override def checkExit(exitCode: Int): Unit = throw new SystemExitException(exitCode)
    })
    try { scala.Console.withOut(outputStream)(scala.Console.withErr(outputStream)(body)); throw new Exception("failed to capture exit code") }
    catch { case SystemExitException(exitCode) => outputStream.close(); (exitCode, outputStorage.toString) }
    finally System.setSecurityManager(manager)
  }
  def runTool(sourceDir: File, tool: Array[String] => Unit): Unit = {
    val sources = sourceDir.listFiles().filter(_.getName.endsWith(".scala")).map(_.getAbsolutePath).toList
    val cp = List("-cp", sys.props("sbt.paths.tests.classpath"))
    val paradise = List("-Xplugin:" + sys.props("sbt.paths.plugin.jar"), "-Xplugin-require:macroparadise")
    val tempDir = File.createTempFile("temp", System.nanoTime.toString); tempDir.delete(); tempDir.mkdir()
    val output = List("-d", tempDir.getAbsolutePath)
    val options = cp ++ paradise ++ output ++ sources
    val (exitCode, stdout) = virtualizedPopen(tool(options.toArray))
    val actualOutput = exitCode + EOL + stdout
    val expectedOutput = Source.fromFile(sourceDir.getAbsolutePath + ".check").mkString
    assert(actualOutput === expectedOutput)
  }
}
