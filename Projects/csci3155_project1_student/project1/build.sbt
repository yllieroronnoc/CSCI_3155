ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.7"

libraryDependencies ++= Seq(
   "org.scalactic" %% "scalactic" % "3.2.10",
    "org.scalatest" %% "scalatest" % "3.2.10" % "test"
)


lazy val root = (project in file("."))
  .settings(
    name := "project1"
  )

///
/// Everything that follows is for the `sbt checkAndZipSubmission` task
///
val extraPath = "extra_submission_files"
val zipFilePath = "submission.zip"
val confirmBeforeOverwrite = false


lazy val checkAndZipSubmission = taskKey[Unit]("Zip answer source files into submission")
checkAndZipSubmission := {
    val bd = baseDirectory.value
    // Scala task dependencies
    (Compile / compile).value

    def fail[T](message: String): T = {
        throw new sbt.MessageOnlyException(s"$message\nZip not made")
    }

    println("\n\n - This task does not check that your solution is correct, only that the files are in the correct place.\n   To run tests, use `sbt test` or run the 'Run All Tests' configuration in Intellij")

    // Relative -> absolute paths
    val rootPath = bd.getAbsolutePath
    val zipFileAbsPath = s"$rootPath/$zipFilePath"
    val srcAbsPath = s"$rootPath/src"
    val extraAbsPath = s"$rootPath/$extraPath"

    // Ensure files exist and make zip list
    val srcAbsDir = new File(srcAbsPath)
    val extraAbsDir = new File(extraAbsPath)
    if (!srcAbsDir.isDirectory) fail(s"X Could not find src directory (expected at $srcAbsPath)")
    val submissionFilePairs: Traversable[(File, String)] = Path.allSubpaths(srcAbsDir) ++ Path.allSubpaths(extraAbsDir)

    val zipFile = new File(zipFileAbsPath)
    if (zipFile.isFile && confirmBeforeOverwrite) {
        print("\n\n * INPUT REQUIRED: This will erase the old submission zip, continue?\n   Type 'y' or 'n' and press enter: ")
        val input = scala.io.StdIn.readChar()
        if (input != 'y') {
            fail("Canceled by user")
        }
    }

    println("\n\n - Saving zip of files:")
    println(submissionFilePairs
      .map { case (_, path) => "      " + path }
      .mkString("\n"))
    println("   to:")
    println("      " + zipFileAbsPath + "\n\n")

    IO.zip(submissionFilePairs , new File(zipFileAbsPath), None)
}
