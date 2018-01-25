name := "scala-with-cats-sandbox"
version := "0.0.1-SNAPSHOT"

scalaVersion := "2.12.4"

scalacOptions ++= Seq(
  "-encoding",
  "UTF-8", // source files are in UTF-8
  "-deprecation", // warn about use of deprecated APIs
  "-unchecked", // warn about unchecked type parameters
  "-feature", // warn about misused language features
  "-language:higherKinds", // allow higher kinded types without `import scala.language.higherKinds`
  "-Xlint", // enable handy linter warnings
  "-Xfatal-warnings", // turn compiler warnings into errors
  "-Ypartial-unification" // allow the compiler to unify type constructors of different arities
)

scalacOptions in (Compile, console) -= "-Xfatal-warnings"

scalacOptions in (Compile, console) += "-Ywarn-unused:-imports"

scalacOptions in (Test, console) := (scalacOptions in (Compile, console)).value

libraryDependencies += "org.typelevel" %% "cats-core" % "1.0.1"

libraryDependencies += "com.lihaoyi" % "ammonite" % "1.0.3" % Test cross CrossVersion.full

Test / sourceGenerators += Def.task {
  val file = (Test / sourceManaged).value / "amm.scala"
  IO.write(
    file,
    """object amm extends App {
      | ammonite.Main.main(args :+ "--predef-code" :+ "interp.configureCompiler(_.settings.YpartialUnification.value = true)")
      |}""".stripMargin
  )
  Seq(file)
}.taskValue

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.5")
