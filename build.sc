import mill._
import mill.scalalib._
import mill.scalalib.scalafmt._

import $ivy.`com.lihaoyi::mill-contrib-bloop:$MILL_VERSION`

import $ivy.`com.lihaoyi::mill-contrib-scoverage:$MILL_VERSION`
import mill.contrib.scoverage.ScoverageModule

import $ivy.`com.goyeau::mill-scalafix:0.2.2`
import com.goyeau.mill.scalafix.StyleModule

import $ivy.`io.github.davidgregory084::mill-tpolecat:0.2.0`
import io.github.davidgregory084.TpolecatModule

object Dependencies {
  val cats           = "2.6.1"
  val catsEffect     = "3.1.1"
  val catsTime       = "0.3.4"
  val catsEffectTime = "0.1.2"
  val kittens        = "2.3.1"
  val fs2            = "3.0.3"
  val newtype        = "0.4.4"
  val refined        = "0.9.25"
  val enumeratum     = "1.6.1"
  val breeze         = "1.2"
  val atto           = "0.9.5"

  // Testing
  val munit           = "0.7.26"
  val munitCatsEffect = "1.0.3"

  // Compiler Plugins
  val kindProjector    = "0.11.3"
  val betterMonadicFor = "0.3.1"
  val scalaTypedHoles  = "0.1.8"
  val splain           = "0.5.8"

  // Scalafix
  val organizeImports = "0.5.0"
}

object prediction extends ScalaModule with TpolecatModule with StyleModule with CommonScoverageModule {

  override def scalaVersion = "2.13.5"

  override def coverageExcludedPackages = "prediction.application.*"

  override def artifactName = "prediction"

  // override def forkArgs = Seq("-Xmx64M")

  override def ivyDeps =
    Agg(
      ivy"org.typelevel::cats-core:${Dependencies.cats}",
      ivy"org.typelevel::cats-effect:${Dependencies.catsEffect}",
      ivy"io.chrisdavenport::cats-time:${Dependencies.catsTime}",
      ivy"io.chrisdavenport::cats-effect-time:${Dependencies.catsEffectTime}",
      ivy"org.typelevel::kittens:${Dependencies.kittens}",
      ivy"co.fs2::fs2-core:${Dependencies.fs2}",
      ivy"co.fs2::fs2-io:${Dependencies.fs2}",
      ivy"io.estatico::newtype:${Dependencies.newtype}",
      ivy"eu.timepit::refined:${Dependencies.refined}",
      ivy"eu.timepit::refined-cats:${Dependencies.refined}",
      ivy"com.beachape::enumeratum:${Dependencies.enumeratum}",
      ivy"com.beachape::enumeratum-cats:${Dependencies.enumeratum}",
      ivy"com.beachape::enumeratum-scalacheck:${Dependencies.enumeratum}",
      ivy"org.tpolecat::atto-core:${Dependencies.atto}",
      ivy"org.tpolecat::atto-fs2:${Dependencies.atto}",
      ivy"org.tpolecat::atto-refined:${Dependencies.atto}",
      ivy"org.scalanlp::breeze:${Dependencies.breeze}",
      ivy"org.scalanlp::breeze-natives:${Dependencies.breeze}"
    )

  override def scalacOptions =
    super.scalacOptions() ++
      Seq(
        "-Yrangepos",                   // Use range positions for syntax trees.
        "-Ymacro-annotations",          // Scala recognizes as a macro will let it expand, possibly into multiple members.
        "-P:typed-holes:log-level:info" // Emulate the "typed holes" feature of Haskell, Idris, Agda, etc.
      )

  override def scalacPluginIvyDeps =
    super.scalacPluginIvyDeps() ++
      Agg(
        ivy"org.typelevel:::kind-projector:${Dependencies.kindProjector}",
        ivy"com.olegpy::better-monadic-for:${Dependencies.betterMonadicFor}",
        ivy"com.github.cb372:::scala-typed-holes:${Dependencies.scalaTypedHoles}",
        ivy"io.tryp:::splain::${Dependencies.splain}"
      )

  override def scalafixIvyDeps =
    Agg(
      ivy"com.github.liancheng::organize-imports:${Dependencies.organizeImports}"
    )

  object test extends ScoverageTests with StyleModule {

    override def ivyDeps =
      Agg(
        ivy"org.scalameta::munit::${Dependencies.munit}",
        ivy"org.scalameta::munit-scalacheck::${Dependencies.munit}",
        ivy"org.typelevel::munit-cats-effect-3::${Dependencies.munitCatsEffect}"
      )

    override def scalafixIvyDeps =
      Agg(
        ivy"com.github.liancheng::organize-imports:${Dependencies.organizeImports}"
      )

    override def testFrameworks = Seq("munit.Framework")
  }
}

trait CommonScoverageModule extends ScoverageModule {
  outer: ScalaModule =>

  def coverageExcludedPackages: T[String] = T("")

  override def scoverageVersion = "1.4.7"

  override def scoveragePluginDep = T(ivy"org.scoverage:::scalac-scoverage-plugin:${scoverageVersion()}")

  override val scoverage: ScoverageData = new CustomScoverageData(implicitly)

  class CustomScoverageData(ctx0: mill.define.Ctx) extends ScoverageData(ctx0) {
    override def scalacOptions =
      super.scalacOptions() ++
        Seq(s"-P:scoverage:excludedPackages:${outer.coverageExcludedPackages()}")
  }
}
