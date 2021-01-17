import mill._
import mill.scalalib._
import mill.scalalib.scalafmt._

import $ivy.`com.lihaoyi::mill-contrib-bloop:$MILL_VERSION`

import $ivy.`com.lihaoyi::mill-contrib-scoverage:$MILL_VERSION`
import mill.contrib.scoverage.ScoverageModule

import $ivy.`com.goyeau::mill-scalafix:0.2.1`
import com.goyeau.mill.scalafix.StyleModule

import $ivy.`io.github.davidgregory084::mill-tpolecat:0.2.0`
import io.github.davidgregory084.TpolecatModule

object Dependencies {
  // Cats Core
  val cats       = "2.3.0"
  val kittens    = "2.2.1"
  val mouse      = "0.26.2"
  val catsEffect = "2.3.0"

  // Streaming
  val fs2 = "2.4.6"

  // Parser
  val atto     = "0.8.0"
  val mainargs = "0.1.4"

  // Math
  val breeze = "1.1"

  // Types
  val shapeless  = "2.3.3"
  val newtype    = "0.4.4"
  val refined    = "0.9.19"
  val enumeratum = "1.6.1"

  // Logging
  val log4cats = "1.1.1"
  val log4j2   = "2.13.3"

  // Compiler Plugins
  val kindProjector    = "0.11.2"
  val betterMonadicFor = "0.3.1"
  val scalaTypedHoles  = "0.1.6"
  val splain           = "0.5.7"

  // Scalafix
  val organizeImports = "0.4.4"
}

object prediction extends CommonModule {

  override def artifactName = "prediction"

  override def forkArgs = Seq("-Xmx256M")

  override def ivyDeps =
    Agg(
      ivy"org.typelevel::alleycats-core:${Dependencies.cats}",
      ivy"org.typelevel::cats-core:${Dependencies.cats}",
      ivy"org.typelevel::cats-effect:${Dependencies.catsEffect}",
      ivy"org.typelevel::kittens:${Dependencies.kittens}",
      ivy"org.typelevel::mouse:${Dependencies.mouse}",
      ivy"co.fs2::fs2-core:${Dependencies.fs2}",
      ivy"co.fs2::fs2-io:${Dependencies.fs2}",
      ivy"org.tpolecat::atto-core:${Dependencies.atto}",
      ivy"org.tpolecat::atto-fs2:${Dependencies.atto}",
      ivy"org.tpolecat::atto-refined:${Dependencies.atto}",
      ivy"com.lihaoyi::mainargs:${Dependencies.mainargs}",
      ivy"org.scalanlp::breeze:${Dependencies.breeze}",
      ivy"org.scalanlp::breeze-natives:${Dependencies.breeze}",
      ivy"org.scalanlp::breeze-viz:${Dependencies.breeze}",
      ivy"io.chrisdavenport::log4cats-core:${Dependencies.log4cats}",
      ivy"io.chrisdavenport::log4cats-slf4j:${Dependencies.log4cats}",
      ivy"com.chuusai::shapeless:${Dependencies.shapeless}",
      ivy"io.estatico::newtype:${Dependencies.newtype}",
      ivy"eu.timepit::refined:${Dependencies.refined}",
      ivy"eu.timepit::refined-cats:${Dependencies.refined}",
      ivy"com.beachape::enumeratum:${Dependencies.enumeratum}",
      ivy"com.beachape::enumeratum-cats:${Dependencies.enumeratum}"
    )

  override def runIvyDeps =
    Agg(
      ivy"org.apache.logging.log4j:log4j-api:${Dependencies.log4j2}",
      ivy"org.apache.logging.log4j:log4j-core:${Dependencies.log4j2}",
      ivy"org.apache.logging.log4j:log4j-slf4j-impl:${Dependencies.log4j2}"
    )
}

// ----- Base -----

trait CommonModule extends ScalaModule with TpolecatModule with CommonStyleModule {

  override def scalaVersion = "2.13.4"

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
}

trait CommonStyleModule extends StyleModule {
  override def scalafixIvyDeps =
    Agg(
      ivy"com.github.liancheng::organize-imports:${Dependencies.organizeImports}"
    )
}
