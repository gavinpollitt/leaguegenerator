package uk.gav.league.generation

import java.util.Date
import scala.collection.mutable.ArrayBuffer
import scala.Array
import scala.Array.ofDim
import scala.collection.mutable.Buffer
import scala.util.Random
import play.libs.Json
import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.module.scala.DefaultScalaModule

/**
 * @author gavin
 * Uses the alogirthm to generate the fixtures
 */
class LeagueGenerator(val teams: Array[Team], val headToHead: Int) {
  val groups: ArrayBuffer[FixtureGroup] = ArrayBuffer[FixtureGroup]()

  //Don't need 'new' for Option as it's an object being called on its 'apply' method
  val emptyTeam: Option[Int] = if (teams.length % 2 == 0) None else Some(teams.length)

  def generateFixtures() {
    val teamCnt = teams.length + teams.length % 2
    val indArray = 0 to teamCnt - 1 toArray //array of Int

    val fixSet = ofDim[Int](teamCnt - 1, teamCnt) //This is how to set a multi-dimensional array in scala
    fixSet(0) = generateSetOne(indArray);

    for (i <- 1 to fixSet.length - 1) {
      fixSet(i) = generateSets(fixSet(i - 1), i)
    }

    //Assign the teams
    assignTeams(fixSet)

    // Reverse
    val revSet = ArrayBuffer[FixtureGroup]()
    for (fg <- groups) {
      revSet += fg.reverseFixtures(1)
    }

    groups ++= revSet

    for (fg <- groups) {
      println(fg.fixtures.mkString(","))
    }

  }

  private def generateSetOne(fx: Array[Int]) = {
    val outfx = new Array[Int](fx.length)

    var ind = 0
    for (i <- 0 to fx.length / 2 - 1) {
      outfx(ind) = fx(i)
      ind += 1
      outfx(ind) = fx(fx.length - 1 - i)
      ind += 1
    }
    outfx
  }

  private def generateSets(fx: Array[Int], groupInd: Int) = {
    val outfx = new Array[Int](2)
    val odd = groupInd % 2 == 1
    outfx(if (odd) 1 else 0) = fx(fx.length - 1)
    // reduceLeft works from right on successive pairs in array
    outfx(if (odd) 0 else 1) = fx.reduceLeft(_ max _) // don't forget this is how functions can be call obj func arg

    val fxBuff = fx.toBuffer
    fxBuff -= outfx(0)
    fxBuff -= outfx(1)

    outfx ++ constructSet(fxBuff)
  }

  // Needs return type as it's recursive
  private def constructSet(fx: Buffer[Int]): Array[Int] = {
    if (fx.size == 0) return new Array[Int](0)
    val outfx = new Array[Int](2)
    outfx(1) = fx.remove(fx.length - 1)
    outfx(0) = fx.remove(fx.length - 1)
    outfx ++ constructSet(fx)
  }

  private def assignTeams(fx: Array[Array[Int]]) {
    //Randomize teams
    val sortedTeams = new Array[Team](teams.length)
    val rng = new Random
    //Option protects a value from null by giving it defined outputs depending on 
    // whether it is populated or not.
    val emptySlot = (index: Int) => (Option[Team](sortedTeams(index))).isEmpty

    for (i <- 0 to teams.length - 1) {
      var pos = rng.nextInt(teams.length)
      while (!emptySlot(pos)) {
        pos = rng.nextInt(teams.length)
      }

      sortedTeams(pos) = teams(i)
    }

    // Now sort the groups
    var index = 0
    for (fxLine <- fx) {
      val fg = new FixtureGroup(0, index)
      index += 1
      // See if we need to skip the fixture because team is the undefined 'odd' team
      var emptyFx = (h: Int, a: Int) => emptyTeam.isDefined && (emptyTeam.get == h || emptyTeam.get == a)
      groups += fg
      for (i <- 0 until (fxLine.length - 1, 2)) {
        if (!emptyFx(fxLine(i), fxLine(i + 1))) {
          var fx = new Fixture(sortedTeams(fxLine(i)), sortedTeams(fxLine(i + 1)))
          fg.fixtures += fx
        }
      }
    }

  }

  private def shuffle(sep: Int) {

  }

  def serialiseLeague() = {
    val mapper = new ObjectMapper()
    mapper.registerModule(DefaultScalaModule)
    Json.setObjectMapper(mapper)
    val json = Json.toJson(groups)
    Json.prettyPrint(json)
  }
}

object LeagueGenerator {
  def main(args: Array[String]) {
    val teams: Array[Team] = Array(new Team("Man Utd"), new Team("Man City"), new Team("Liverpool"), new Team("Chelsea"), new Team("Spurs"))
    val lg: LeagueGenerator = new LeagueGenerator(teams, 2)
    lg.generateFixtures()
    println("JSON:" + lg.serialiseLeague())
  }

  def apply(teams:Array[Team]) = {
    new LeagueGenerator(teams, 2)
  }
}

/**
 * @author gavin
 */
class Fixture(val homeTeam: Team, val awayTeam: Team) {
  var venue: String = _
  var time: Date = _ //for now
  var homeScore = 0
  var awayScore = 0

  override def toString() = homeTeam.name + " v " + awayTeam.name
}

/**
 * @author gavin
 */
class FixtureGroup(val gpNum: Int, val id: Int) {
  val fixtures: ArrayBuffer[Fixture] = ArrayBuffer[Fixture]()

  def reverseFixtures(newGp: Int) = {
    val revFx = ArrayBuffer[Fixture]()
    for (fx <- fixtures) {
      var newFx = new Fixture(fx.awayTeam, fx.homeTeam)
      revFx += newFx
    }

    val fg = new FixtureGroup(newGp, id)
    fg.fixtures ++= revFx
    fg
  }

}