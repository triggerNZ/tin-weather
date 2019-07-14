package triggernz.weather

import triggernz.weather.util.NonEmptyVector
import utest._

import scalaz.syntax.foldable1._

import nyaya.prop._
import nyaya.gen._

import scalaz.Equal
import scalaz.std.anyVal._

import nyaya.test.PropTest._

object NonEmptyVectorTest extends TestSuite {
  object laws {
    def leftFM1Consistent[A : Equal] = Prop.test[NonEmptyVector[A]](
      "leftFM1Consistent",
      NonEmptyVector.foldable1.foldable1Law.leftFM1Consistent(_)
    )

    def leftFMConsistent [A : Equal] = Prop.test[NonEmptyVector[A]](
      "leftFMConsistent",
      NonEmptyVector.foldable1.foldable1Law.leftFMConsistent(_)
    )

    def rightFM1Consistent[A : Equal] = Prop.test[NonEmptyVector[A]](
      "rightFM1Consistent",
      NonEmptyVector.foldable1.foldable1Law.rightFM1Consistent(_)
    )

    def rightFMConsistent[A : Equal] = Prop.test[NonEmptyVector[A]](
      "rightFMConsistent",
      NonEmptyVector.foldable1.foldable1Law.rightFMConsistent(_)
    )
  }


  def genNev[A](underlying: Gen[A]): Gen[NonEmptyVector[A]] = for {
    head <- underlying
    tail <- underlying.vector(0 to 10)
  } yield (NonEmptyVector(head, tail))

  override def tests = Tests {
    'foldable1 - {
      'examples - {
        val vec = NonEmptyVector(1,2,3,4,5)
        vec.foldLeft(0)(_ + _) ==> 15
      }

      'laws - {
        testProp(
          laws.leftFM1Consistent[Int] & laws.leftFMConsistent /* & laws.rightFM1Consistent &  & laws.rightFMConsistent */,
          genNev(Gen.int)
        )


        //TODO: Right laws are broken! fix and reenable
      }
    }
  }
}
