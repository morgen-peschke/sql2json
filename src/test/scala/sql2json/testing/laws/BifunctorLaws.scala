package sql2json
package testing
package laws 

import cat.Bifunctor
import cat.Bifunctor.given
import cat.Eq
import cat.Show
import cat.Applicative.~
import testing.Arbitrary
import testing.Arbitrary.forAll
import testing.Result.given
import org.junit.Test

abstract class BifunctorLaws[F[_,_], LA, LB, LC, RA, RB, RC](given BF: BifunctorLaws.Givens[F, LA, LB, LC, RA, RB, RC])

  @Test def bimapIdentityLaw(): Unit = BF.run {
    forAll[F[LA,RA]]("identity over bimap") { fa => 
      fa <-> fa.bimap(identity, identity)
    }
  }
    
  @Test def leftMapIdentityLaw(): Unit = BF.run {
    forAll[F[LA,RA]]("identity over leftMap") { fa => 
      fa <-> fa.leftMap(identity)
    }
  }

  @Test def rightMapIdentityLaw(): Unit = BF.run {
    forAll[F[LA,RA]]("identity over rightMap") { fa => 
      fa <-> fa.rightMap(identity)
    }
  }

  @Test def bimapCompositionLaw(): Unit = BF.run {
    forAll[F[LA,RA] ~ (LA => LB) ~ (LB => LC) ~ (RA => RB) ~ (RB => RC)]("cmposition over bimap") {
      case fab ~ al2bl ~ bl2cl ~ ar2br ~ br2cr => 
        fab.bimap(al2bl, ar2br).bimap(bl2cl, br2cr) <-> fab.bimap(al2bl andThen bl2cl, ar2br andThen br2cr)
    }
  }

object BifunctorLaws
  class Givens[F[_,_], LA, LB, LC, RA, RB, RC](
    given
    Bifunctor[F],
    Show[F[LA,RA]],
    Show[F[LC,RC]],
    Eq[F[LA,RA]],
    Eq[F[LC, RC]],
    Arbitrary[F[LA,RA]],
    Arbitrary[LA => LB],
    Arbitrary[LB => LC],
    Arbitrary[RA => RB],
    Arbitrary[RB => RC]
  ) with
    def run(body: (
      given 
      Bifunctor[F],
      Show[F[LA,RA]],
      Show[F[LC,RC]],
      Eq[F[LA,RA]],
      Eq[F[LC, RC]],
      Arbitrary[F[LA,RA]],
      Arbitrary[LA => LB],
      Arbitrary[LB => LC],
      Arbitrary[RA => RB],
      Arbitrary[RB => RC]
    ) => Unit): Unit = 
        body.apply

  given[F[_,_], LA, LB, LC, RA, RB, RC](
    given
    Bifunctor[F],
    Show[F[LA,RA]],
    Show[F[LC,RC]],
    Eq[F[LA,RA]],
    Eq[F[LC, RC]],
    Arbitrary[F[LA,RA]],
    Arbitrary[LA => LB],
    Arbitrary[LB => LC],
    Arbitrary[RA => RB],
    Arbitrary[RB => RC]
  ): Givens[F, LA, LB, LC, RA, RB, RC]