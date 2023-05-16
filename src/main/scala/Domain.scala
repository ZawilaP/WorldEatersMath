import cats.implicits.catsSyntaxPartialOrder

object Domain {
  type DiceValue = Int
  type Count = Int

  sealed trait Trigger
  trait AnyDouble extends Trigger {
    def anyDoubleCheck(value: Map[DiceValue, Count]): Boolean = value.values.exists(_ > 1)
  }
  trait AnyTriple extends Trigger {
    def anyTripleCheck(value: Map[DiceValue, Count]): Boolean = value.values.exists(_ > 2)
  }
  trait Double3Up extends Trigger {
    def double3UpCheck(value: Map[DiceValue, Count]): Boolean = value.filter(dc => dc._1 > 2 && dc._2 > 0).values.sum > 1
  }
  trait Double4Up extends Trigger {
    def double4UpCheck(value: Map[DiceValue, Count]): Boolean = value.filter(dc => dc._1 > 3 && dc._2 > 0).values.sum > 1
  }
  trait Double5Up extends Trigger {
    def double5UpCheck(value: Map[DiceValue, Count]): Boolean = value.filter(dc => dc._1 > 4 && dc._2 > 0).values.sum > 1
  }
  trait Double6Up extends Trigger {
    def double6UpCheck(value: Map[DiceValue, Count]): Boolean = value.get(6) > Some(1)
  }
  trait Triple4Up extends Trigger {
    def triple4UpCheck(value: Map[DiceValue, Count]): Boolean = value.filter(dc => dc._1 > 3 && dc._2 > 0).values.sum > 2
  }

  sealed trait Powers {
    def check(value: Map[DiceValue, Count]): Boolean

    override def toString: String = getClass.getSimpleName.replaceAll("\\$", "")
  }
  object RageFuelledInvigoration extends Powers with AnyDouble {
    override def check(value: Map[DiceValue, Count]): Boolean = anyDoubleCheck(value)
  }
  object WrathfulDevotion extends Powers with AnyDouble {
    override def check(value: Map[DiceValue, Count]): Boolean = anyDoubleCheck(value)
  }
  object MartialExcellence extends Powers with Double3Up {
    override def check(value: Map[DiceValue, Count]): Boolean = double3UpCheck(value)
  }
  object TotalCarnage extends Powers with Double4Up with AnyTriple {
    override def check(value: Map[DiceValue, Count]): Boolean = double4UpCheck(value) || anyTripleCheck(value)
  }
  object WarpBlades extends Powers with Double5Up with AnyTriple {
    override def check(value: Map[DiceValue, Count]): Boolean = double5UpCheck(value) || anyTripleCheck(value)
  }
  object UnbridledBloodlust extends Powers with Double6Up with Triple4Up {
    override def check(value: Map[DiceValue, Count]): Boolean = double6UpCheck(value) || triple4UpCheck(value)
  }
}
