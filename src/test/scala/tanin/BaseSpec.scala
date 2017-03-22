package tanin

import org.mockito.Mockito
import org.scalatest._

class BaseSpec extends FunSpec with org.specs2.mock.Mockito with BeforeAndAfterAll with BeforeAndAfter with Matchers {
  def verify[T](m: T, count: Int = 1): T = Mockito.verify(m, Mockito.times(count))
}
