
import org.junit.Test
import org.junit.Assert._

class TestMacros {

  @Test def testSum = {
    assertEquals(6, snippets.Section3Macros.sum(Array(1, 2, 3)))
    assertEquals(6, snippets.Section3Macros.sumN(3, Array(1, 2, 3)))

    var sum = 0
    snippets.Section6.foreach(Array(1, 2, 3), x => sum += x)
    assertEquals(6, sum)
  }

}