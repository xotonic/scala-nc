import org.eltex.ems.netconf.{YangParser, YangParserV2}
import org.junit.Assert._
import org.junit.Test

class CommonTest extends Timing {
  @Test def identifier_naming() {
    assertTrue(YangParser.parseAll(YangParser.identifier, "test_1").successful)
    assertFalse(YangParser.parseAll(YangParser.identifier, "1est_1").successful)
    assertFalse(YangParser.parseAll(YangParser.identifier, "mest_1").successful)
  }
  @Test def module_common() {
    time {
      println(YangParser.parseAll(YangParser.moduleStmt,
        """
          |module ap-config {
          |   yang-version 1;
          |   prefix ap-config;
          |}
          |
      """.stripMargin))
    }
  }
@Test def parboiled() {
    time {
      val run = new YangParserV2("s").alpha.run()
      print(run.get)
    }
  }

}
