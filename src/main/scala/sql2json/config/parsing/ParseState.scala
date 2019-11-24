package sql2json
package config
package parsing

import cat.Functor.given
import cat.ApplicativeError.given
import cat.Show
import cat.Show.given
import types.Errors

import java.nio.file.Path
import sql2json.jdbc.Database
import sql2json.types.Validated
import sql2json.types.Validated.given

import scala.util.matching.Regex 

final case class ParseContext(file: Path, line: Int)
  def next: ParseContext = copy(line = line + 1)

object ParseContext
  def apply(file: Path): ParseContext = ParseContext(file, 1)

  given Show[ParseContext]
    def show (ctx: ParseContext): String = s"${ctx.file.show}:${ctx.line}"

sealed abstract class PartialConfig
  def accum: Map[Database, DBConfig]

object PartialConfig 
  final case class Stable(override val accum: Map[Database, DBConfig]) extends PartialConfig
    def startBlock(db: Database)(given ParseContext): Incomplete = Incomplete(accum, IncompleteBlock.init(db))

  final case class Incomplete(override val accum: Map[Database, DBConfig], block: IncompleteBlock) 
                              extends PartialConfig
    def stabilize(result: (Database, DBConfig)): Stable = Stable(accum + result)

    def continue(block: IncompleteBlock): Incomplete = copy(block = block)

sealed abstract class ParseState
  def consumeLine(line: String)(given ParseContext): Validated[ParseState]

  def finish(given ParseContext): Validated[Config]

object ParseState
  final val CommentRegex: Regex = """\s*#.*""".r
  final val BlankLineRegex: Regex = """\s*""".r

  val init = WaitingForBlock(PartialConfig.Stable(Map.empty))

  final case class WaitingForBlock(partial: PartialConfig.Stable) extends ParseState
    final val BlockHeaderRegex: Regex = """\[(.+)\]\s*""".r

    def consumeLine(line: String)(given ctx: ParseContext): Validated[ParseState] =
      line match
        case BlankLineRegex() => this.valid
        case CommentRegex() => this.valid
        case BlockHeaderRegex(raw) => 
          Database(raw)
            .map(partial.startBlock)
            .map(ProcessingBlock)
            .mapError[Errors](_.map(error => s"${ctx.show} $error"))
        
        case _ => s"${ctx.show} unparsable line".invalid
  
    def finish(given ParseContext): Validated[Config] = Config(partial.accum).valid 

  final case class ProcessingBlock(partial: PartialConfig.Incomplete) extends ParseState

    def consumeLine(line: String)(given ParseContext): Validated[ParseState] =
      line match
        case CommentRegex() => this.valid
        case BlankLineRegex() => 
          partial
            .block
            .complete
            .map(partial.stabilize)
            .map(WaitingForBlock)

        case parsableLine => 
          partial
            .block
            .consumeLine(parsableLine)
            .map(partial.continue)
            .map(ProcessingBlock)

    def finish(given ParseContext): Validated[Config] = 
      partial
        .block
        .complete
        .map(partial.stabilize)
        .map(_.accum)
        .map(Config(_))


