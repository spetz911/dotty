package dotty.tools.dotc.util

import Positions.{Position, NoPosition}

/** A source position is comprised of a position in a source file */
case class SourcePosition(source: SourceFile, pos: Position) {
  def point: Int = pos.point
  def start: Int = pos.start
  def end: Int = pos.end
  def exists = pos.exists

  def lineContents: String = source.lineContents(point)
  def line: Int = source.offsetToLine(point)
  def column: Int = source.column(point)

  override def toString =
    if (source.exists) s"${source.file}:${line + 1}"
    else s"(no source file, offset = ${pos.point})"
}

/** A sentinel for a non-existing source position */
object NoSourcePosition extends SourcePosition(NoSource, NoPosition) {
  override def toString = "?"
}

