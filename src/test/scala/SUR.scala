
package sur

import chisel3._
import chisel3.util._

import chiseltest._
import chiseltest.formal._
import org.scalatest.flatspec.AnyFlatSpec
import chisel3.experimental.BundleLiterals._

// Stale-Update-Revalidate
object SUR{
  def apply[T <: Data](validate: Bool, invalidate: Bool, incoming: T) = {
    
    val switch = RegInit(false.B)
    when (validate) {
      switch := true.B
    }
    when (invalidate) {
      switch := false.B
    }
    val stored = RegEnable(incoming, validate)

    (switch || validate, Mux(validate, incoming, stored))
  }

} 

class SUR extends Module {
  val io = IO(new Bundle {
    val validate = Input(Bool())
    val invalidate = Input(Bool())

    val input = Input(UInt(32.W))
    val output = Output(UInt(32.W))

    val isCurrValid = Output(Bool())
  })

  val (isCurrValid, output) = SUR(
    io.validate,
    io.invalidate,
    io.input
  )

  // When currently validated
  when(io.validate) {    
    // the incoming data overrides stored data even if invalidate signal
    // arrive at the same time.
    assert(isCurrValid === true.B)
  }
  // When currently not but previously validated
  .elsewhen(past(io.invalidate)) {
    assert(isCurrValid === false.B)
    // assert(output === past(io.input))
  }
  .elsewhen(!past(io.validate)) {
    assert(isCurrValid === past(isCurrValid))
  }.otherwise {
    assert(isCurrValid === true.B)
  }

  when(io.validate) {
    assert(output === io.input)
  }.elsewhen(past(io.validate)){
    assert(output === past(io.input))
  }.elsewhen(past(io.validate, 2)){
    assert(output === past(io.input, 2))
  }

  io.isCurrValid := isCurrValid
  io.output := output
}

class FormalSURSpec extends AnyFlatSpec with ChiselScalatestTester with Formal {
  "SUR" should "pass" in {
    verify(new SUR(), Seq(BoundedCheck(20)))
  }
}