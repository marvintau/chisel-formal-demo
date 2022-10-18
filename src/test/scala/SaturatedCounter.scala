
package pht

import chisel3._
import chisel3.util._

import chiseltest._
import chiseltest.formal._

import org.scalatest.flatspec.AnyFlatSpec
import chisel3.experimental.BundleLiterals._

trait Config {

  val XLEN = 32
  val DATA_BITS  = XLEN 
  val DATA_BYTES = XLEN / 8

  val PADDING_LEN = 2
  val VIRT_MEM_ADDR_LEN = 32 
  val PHYS_ADDR_LEN = 32

  // ADDR_BITS is used at ALU ouput, see ArithLogic module
  val ADDR_BITS = 64

  val M_MODE_ONLY = true
  val DIV_SUPPORT = true

  val RV_COMPRESS = false
}

// Formerly ALUOpType
object ALUOperType {
  def isBranch(func: UInt) = !func(3)
}

// Formerly FuOpType
object OperType {
  def apply() = UInt(7.W)
}

// An entry address data structure to let you access segments in the midst
class EntryAddr(val idxBits: Int) extends Bundle with Config {

  // val padLen = if (Settings.get("IsRV32") || !Settings.get("EnableOutOfOrderExec")) 2 else 3
  val padLen = 2
  def tagBits = VIRT_MEM_ADDR_LEN - padLen - idxBits

  //val res = UInt((AddrBits - VAddrBits).W)
  val tag = UInt(tagBits.W)
  val idx = UInt(idxBits.W)
  val pad = UInt(padLen.W)

  def fromUInt(x: UInt) = x.asTypeOf(UInt(VIRT_MEM_ADDR_LEN.W)).asTypeOf(this)
  def getTag(x: UInt) = fromUInt(x).tag
  def getIdx(x: UInt) = fromUInt(x).idx
}

// Pattern History Table / PHT responds for determining whether to
// take branch or not, based on the most recent two records.
// 
// PHT is embedded into original BPU, and excerpted out. 
// https://github.com/OSCPU/NutShell/blob/fd86beadfc47f52973270ce6109edebd2a30363b/src/main/scala/nutcore/frontend/BPU.scala#L112

// Docs can be found at:
// https://oscpu.gitbook.io/nutshell/gong-neng-bu-jian-she-ji-xi-jie/bpu#geng-xin-ji-zhi


class FeedbackPort extends Bundle with Config {

  val valid = Output(Bool())

  val pc                = Output(UInt(VIRT_MEM_ADDR_LEN.W))
  // val isMispredicted    = Output(Bool())
  // val branchDest        = Output(UInt(VIRT_MEM_ADDR_LEN.W))
  val operType          = Output(OperType())
  // val branchType        = Output(BranchType())
  val branchTaken       = Output(Bool())  // for branch

  val compressed        = Output(Bool())  // for ras, save PC+2 to stack if is RVC
}


class PredictPort extends Bundle {
  val flush = Input(Bool())
  val feed  = Flipped(new FeedbackPort())
  val pc    = Flipped(Valid(UInt(32.W)))
}

class PatternHistory(val entryCount: Int, val addr: EntryAddr) extends Module {

  val io = IO(new Bundle {

    val in = new PredictPort()

    val out = Output(Bool())
    // val out = Output(UInt(2.W))

    // for debugging output
    val feedback = new FeedbackPort()

  })
 
  // Mem: Async Read (curr-cycle), Sync Write (next-cycle)
  // Notes on RAW Hazard issue: Mem is synced to be register file/bank. Read through wire, and Update 
  // at positive edge. Thus read always after write in the same cycle.
  val mem = Mem(entryCount, UInt(2.W))
 
  val currCount = RegNext(mem.read(addr.getIdx(io.in.feed.pc)))
  val feedback  = RegNext(io.in.feed)

  val taken = feedback.branchTaken
  // val nextCount = Mux(taken, currCount + 1.U, currCount - 1.U)
  // val shouldUpdate = (taken && (currCount =/= "b11".U)) || (!taken && (currCount =/= "b00".U))

  val nextCount = Mux(taken, 
    Mux(currCount === "b11".U, "b11".U, currCount + 1.U),
    Mux(currCount === "b00".U, "b00".U, currCount - 1.U),
  )

  when (feedback.valid && ALUOperType.isBranch(feedback.operType) ) {
    
    // when (shouldUpdate) {
    //   mem.write(addr.getIdx(feedback.pc), nextCount)
    // }
    mem.write(addr.getIdx(feedback.pc), nextCount)

  }

  // ========================================================================================
  // FORMAL VERIFICATION 
  // 
  // If the feedback request arrives, and contains branching instruction, then the history table will
  // be updated.

  // This means, if feedback arrives 2 cycles ago, the memory will be updated at the past 1 cycle. If
  // there is no new feedback arriving, we will be able to read the result at the current cycle.

  // assert(past(feedback) === RegNext(feedback))

  
  when (past(feedback.valid) && ALUOperType.isBranch(past(feedback.operType))){

    val memReadPrev = mem.read(addr.getIdx(past(feedback.pc)))
    
    when (past(feedback).branchTaken) {
      when (past(currCount) =/= "b11".U) {
        assert(memReadPrev === past(currCount) + 1.U)
      }.otherwise {
        assert(memReadPrev === "b11".U)
      }
    }.otherwise {
      when (past(currCount) =/= "b00".U) {
        assert(memReadPrev === past(currCount) - 1.U)
      }.otherwise {
        assert(memReadPrev === "b00".U)
      }
    }
  }

  io.out := RegEnable(mem.read(addr.getIdx(io.in.pc.bits))(1), io.in.pc.valid)
  
  io.feedback <> feedback
}




class FormalPHTSpec extends AnyFlatSpec with ChiselScalatestTester with Formal {
  "PHT" should "pass" in {

    val PHT_ENTRY_NUM = 4
    val TARG_BUFFER_ENTRY_NUM = 8 

    verify(new PatternHistory(PHT_ENTRY_NUM, new EntryAddr(TARG_BUFFER_ENTRY_NUM)), Seq(BoundedCheck(20)))
  }
}