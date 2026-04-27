package ysyx

import chisel3._
import chisel3.util._
import chisel3.experimental.Analog

import freechips.rocketchip.amba.apb._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util._

class QSPIIO extends Bundle {
  val sck = Output(Bool())
  val ce_n = Output(Bool())
  val dio = Analog(4.W)
}

class psram_top_apb extends BlackBox {
  val io = IO(new Bundle {
    val clock = Input(Clock())
    val reset = Input(Reset())
    val in = Flipped(new APBBundle(APBBundleParameters(addrBits = 32, dataBits = 32)))
    val qspi = new QSPIIO
  })
}

class psram extends BlackBox {
  val io = IO(Flipped(new QSPIIO))
}

class psramChisel extends RawModule {
  val io = IO(Flipped(new QSPIIO))
  private val memBytes = 4 * 1024 * 1024
  private val addrMask = (memBytes - 1).U(22.W)

  val dioOut = WireDefault(0.U(4.W))
  val dioOutEnable = WireDefault(false.B)
  val dioIn = TriStateInBuf(io.dio, dioOut, dioOutEnable)

  withClockAndReset(io.sck.asClock, io.ce_n.asAsyncReset) {
    val mem = Mem(memBytes, UInt(8.W))
    val phase = RegInit(0.U(8.W))
    val cmdShift = RegInit(0.U(8.W))
    val cmd = RegInit(0.U(8.W))
    val addr = RegInit(0.U(24.W))
    val writeHi = RegInit(0.U(4.W))

    val readDelta = phase - 21.U
    val readByteOffset = readDelta(7, 1)
    val readAddr = (addr(21, 0) + readByteOffset) & addrMask
    val readByte = mem.read(readAddr)
    val readPhase = !io.ce_n && cmd === "heb".U && phase >= 21.U

    val writeDelta = phase - 14.U
    val writeByteOffset = writeDelta(7, 1)
    val writeAddr = (addr(21, 0) + writeByteOffset) & addrMask

    dioOut := Mux(phase(0), readByte(7, 4), readByte(3, 0))
    dioOutEnable := readPhase

    when (phase < 8.U) {
      cmdShift := Cat(cmdShift(6, 0), dioIn(0))
      when (phase === 7.U) {
        cmd := Cat(cmdShift(6, 0), dioIn(0))
      }
    } .elsewhen (phase < 14.U) {
      addr := Cat(addr(19, 0), dioIn)
    } .elsewhen (cmd === "h38".U) {
      when (!phase(0)) {
        writeHi := dioIn
      } .otherwise {
        mem.write(writeAddr, Cat(writeHi, dioIn))
      }
    }

    phase := phase + 1.U
  }
}

class APBPSRAM(address: Seq[AddressSet])(implicit p: Parameters) extends LazyModule {
  val node = APBSlaveNode(Seq(APBSlavePortParameters(
    Seq(APBSlaveParameters(
      address       = address,
      executable    = true,
      supportsRead  = true,
      supportsWrite = true)),
    beatBytes  = 4)))

  lazy val module = new Impl
  class Impl extends LazyModuleImp(this) {
    val (in, _) = node.in(0)
    val qspi_bundle = IO(new QSPIIO)

    val mpsram = Module(new psram_top_apb)
    mpsram.io.clock := clock
    mpsram.io.reset := reset
    mpsram.io.in <> in
    qspi_bundle <> mpsram.io.qspi
  }
}
