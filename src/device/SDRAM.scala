package ysyx

import chisel3._
import chisel3.util._
import chisel3.experimental.Analog

import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.amba.apb._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util._

class SDRAMIO extends Bundle {
  val clk = Output(Bool())
  val cke = Output(Bool())
  val cs  = Output(Bool())
  val ras = Output(Bool())
  val cas = Output(Bool())
  val we  = Output(Bool())
  val a   = Output(UInt(13.W))
  val ba  = Output(UInt(2.W))
  val dqm = Output(UInt(2.W))
  val dq  = Analog(16.W)
}

class sdram_top_axi extends BlackBox {
  val io = IO(new Bundle {
    val clock = Input(Clock())
    val reset = Input(Bool())
    val in = Flipped(new AXI4Bundle(AXI4BundleParameters(addrBits = 32, dataBits = 32, idBits = 4)))
    val sdram = new SDRAMIO
  })
}

class sdram_top_apb extends BlackBox {
  val io = IO(new Bundle {
    val clock = Input(Clock())
    val reset = Input(Bool())
    val in = Flipped(new APBBundle(APBBundleParameters(addrBits = 32, dataBits = 32)))
    val sdram = new SDRAMIO
  })
}

class sdram extends BlackBox {
  val io = IO(Flipped(new SDRAMIO))
}

class sdramChisel extends RawModule {
  val io = IO(Flipped(new SDRAMIO))

  private val halfwords = 16 * 1024 * 1024

  private val cmdActive = "b0011".U(4.W)
  private val cmdRead = "b0101".U(4.W)
  private val cmdWrite = "b0100".U(4.W)
  private val cmdPrecharge = "b0010".U(4.W)
  private val cmdRefresh = "b0001".U(4.W)
  private val cmdLoadMode = "b0000".U(4.W)

  val dqOut = WireDefault(0.U(16.W))
  val dqOutEnable = WireDefault(false.B)
  val dqIn = TriStateInBuf(io.dq, dqOut, dqOutEnable)

  withClockAndReset(io.clk.asClock, (!io.cke).asAsyncReset) {
    val mem = Mem(halfwords, UInt(16.W))
    val activeRow = RegInit(VecInit(Seq.fill(4)(0.U(13.W))))
    val dqOutReg = RegInit(0.U(16.W))
    val dqOutEnableReg = RegInit(false.B)
    val pendingWriteUpper = RegInit(false.B)
    val pendingReadStage = RegInit(0.U(2.W))
    val burstAddr = RegInit(0.U(24.W))
    val modeReg = RegInit(0.U(13.W))

    val cmd = Cat(io.cs, io.ras, io.cas, io.we)
    val currentAddr = Cat(activeRow(io.ba), io.ba, io.a(8, 0))

    dqOut := dqOutReg
    dqOutEnable := dqOutEnableReg

    def writeHalfword(index: UInt, data: UInt, mask: UInt): Unit = {
      val oldData = mem.read(index)
      val nextData = Cat(
        Mux(!mask(1), data(15, 8), oldData(15, 8)),
        Mux(!mask(0), data(7, 0), oldData(7, 0))
      )
      mem.write(index, nextData)
    }

    when (pendingWriteUpper) {
      writeHalfword(burstAddr + 1.U, dqIn, io.dqm)
      pendingWriteUpper := false.B
    }

    when (pendingReadStage === 1.U) {
      dqOutReg := mem.read(burstAddr)
      dqOutEnableReg := true.B
      pendingReadStage := 2.U
    } .elsewhen (pendingReadStage === 2.U) {
      dqOutReg := mem.read(burstAddr + 1.U)
      dqOutEnableReg := true.B
      pendingReadStage := 0.U
    } .otherwise {
      dqOutEnableReg := false.B
    }

    switch (cmd) {
      is (cmdLoadMode) {
        modeReg := io.a
      }
      is (cmdActive) {
        activeRow(io.ba) := io.a
      }
      is (cmdRead) {
        burstAddr := currentAddr
        pendingReadStage := 1.U
      }
      is (cmdWrite) {
        burstAddr := currentAddr
        writeHalfword(currentAddr, dqIn, io.dqm)
        pendingWriteUpper := true.B
      }
      is (cmdPrecharge) {
        pendingWriteUpper := false.B
        pendingReadStage := 0.U
      }
      is (cmdRefresh) {
        pendingWriteUpper := false.B
        pendingReadStage := 0.U
      }
    }
  }
}

class AXI4SDRAM(address: Seq[AddressSet])(implicit p: Parameters) extends LazyModule {
  val beatBytes = 4
  val node = AXI4SlaveNode(Seq(AXI4SlavePortParameters(
    Seq(AXI4SlaveParameters(
        address       = address,
        executable    = true,
        supportsWrite = TransferSizes(1, beatBytes),
        supportsRead  = TransferSizes(1, beatBytes),
        interleavedId = Some(0))
    ),
    beatBytes  = beatBytes)))

  lazy val module = new Impl
  class Impl extends LazyModuleImp(this) {
    val (in, _) = node.in(0)
    val sdram_bundle = IO(new SDRAMIO)

    val msdram = Module(new sdram_top_axi)
    msdram.io.clock := clock
    msdram.io.reset := reset.asBool
    msdram.io.in <> in
    sdram_bundle <> msdram.io.sdram
  }
}

class APBSDRAM(address: Seq[AddressSet])(implicit p: Parameters) extends LazyModule {
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
    val sdram_bundle = IO(new SDRAMIO)

    val msdram = Module(new sdram_top_apb)
    msdram.io.clock := clock
    msdram.io.reset := reset.asBool
    msdram.io.in <> in
    sdram_bundle <> msdram.io.sdram
  }
}
