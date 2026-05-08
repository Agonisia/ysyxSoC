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
  private val cmdTerminate = "b0110".U(4.W)
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
    val modeReg = RegInit(0.U(13.W))
    val readActive = RegInit(false.B)
    val readDelay = RegInit(0.U(3.W))
    val readBeatsLeft = RegInit(0.U(4.W))
    val readAddr = RegInit(0.U(24.W))
    val writeActive = RegInit(false.B)
    val writeBeatsLeft = RegInit(0.U(4.W))
    val writeAddr = RegInit(0.U(24.W))

    val cmd = Cat(io.cs, io.ras, io.cas, io.we)
    val currentAddr = Cat(activeRow(io.ba), io.ba, io.a(8, 0))
    val burstBeats = MuxLookup(modeReg(2, 0), 1.U(4.W))(Seq(
      "b000".U -> 1.U(4.W),
      "b001".U -> 2.U(4.W),
      "b010".U -> 4.U(4.W),
      "b011".U -> 8.U(4.W)
    ))
    val casLatency = MuxLookup(modeReg(6, 4), 2.U(3.W))(Seq(
      "b001".U -> 1.U(3.W),
      "b010".U -> 2.U(3.W),
      "b011".U -> 3.U(3.W)
    ))
    val readDelayStart = Mux(casLatency > 1.U, casLatency - 2.U, 0.U)
    val extraWriteBeats = burstBeats - 1.U

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

    dqOutEnableReg := false.B

    when (readActive) {
      when (readDelay =/= 0.U) {
        readDelay := readDelay - 1.U
      } .otherwise {
        dqOutReg := mem.read(readAddr)
        dqOutEnableReg := true.B
        readAddr := readAddr + 1.U
        when (readBeatsLeft <= 1.U) {
          readActive := false.B
        } .otherwise {
          readBeatsLeft := readBeatsLeft - 1.U
        }
      }
    }

    when (writeActive) {
      writeHalfword(writeAddr, dqIn, io.dqm)
      writeAddr := writeAddr + 1.U
      when (writeBeatsLeft <= 1.U) {
        writeActive := false.B
      } .otherwise {
        writeBeatsLeft := writeBeatsLeft - 1.U
      }
    }

    switch (cmd) {
      is (cmdLoadMode) {
        modeReg := io.a
        readActive := false.B
        writeActive := false.B
      }
      is (cmdActive) {
        activeRow(io.ba) := io.a
      }
      is (cmdRead) {
        readActive := true.B
        readDelay := readDelayStart
        readBeatsLeft := burstBeats
        readAddr := currentAddr
        writeActive := false.B
      }
      is (cmdWrite) {
        writeHalfword(currentAddr, dqIn, io.dqm)
        writeAddr := currentAddr + 1.U
        writeBeatsLeft := extraWriteBeats
        writeActive := burstBeats =/= 1.U
        readActive := false.B
      }
      is (cmdTerminate) {
        readActive := false.B
        writeActive := false.B
      }
      is (cmdPrecharge) {
        readActive := false.B
        writeActive := false.B
      }
      is (cmdRefresh) {
        readActive := false.B
        writeActive := false.B
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
