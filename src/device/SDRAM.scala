package ysyx

import chisel3._
import chisel3.util._
import chisel3.experimental.{Analog, attach}

import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.amba.apb._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util._

class SDRAMIO extends Bundle {
  val clk = Output(Bool())
  val cke = Output(Bool())
  val cs  = Output(UInt(2.W))
  val ras = Output(Bool())
  val cas = Output(Bool())
  val we  = Output(Bool())
  val a   = Output(UInt(13.W))
  val ba  = Output(UInt(2.W))
  val dqm = Output(UInt(4.W))
  val dq  = Analog(32.W)
}

object SDRAMIO {
  def connect(sink: SDRAMIO, source: SDRAMIO): Unit = {
    sink.clk := source.clk
    sink.cke := source.cke
    sink.cs := source.cs
    sink.ras := source.ras
    sink.cas := source.cas
    sink.we := source.we
    sink.a := source.a
    sink.ba := source.ba
    sink.dqm := source.dqm
    attach(sink.dq, source.dq)
  }
}

class sdram_top_axi extends BlackBox {
  val io = IO(new Bundle {
    val clock = Input(Clock())
    val reset = Input(Bool())
    val in = Flipped(new AXI4Bundle(AXI4BundleParameters(addrBits = 32, dataBits = 32, idBits = 4)))
    val sdram = new SDRAMIO
  })
}

class sdram extends BlackBox {
  val io = IO(Flipped(new SDRAMIO))
}

class sdramChisel extends RawModule {
  val io = IO(Flipped(new SDRAMIO))

  private val ranks = 2
  private val rowBits = 12
  private val colBits = 9
  private val bankBits = 2
  private val wordAddrBits = rowBits + bankBits + colBits
  private val wordsPerRank = 1 << wordAddrBits
  private val totalWords = ranks * wordsPerRank

  private val cmdActive = "b0011".U(4.W)
  private val cmdRead = "b0101".U(4.W)
  private val cmdWrite = "b0100".U(4.W)
  private val cmdTerminate = "b0110".U(4.W)
  private val cmdPrecharge = "b0010".U(4.W)
  private val cmdRefresh = "b0001".U(4.W)
  private val cmdLoadMode = "b0000".U(4.W)

  val dqOut = WireDefault(0.U(32.W))
  val dqOutEnable = WireDefault(false.B)
  val dqIn = TriStateInBuf(io.dq, dqOut, dqOutEnable)

  withClockAndReset(io.clk.asClock, (!io.cke).asAsyncReset) {
    val mem = Mem(totalWords, UInt(32.W))
    val activeRow = RegInit(VecInit(Seq.fill(ranks)(VecInit(Seq.fill(4)(0.U(rowBits.W))))))
    val dqOutReg = RegInit(0.U(32.W))
    val dqOutEnableReg = RegInit(false.B)
    val modeReg = RegInit(0.U(13.W))
    val readActive = RegInit(false.B)
    val readDelay = RegInit(0.U(3.W))
    val readBeatsLeft = RegInit(0.U(4.W))
    val readRank = RegInit(0.U(1.W))
    val readAddr = RegInit(0.U(wordAddrBits.W))
    val writeActive = RegInit(false.B)
    val writeBeatsLeft = RegInit(0.U(4.W))
    val writeRank = RegInit(0.U(1.W))
    val writeAddr = RegInit(0.U(wordAddrBits.W))

    val rankActive = VecInit((0 until ranks).map(i => !io.cs(i).asBool))
    val activeCount = PopCount(rankActive)
    val anyRankActive = activeCount =/= 0.U
    val selectedRankValid = activeCount === 1.U
    val selectedRank = Mux(rankActive(1), 1.U(1.W), 0.U(1.W))
    val cmd = Cat(!anyRankActive, io.ras, io.cas, io.we)
    val currentAddr = Cat(activeRow(selectedRank)(io.ba), io.ba, io.a(colBits - 1, 0))
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

    def absAddr(rank: UInt, index: UInt): UInt = Cat(rank, index)

    def writeWord(rank: UInt, index: UInt, data: UInt, mask: UInt): Unit = {
      val oldData = mem.read(absAddr(rank, index))
      val nextBytes = Wire(Vec(4, UInt(8.W)))
      for (i <- 0 until 4) {
        nextBytes(i) := Mux(!mask(i), data(8 * i + 7, 8 * i), oldData(8 * i + 7, 8 * i))
      }
      mem.write(absAddr(rank, index), Cat(nextBytes(3), nextBytes(2), nextBytes(1), nextBytes(0)))
    }

    dqOutEnableReg := false.B

    when (readActive) {
      when (readDelay =/= 0.U) {
        readDelay := readDelay - 1.U
      } .otherwise {
        val readData = mem.read(absAddr(readRank, readAddr))
        dqOutReg := readData
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
      writeWord(writeRank, writeAddr, dqIn, io.dqm)
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
        when (selectedRankValid) {
          activeRow(selectedRank)(io.ba) := io.a(rowBits - 1, 0)
        }
      }
      is (cmdRead) {
        when (selectedRankValid) {
          readActive := true.B
          readDelay := readDelayStart
          readBeatsLeft := burstBeats
          readRank := selectedRank
          readAddr := currentAddr
        }
        writeActive := false.B
      }
      is (cmdWrite) {
        when (selectedRankValid) {
          writeWord(selectedRank, currentAddr, dqIn, io.dqm)
          writeRank := selectedRank
          writeAddr := currentAddr + 1.U
          writeBeatsLeft := extraWriteBeats
          writeActive := burstBeats =/= 1.U
        } .otherwise {
          writeActive := false.B
        }
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

class APBSDRAMChisel extends Module {
  val in = IO(Flipped(new APBBundle(APBBundleParameters(addrBits = 32, dataBits = 32))))
  val sdram = IO(new SDRAMIO)

  private val modeRegBurst1Cas2 = (2 << 4).U(13.W)

  private val (sPowerUp :: sLoadMode :: sIdle :: sActive :: sTrcd ::
    sRead :: sReadWait :: sReadCapture :: sWrite :: sResp :: Nil) = Enum(10)

  val state = RegInit(sPowerUp)
  val reqAddr = RegInit(0.U(32.W))
  val reqWrite = RegInit(false.B)
  val reqWdata = RegInit(0.U(32.W))
  val reqStrb = RegInit(0.U(4.W))
  val readData = RegInit(0.U(32.W))

  val dqOut = WireDefault(reqWdata)
  val dqOutEnable = WireDefault(state === sWrite)
  val dqIn = TriStateInBuf(sdram.dq, dqOut, dqOutEnable)

  val apbSetup = in.psel && !in.penable
  val apbAccess = in.psel && in.penable
  val rank = reqAddr(25)
  val row = reqAddr(24, 13)
  val bank = reqAddr(12, 11)
  val col = reqAddr(10, 2)
  val rankCs = Mux(rank.asBool, "b01".U(2.W), "b10".U(2.W))

  in.pready := state === sResp
  in.prdata := readData
  in.pslverr := false.B

  sdram.clk := clock.asBool
  sdram.cke := state =/= sPowerUp
  sdram.cs := "b11".U
  sdram.ras := true.B
  sdram.cas := true.B
  sdram.we := true.B
  sdram.a := 0.U
  sdram.ba := 0.U
  sdram.dqm := 0.U

  switch (state) {
    is (sLoadMode) {
      sdram.cs := "b00".U
      sdram.ras := false.B
      sdram.cas := false.B
      sdram.we := false.B
      sdram.a := modeRegBurst1Cas2
    }
    is (sActive) {
      sdram.cs := rankCs
      sdram.ras := false.B
      sdram.cas := true.B
      sdram.we := true.B
      sdram.a := row
      sdram.ba := bank
    }
    is (sRead) {
      sdram.cs := rankCs
      sdram.ras := true.B
      sdram.cas := false.B
      sdram.we := true.B
      sdram.a := col
      sdram.ba := bank
      sdram.dqm := 0.U
    }
    is (sWrite) {
      sdram.cs := rankCs
      sdram.ras := true.B
      sdram.cas := false.B
      sdram.we := false.B
      sdram.a := col
      sdram.ba := bank
      sdram.dqm := ~reqStrb
    }
  }

  switch (state) {
    is (sPowerUp) {
      state := sLoadMode
    }
    is (sLoadMode) {
      state := sIdle
    }
    is (sIdle) {
      when (apbSetup) {
        reqAddr := in.paddr
        reqWrite := in.pwrite
        reqWdata := in.pwdata
        reqStrb := in.pstrb
        state := sActive
      }
    }
    is (sActive) {
      state := sTrcd
    }
    is (sTrcd) {
      state := Mux(reqWrite, sWrite, sRead)
    }
    is (sRead) {
      state := sReadWait
    }
    is (sReadWait) {
      state := sReadCapture
    }
    is (sReadCapture) {
      readData := dqIn
      state := sResp
    }
    is (sWrite) {
      state := sResp
    }
    is (sResp) {
      when (apbAccess || !in.psel) {
        state := sIdle
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
    SDRAMIO.connect(sdram_bundle, msdram.io.sdram)
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

    val msdram = Module(new APBSDRAMChisel)
    msdram.in <> in
    SDRAMIO.connect(sdram_bundle, msdram.sdram)
  }
}
