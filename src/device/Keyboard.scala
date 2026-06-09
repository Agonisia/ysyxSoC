package ysyx

import chisel3._
import chisel3.util._

import freechips.rocketchip.amba.apb._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util._

class PS2IO extends Bundle {
  val clk = Input(Bool())
  val data = Input(Bool())
}

class PS2CtrlIO extends Bundle {
  val clock = Input(Clock())
  val reset = Input(Bool())
  val in = Flipped(new APBBundle(APBBundleParameters(addrBits = 32, dataBits = 32)))
  val ps2 = new PS2IO
}

class ps2Chisel extends RawModule {
  val io = IO(new PS2CtrlIO)

  withClockAndReset(io.clock, io.reset) {
    val fifoDepth = 16
    val ps2ClkSync = RegInit("b111".U(3.W))
    val ps2DataSync = RegInit("b111".U(3.W))
    val bitCount = RegInit(0.U(4.W))
    val dataShift = RegInit(VecInit(Seq.fill(8)(false.B)))
    val parityBit = RegInit(false.B)
    val fifo = Reg(Vec(fifoDepth, UInt(8.W)))
    val rdPtr = RegInit(0.U(4.W))
    val wrPtr = RegInit(0.U(4.W))
    val fifoCount = RegInit(0.U(5.W))
    val readDataReg = RegInit(0.U(8.W))

    val regAddr = io.in.paddr(3, 2)
    val dataReadFire = io.in.psel && !io.in.penable && !io.in.pwrite && regAddr === 0.U
    val popFire = dataReadFire && fifoCount =/= 0.U
    val ps2ClkFalling = ps2ClkSync(2, 1) === "b10".U
    val ps2DataSample = ps2DataSync(2)
    val frameDone = ps2ClkFalling && bitCount === 10.U
    val frameValid = frameDone && ps2DataSample && Cat(parityBit, dataShift.asUInt).xorR
    val pushFire = frameValid && (fifoCount =/= fifoDepth.U || popFire)

    ps2ClkSync := Cat(ps2ClkSync(1, 0), io.ps2.clk)
    ps2DataSync := Cat(ps2DataSync(1, 0), io.ps2.data)

    when (dataReadFire) {
      readDataReg := Mux(popFire, fifo(rdPtr), 0.U)
    }

    when (popFire) {
      rdPtr := rdPtr + 1.U
    }

    when (pushFire) {
      fifo(wrPtr) := dataShift.asUInt
      wrPtr := wrPtr + 1.U
    }

    switch (Cat(pushFire, popFire)) {
      is ("b10".U) { fifoCount := fifoCount + 1.U }
      is ("b01".U) { fifoCount := fifoCount - 1.U }
    }

    when (ps2ClkFalling) {
      switch (bitCount) {
        is (0.U) {
          when (!ps2DataSample) {
            bitCount := 1.U
          }
        }
        is (1.U, 2.U, 3.U, 4.U, 5.U, 6.U, 7.U, 8.U) {
          dataShift(bitCount(2, 0) - 1.U) := ps2DataSample
          bitCount := bitCount + 1.U
        }
        is (9.U) {
          parityBit := ps2DataSample
          bitCount := 10.U
        }
        is (10.U) {
          bitCount := 0.U
        }
      }
    }

    io.in.pready := true.B
    io.in.pslverr := false.B
    io.in.prdata := MuxLookup(regAddr, 0.U(32.W))(Seq(
      0.U -> Cat(0.U(24.W), readDataReg),
      1.U -> Cat(0.U(27.W), fifoCount)
    ))
  }
}

class APBKeyboard(address: Seq[AddressSet])(implicit p: Parameters) extends LazyModule {
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
    val ps2_bundle = IO(new PS2IO)

    val mps2 = Module(new ps2Chisel)
    mps2.io.clock := clock
    mps2.io.reset := reset
    mps2.io.in <> in
    ps2_bundle <> mps2.io.ps2
  }
}
