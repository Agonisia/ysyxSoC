package ysyx

import chisel3._
import chisel3.util._

import freechips.rocketchip.amba.apb._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util._

class GPIOIO extends Bundle {
  val out = Output(UInt(16.W))
  val in = Input(UInt(16.W))
  val seg = Output(Vec(8, UInt(8.W)))
}

class GPIOCtrlIO extends Bundle {
  val clock = Input(Clock())
  val reset = Input(Reset())
  val in = Flipped(new APBBundle(APBBundleParameters(addrBits = 32, dataBits = 32)))
  val gpio = new GPIOIO
}

class gpioChisel extends RawModule {
  val io = IO(new GPIOCtrlIO)

  withClockAndReset(io.clock, io.reset) {
    val gpioOut = RegInit(0.U(16.W))
    val gpioSeg = RegInit(0.U(32.W))
    val regAddr = io.in.paddr(3, 2)
    val writeFire = io.in.psel && !io.in.penable && io.in.pwrite

    def applyWstrb(oldData: UInt, newData: UInt, strobe: UInt): UInt = {
      val nextBytes = Wire(Vec(4, UInt(8.W)))
      for (i <- 0 until 4) {
        nextBytes(i) := Mux(strobe(i), newData(8 * i + 7, 8 * i), oldData(8 * i + 7, 8 * i))
      }
      Cat(nextBytes(3), nextBytes(2), nextBytes(1), nextBytes(0))
    }

    def hexToSeg(hex: UInt): UInt = {
      VecInit(Seq(
        "h03".U(8.W), "h9f".U(8.W), "h25".U(8.W), "h0d".U(8.W),
        "h99".U(8.W), "h49".U(8.W), "h41".U(8.W), "h1f".U(8.W),
        "h01".U(8.W), "h09".U(8.W), "h11".U(8.W), "hc1".U(8.W),
        "h63".U(8.W), "h85".U(8.W), "h61".U(8.W), "h71".U(8.W)
      ))(hex)
    }

    when (writeFire) {
      switch (regAddr) {
        is (0.U) {
          gpioOut := Cat(
            Mux(io.in.pstrb(1), io.in.pwdata(15, 8), gpioOut(15, 8)),
            Mux(io.in.pstrb(0), io.in.pwdata(7, 0), gpioOut(7, 0))
          )
        }
        is (2.U) {
          gpioSeg := applyWstrb(gpioSeg, io.in.pwdata, io.in.pstrb)
        }
      }
    }

    io.in.pready := true.B
    io.in.pslverr := false.B
    io.in.prdata := MuxLookup(regAddr, 0.U(32.W))(Seq(
      0.U -> Cat(0.U(16.W), gpioOut),
      1.U -> Cat(0.U(16.W), io.gpio.in),
      2.U -> gpioSeg
    ))

    io.gpio.out := gpioOut
    for (i <- 0 until 8) {
      io.gpio.seg(i) := hexToSeg(gpioSeg(4 * i + 3, 4 * i))
    }
  }
}

class APBGPIO(address: Seq[AddressSet])(implicit p: Parameters) extends LazyModule {
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
    val gpio_bundle = IO(new GPIOIO)

    val mgpio = Module(new gpioChisel)
    mgpio.io.clock := clock
    mgpio.io.reset := reset
    mgpio.io.in <> in
    gpio_bundle <> mgpio.io.gpio
  }
}
