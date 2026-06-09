package ysyx

import chisel3._
import chisel3.util._

import freechips.rocketchip.amba.apb._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util._

class VGAIO extends Bundle {
  val r = Output(UInt(8.W))
  val g = Output(UInt(8.W))
  val b = Output(UInt(8.W))
  val hsync = Output(Bool())
  val vsync = Output(Bool())
  val valid = Output(Bool())
}

class VGACtrlIO extends Bundle {
  val clock = Input(Clock())
  val reset = Input(Bool())
  val in = Flipped(new APBBundle(APBBundleParameters(addrBits = 32, dataBits = 32)))
  val vga = new VGAIO
}

class vgaChisel extends RawModule {
  val io = IO(new VGACtrlIO)

  withClockAndReset(io.clock, io.reset) {
    val hVisible = 640.U(10.W)
    val hFront = 16.U(10.W)
    val hSync = 96.U(10.W)
    val hTotal = 800.U(10.W)
    val vVisible = 480.U(10.W)
    val vFront = 10.U(10.W)
    val vSync = 2.U(10.W)
    val vTotal = 525.U(10.W)
    val vmemWords = 524288

    val framebuffer = Mem(vmemWords, Vec(4, UInt(8.W)))
    val hCount = RegInit(0.U(10.W))
    val vCount = RegInit(0.U(10.W))
    val wordIndex = io.in.paddr(20, 2)
    val writeFire = io.in.psel && !io.in.penable && io.in.pwrite
    val visible = hCount < hVisible && vCount < vVisible
    val displayRow = Cat(vCount, 0.U(9.W)) + Cat(0.U(2.W), vCount, 0.U(7.W))
    val displayIndex = displayRow(18, 0) + Cat(0.U(9.W), hCount)

    val apbReadBytes = framebuffer(wordIndex)
    val displayBytes = framebuffer(displayIndex)
    val apbReadData = Cat(apbReadBytes(3), apbReadBytes(2), apbReadBytes(1), apbReadBytes(0))
    val displayPixel = Cat(displayBytes(3), displayBytes(2), displayBytes(1), displayBytes(0))
    val writeBytes = Wire(Vec(4, UInt(8.W)))
    for (i <- 0 until 4) {
      writeBytes(i) := io.in.pwdata(8 * i + 7, 8 * i)
    }

    when (hCount === hTotal - 1.U) {
      hCount := 0.U
      when (vCount === vTotal - 1.U) {
        vCount := 0.U
      } .otherwise {
        vCount := vCount + 1.U
      }
    } .otherwise {
      hCount := hCount + 1.U
    }

    when (writeFire) {
      framebuffer.write(wordIndex, writeBytes, io.in.pstrb.asBools)
    }

    io.in.pready := true.B
    io.in.pslverr := false.B
    io.in.prdata := apbReadData
    io.vga.valid := visible
    io.vga.hsync := !((hCount >= hVisible + hFront) && (hCount < hVisible + hFront + hSync))
    io.vga.vsync := !((vCount >= vVisible + vFront) && (vCount < vVisible + vFront + vSync))
    io.vga.r := Mux(visible, displayPixel(23, 16), 0.U)
    io.vga.g := Mux(visible, displayPixel(15, 8), 0.U)
    io.vga.b := Mux(visible, displayPixel(7, 0), 0.U)
  }
}

class APBVGA(address: Seq[AddressSet])(implicit p: Parameters) extends LazyModule {
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
    val vga_bundle = IO(new VGAIO)

    val mvga = Module(new vgaChisel)
    mvga.io.clock := clock
    mvga.io.reset := reset
    mvga.io.in <> in
    vga_bundle <> mvga.io.vga
  }
}
