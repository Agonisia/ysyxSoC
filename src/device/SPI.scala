package ysyx

import chisel3._
import chisel3.util._

import freechips.rocketchip.amba.apb._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util._

class SPIIO(val ssWidth: Int = 8) extends Bundle {
  val sck = Output(Bool())
  val ss = Output(UInt(ssWidth.W))
  val mosi = Output(Bool())
  val miso = Input(Bool())
}

class spi_top_apb extends BlackBox {
  val io = IO(new Bundle {
    val clock = Input(Clock())
    val reset = Input(Reset())
    val in = Flipped(new APBBundle(APBBundleParameters(addrBits = 32, dataBits = 32)))
    val spi = new SPIIO
    val spi_irq_out = Output(Bool())
  })
}

class flash extends BlackBox {
  val io = IO(Flipped(new SPIIO(1)))
}

class flash_cmd extends BlackBox {
  val io = IO(new Bundle {
    val clock = Input(Clock())
    val valid = Input(Bool())
    val cmd = Input(UInt(8.W))
    val addr = Input(UInt(32.W))
    val data = Output(UInt(32.W))
  })
}

class APBSPI(address: Seq[AddressSet])(implicit p: Parameters) extends LazyModule {
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
    val spi_bundle = IO(new SPIIO)

    val mspi = Module(new spi_top_apb)
    mspi.io.clock := clock
    mspi.io.reset := reset
    spi_bundle <> mspi.io.spi

    val paddr = Cat(0.U(2.W), in.paddr)
    val isFlash = paddr >= "h3000_0000".U && paddr <= "h3fff_ffff".U
    val flashSelected = in.psel && isFlash
    val flashRead = flashSelected && !in.penable && !in.pwrite

    val mflash = Module(new flash_cmd)
    mflash.io.clock := clock
    mflash.io.valid := flashRead
    mflash.io.cmd := "h03".U
    mflash.io.addr := paddr

    mspi.io.in.psel := in.psel && !isFlash
    mspi.io.in.penable := in.penable && !isFlash
    mspi.io.in.pwrite := in.pwrite
    mspi.io.in.paddr := paddr
    mspi.io.in.pprot := in.pprot
    mspi.io.in.pwdata := in.pwdata
    mspi.io.in.pstrb := in.pstrb

    in.pready := Mux(flashSelected, in.penable, mspi.io.in.pready)
    in.pslverr := Mux(flashSelected, in.pwrite && in.penable, mspi.io.in.pslverr)
    in.prdata := Mux(flashSelected, mflash.io.data, mspi.io.in.prdata)
  }
}
