package ysyx

import chisel3._
import org.chipsalliance.cde.config.{Parameters, Config}
import freechips.rocketchip.system._
import freechips.rocketchip.diplomacy.LazyModule

object Config {
  private def envFlag(name: String): Boolean =
    sys.env.get(name).exists(value => value == "1" || value.equalsIgnoreCase("true"))

  def hasChipLink: Boolean = envFlag("YSYXSOC_CHIPLINK")
  def sdramUseAXI: Boolean = envFlag("YSYXSOC_SDRAM_AXI")
}

class ysyxSoCTop extends Module {
  implicit val config: Parameters = new Config(new Edge32BitConfig ++ new DefaultRV32Config)

  val io = IO(new Bundle { })
  val dut = LazyModule(new ysyxSoCFull)
  val mdut = Module(dut.module)
  mdut.dontTouchPorts()
  mdut.externalPins := DontCare
  mdut.externalPins.uart.rx := true.B
}

object Elaborate extends App {
  val firtoolOptions = Array("--disable-annotation-unknown")
  circt.stage.ChiselStage.emitSystemVerilogFile(new ysyxSoCTop, args, firtoolOptions)
}
