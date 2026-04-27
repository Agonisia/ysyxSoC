package ysyx

import chisel3._
import chisel3.util._

class bitrev extends BlackBox {
  val io = IO(Flipped(new SPIIO(1)))
}

class bitrevChisel extends RawModule { // we do not need clock and reset
  val io = IO(Flipped(new SPIIO(1)))

  io.miso := true.B

  withClockAndReset(io.sck.asClock, io.ss(0).asAsyncReset) {
    val bitCnt = RegInit(0.U(3.W))
    val rxShift = RegInit(0.U(8.W))
    val txShift = RegInit("hff".U(8.W))

    val rxNext = Cat(rxShift(6, 0), io.mosi)
    val reversedRxNext = Reverse(rxNext)

    rxShift := rxNext
    txShift := Cat(txShift(6, 0), true.B)

    when (bitCnt === 7.U) {
      bitCnt := 0.U
      txShift := reversedRxNext
    } .otherwise {
      bitCnt := bitCnt + 1.U
    }

    io.miso := Mux(io.ss(0), true.B, txShift(7))
  }
}
