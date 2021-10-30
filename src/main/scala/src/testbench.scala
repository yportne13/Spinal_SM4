package SM4

import spinal.core._
import spinal.lib._
import spinal.core.sim._
import spinal.sim._

object testbench extends App {
  SimConfig.withWave.doSim(new toptest){dut =>
    //Fork a process to generate the reset and the clock on the dut
    dut.clockDomain.forkStimulus(period = 10)

    var modelState = 0
    for(idx <- 0 to 800){
      //Drive the dut inputs with random values
      //dut.io.input.payload.MK(0) #= 0x01234567L
      //dut.io.input.payload.MK(1) #= 0x89ABCDEFL
      //dut.io.input.payload.MK(2) #= 0xFEDCBA98L
      //dut.io.input.payload.MK(3) #= 0x76543210L
      if(idx == 2) {
        dut.io.input.valid #= true
        dut.io.input.payload.X(0) #= 0x01234567L
        dut.io.input.payload.X(1) #= 0x89ABCDEFL
        dut.io.input.payload.X(2) #= 0xFEDCBA98L
        dut.io.input.payload.X(3) #= 0x76543210L
        dut.io.input.payload.MK(0) #= 0x01234567L
        dut.io.input.payload.MK(1) #= 0x89ABCDEFL
        dut.io.input.payload.MK(2) #= 0xFEDCBA98L
        dut.io.input.payload.MK(3) #= 0x76543210L
      }else if(idx == 3) {
        dut.io.input.valid #= true
        dut.io.input.payload.X(0) #= 0x797F8D29L
        dut.io.input.payload.X(1) #= 0x2553949BL
        dut.io.input.payload.X(2) #= 0x390F98C6L
        dut.io.input.payload.X(3) #= 0xE648DAAAL
        dut.io.input.payload.MK(0) #= 0x5FC2A1E2L
        dut.io.input.payload.MK(1) #= 0x8F1AE362L
        dut.io.input.payload.MK(2) #= 0xE9941B2AL
        dut.io.input.payload.MK(3) #= 0x4008C31EL
      }else {
        dut.io.input.valid #= false
        dut.io.input.payload.X(0) #= 0//0x01234567L
        dut.io.input.payload.X(1) #= 0//0x89ABCDEFL
        dut.io.input.payload.X(2) #= 0//0xFEDCBA98L
        dut.io.input.payload.X(3) #= 0//0x76543210L
        dut.io.input.payload.MK(0) #= 0//0x01234567L
        dut.io.input.payload.MK(1) #= 0//0x89ABCDEFL
        dut.io.input.payload.MK(2) #= 0//0xFEDCBA98L
        dut.io.input.payload.MK(3) #= 0//0x76543210L
      }

      //Wait a rising edge on the clock
      dut.clockDomain.waitRisingEdge()

      //Check that the dut values match with the reference model ones
      if(dut.io.encoutput.valid.toBoolean) {
        print("enc out:")
        for(i <- 0 until 4) {
          print(dut.io.encoutput.payload(i).toBigInt)
          print(",")
        }
        println()
      }
      if(dut.io.output.valid.toBoolean) {
        print("dec out:")
        for(i <- 0 until 4) {
          print(dut.io.output.payload(i).toBigInt)
          print(",")
        }
        println()
      }
    }
  }
  val res = Array(0x681EDF34L,0xD206965EL,0x86B3E94FL,0x536E4246L)
  res.map(x => print(x+","))
  println()
  val res2 = Array(0x305587B3L,0xAE306D09L,0xD2026FDEL,0xE52453A9L)
  res2.map(x => print(x+","))
  println()
}
