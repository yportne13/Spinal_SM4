package SM4

import spinal.core._
import spinal.lib._

case class tau(
  a : Bits
) extends ImplicitArea[Bits] {

  val SboxTable = List(
    0xD6, 0x90, 0xE9, 0xFE,
    0xCC, 0xE1, 0x3D, 0xB7,
    0x16, 0xB6, 0x14, 0xC2,
    0x28, 0xFB, 0x2C, 0x05,

    0x2B, 0x67, 0x9A, 0x76,
    0x2A, 0xBE, 0x04, 0xC3,
    0xAA, 0x44, 0x13, 0x26,
    0x49, 0x86, 0x06, 0x99,

    0x9C, 0x42, 0x50, 0xF4,
    0x91, 0xEF, 0x98, 0x7A,
    0x33, 0x54, 0x0B, 0x43,
    0xED, 0xCF, 0xAC, 0x62,

    0xE4, 0xB3, 0x1C, 0xA9,
    0xC9, 0x08, 0xE8, 0x95,
    0x80, 0xDF, 0x94, 0xFA,
    0x75, 0x8F, 0x3F, 0xA6,

    0x47, 0x07, 0xA7, 0xFC,
    0xF3, 0x73, 0x17, 0xBA,
    0x83, 0x59, 0x3C, 0x19,
    0xE6, 0x85, 0x4F, 0xA8,

    0x68, 0x6B, 0x81, 0xB2,
    0x71, 0x64, 0xDA, 0x8B,
    0xF8, 0xEB, 0x0F, 0x4B,
    0x70, 0x56, 0x9D, 0x35,

    0x1E, 0x24, 0x0E, 0x5E,
    0x63, 0x58, 0xD1, 0xA2,
    0x25, 0x22, 0x7C, 0x3B,
    0x01, 0x21, 0x78, 0x87,

    0xD4, 0x00, 0x46, 0x57,
    0x9F, 0xD3, 0x27, 0x52,
    0x4C, 0x36, 0x02, 0xE7,
    0xA0, 0xC4, 0xC8, 0x9E,

    0xEA, 0xBF, 0x8A, 0xD2,
    0x40, 0xC7, 0x38, 0xB5,
    0xA3, 0xF7, 0xF2, 0xCE,
    0xF9, 0x61, 0x15, 0xA1,

    0xE0, 0xAE, 0x5D, 0xA4,
    0x9B, 0x34, 0x1A, 0x55,
    0xAD, 0x93, 0x32, 0x30,
    0xF5, 0x8C, 0xB1, 0xE3,

    0x1D, 0xF6, 0xE2, 0x2E,
    0x82, 0x66, 0xCA, 0x60,
    0xC0, 0x29, 0x23, 0xAB,
    0x0D, 0x53, 0x4E, 0x6F,

    0xD5, 0xDB, 0x37, 0x45,
    0xDE, 0xFD, 0x8E, 0x2F,
    0x03, 0xFF, 0x6A, 0x72,
    0x6D, 0x6C, 0x5B, 0x51,

    0x8D, 0x1B, 0xAF, 0x92,
    0xBB, 0xDD, 0xBC, 0x7F,
    0x11, 0xD9, 0x5C, 0x41,
    0x1F, 0x10, 0x5A, 0xD8,

    0x0A, 0xC1, 0x31, 0x88,
    0xA5, 0xCD, 0x7B, 0xBD,
    0x2D, 0x74, 0xD0, 0x12,
    0xB8, 0xE5, 0xB4, 0xB0,

    0x89, 0x69, 0x97, 0x4A,
    0x0C, 0x96, 0x77, 0x7E,
    0x65, 0xB9, 0xF1, 0x09,
    0xC5, 0x6E, 0xC6, 0x84,

    0x18, 0xF0, 0x7D, 0xEC,
    0x3A, 0xDC, 0x4D, 0x20,
    0x79, 0xEE, 0x5F, 0x3E,
    0xD7, 0xCB, 0x39, 0x48)

  val Sbox = Mem(Bits(8 bits),initialContent = SboxTable.map(B(_,8 bits)))
  val SboxOut = Bits(32 bits)
  for(i <- 0 until 4) {
    SboxOut(i*8+7 downto i*8) := Sbox.readSync(U(a(i*8+7 downto i*8)))
  }
  
  override def implicitValue: Bits = SboxOut
}

case class Tfunct(
  a : Bits
) extends ImplicitArea[Bits] {
  val getTAU = tau(a)
  val ret = RegNext(getTAU ^ (getTAU rotateLeft 13) ^ (getTAU rotateLeft 23))
  override def implicitValue: Bits = ret
}

class getK(size : Int) extends Component {
  val io = new Bundle {
    val K = in Vec(Bits(32 bits),size)
    val Klist = out Vec(Bits(32 bits),size+1)
  }

  val Klist = Vec(Reg(Bits(32 bits)),size+1)
  for(i <- 0 until size) {
    Klist(i) := Delay(io.K(i),3)
  }

  val CK = Array(
    0x00070e15L, 0x1C232A31L, 0x383F464DL, 0x545B6269L,
    0x70777e85L, 0x8C939AA1L, 0xA8AFB6BDL, 0xC4CBD2D9L,
    0xe0e7eef5L, 0xFC030A11L, 0x181F262DL, 0x343B4249L,
    0x50575e65L, 0x6C737A81L, 0x888F969DL, 0xA4ABB2B9L,

    0xc0c7ced5L, 0xDCE3EAF1L, 0xF8FF060DL, 0x141B2229L,
    0x30373e45L, 0x4C535A61L, 0x686F767DL, 0x848B9299L,
    0xa0a7aeb5L, 0xBCC3CAD1L, 0xD8DFE6EDL, 0xF4FB0209L,
    0x10171e25L, 0x2C333A41L, 0x484F565DL, 0x646B7279L
  )
  val Tout = Tfunct(RegNext(io.K(size-3)^io.K(size-2)^io.K(size-1)^CK(size-4)))
  Klist(size) := Delay(io.K(size-4),3) ^ Tout
  io.Klist := Klist

} 

case class key_expand(
  MK : Vec[Bits]
) extends ImplicitArea[Vec[Bits]] {

  val FK: Array[BigInt] = Array(0xA3B1BAC6L, 0x56AA3350L, 0x677D9197L, 0xB27022DCL)
  val K = MK.zip(FK).map{case (mk,fk) =>
    mk ^ B(fk,32 bits)
  }

  def getK(K : Vec[Bits]): Vec[Bits] = {
    val size = K.size
    val Kmodule = new getK(size)
    Kmodule.io.K := K
    val ret = Kmodule.io.Klist
    if(size == 35) {
      ret
    }else {
      getK(ret)
    }
  }

  val ret = getK(Vec(K))

  override def implicitValue: Vec[Bits] = Vec((0 until 32).map(idx => ret(idx+4)))

}

case class Tfunct2(
  a : Bits
) extends ImplicitArea[Bits] {
  val getTAU = tau(a)
  val ret = RegNext(getTAU ^ (getTAU rotateLeft 2) ^ (getTAU rotateLeft 10) ^ (getTAU rotateLeft 18) ^ (getTAU rotateLeft 24))
  override def implicitValue: Bits = ret
}

case class Ffunct(
  X : Vec[Bits],
  rk : Bits
) extends ImplicitArea[Bits] {
  val ret = Reg(Bits(32 bits))
  ret := Delay(X(0),3) ^ Tfunct2(RegNext(X(1) ^ X(2) ^ X(3) ^ rk))
  override def implicitValue: Bits = ret
}

object EncInput {
  class EncInput extends Bundle {
    val X = Vec(Bits(32 bits),4)
    val MK = Vec(Bits(32 bits),4)
  }
  def apply() = {
    val ret = new EncInput
    ret
  }
}

class encrypt extends Component {
  val io = new Bundle {
    val input = slave Flow(EncInput())
    val output = master Flow(Vec(Bits(32 bits),4))
  }

  val key = key_expand(io.input.payload.MK)
  val X = Delay(io.input.payload.X,32*4)
  def getX(X : Vec[Bits], key : Vec[Bits], idx : Int): Vec[Bits] = {
    val ret = Vec(Bits(32 bits),4)
    val newX = Ffunct(X,key(idx))
    newX.setName("newX_"+idx)
    ret(0) := Delay(X(1),4)
    ret(1) := Delay(X(2),4)
    ret(2) := Delay(X(3),4)
    ret(3) := Ffunct(X, key(idx))
    if(idx == 31) {
      ret
    }else {
      getX(ret,Delay(key,4),idx+1)
    }
  }
  val output = getX(X,key,0)
  io.output.payload := Vec(output(3),output(2),output(1),output(0))
  io.output.valid := Delay(io.input.valid,32*4*2,init = False)

}

class decrypt extends Component {
  val io = new Bundle {
    val input = slave Flow(EncInput())
    val output = master Flow(Vec(Bits(32 bits),4))
  }

  val key = key_expand(io.input.payload.MK)
  val X = Delay(io.input.payload.X,32*4)
  def getX(X : Vec[Bits], key : Vec[Bits], idx : Int): Vec[Bits] = {
    val ret = Vec(Bits(32 bits),4)
    val newX = Ffunct(X,key(idx))
    newX.setName("newX_"+idx)
    ret(0) := Delay(X(1),4)
    ret(1) := Delay(X(2),4)
    ret(2) := Delay(X(3),4)
    ret(3) := Ffunct(X, key(31-idx))
    if(idx == 31) {
      ret
    }else {
      getX(ret,Delay(key,4),idx+1)
    }
  }
  val output = getX(X,key,0)
  io.output.payload := Vec(output(3),output(2),output(1),output(0))
  io.output.valid := Delay(io.input.valid,32*4*2,init = False)

}

class toptest extends Component {
  val io = new Bundle {
    val input = slave Flow(EncInput())
    val encoutput = master Flow(Vec(Bits(32 bits),4))
    val output = master Flow(Vec(Bits(32 bits),4))
  }

  val enc = new encrypt
  val dec = new decrypt
  enc.io.input := io.input
  io.encoutput := enc.io.output
  dec.io.input.valid := enc.io.output.valid
  dec.io.input.payload.MK := Delay(io.input.payload.MK,32*8)
  dec.io.input.payload.X := enc.io.output.payload
  io.output := dec.io.output

}

object gen extends App {
  SpinalVerilog(new toptest)
}
