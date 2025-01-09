package vaquita
import chisel3._
import chisel3.util._
import alu_obj._

class vec_alu(implicit val config: Vaquita_Config) extends Module{
  val io = IO(new Bundle{
  val vs1_in       = Input(Vec(8, Vec(config.count_lanes, SInt(config.XLEN.W))))
  val vs2_in       = Input(Vec(8, Vec(config.count_lanes, SInt(config.XLEN.W))))
  val vs3_in       = Input(Vec(8, Vec(config.count_lanes, SInt(config.XLEN.W))))
  val vs0_in       = Input(Vec(8, Vec(config.count_lanes, SInt(config.XLEN.W))))
  val sew          = Input(UInt(3.W))
  val vl           = Input(UInt(32.W)) //remove this vl
  val vl_in        = Input(UInt(32.W)) 
  val alu_opcode   = Input(UInt(6.W))
  val mask_arith   = Input(Bool())
  val vsd_out      = Output(Vec(8, Vec(config.count_lanes, SInt(config.XLEN.W))))

  })

  def slide_sew_selector(vstart:Int,maxStartOffset:SInt ,vs2_in:SInt,vs3_in:SInt,slide_vec_wire:UInt,vs1_value:UInt,slide_i_value:UInt,sew:Int,count_sew:Int):SInt={
    Mux(io.vl_in > vstart.U,
      Mux(
        vstart.S < maxStartOffset && vstart.S >= 0.S,
        vs3_in,
        Mux(
          vstart.S >= maxStartOffset && vstart.S < io.vl_in.asSInt,
          vslideup(
            vs2_in,
            (slide_vec_wire)%8.U,
            (vs0_mask(vstart)),
            vs3_in,
            Mux(
              vs1_value===0.U,
              slide_i_value,
              (slide_vec_wire)/8.U
            ),
            sew,
            count_sew
          ),
          0.S
        )
      ),
      Mux(
        tail === 0.B,
        vs3_in,
        Fill(32, 1.U).asSInt
      )
    )
  }
    //convert into one array
  val vs0_mask = io.vs0_in.asUInt()(config.vlen,0)
 dontTouch(vs0_mask)

def comp_element_fn(sew:Int,counter:UInt):SInt={
  val cat_element = WireInit(0.S(32.W))
  val comp_fn_value = comparison_func(sew).asSInt
  val comp_shift     = 0
  val output_comp_Data = VecInit(Seq.tabulate(config.count_lanes)(i => comp_fn_value(32 * (i + 1) - 1, 32 * i)))
  dontTouch(cat_element)
  val comp_1bt_cn       = WireInit(VecInit(Seq.fill(32)(0.U(32.W))))
  dontTouch(comp_1bt_cn)
          for (i <- 1 to 31) {
            comp_1bt_cn(i) := ((io.vl_in) - (32.U * counter))
            when(comp_1bt_cn(i) === i.U) {
              cat_element  := Cat(io.vs3_in(0)(counter)(31,i), output_comp_Data(counter)(i-1, 0)).asSInt
            }.elsewhen(comp_1bt_cn(i)===32.U || (comp_1bt_cn(i)/32.U)>0.U){
              cat_element := output_comp_Data(counter).asSInt 
            }
          }
        cat_element
}

def comparison_operators(vs1_in:SInt,vs2_in:SInt):Bool={
    val comparison_table = Seq(
      "b011000".U -> (vs1_in===vs2_in),//vmseq
      "b011001".U -> (vs1_in.asUInt =/= vs2_in.asUInt),//vmsne
      "b011010".U -> (vs1_in.asUInt > vs2_in.asUInt),//vmsltu
      "b011011".U -> (vs1_in > vs2_in),//vmslt
      "b011100".U -> (vs1_in.asUInt >= vs2_in.asUInt),//vmsleu
      "b011101".U -> (vs1_in >= vs2_in),//vmsle
      "b011110".U -> (vs1_in.asUInt < vs2_in.asUInt),//vmsgtu
      "b011111".U -> (vs1_in < vs2_in),//vmsgt
      "b010001".U -> Mux((vs1_in +& vs2_in).asUInt < "hffffffff".U,1.B,0.B)//vmadc
      // "b011111".U -> (vs1_in < vs2_in)//vmsbc
    )
      MuxLookup(io.alu_opcode, 0.B, comparison_table)
  }

def comparison_func(sew: Int): UInt = {
  val elementsPerLane = config.vlen / sew

  val comparison_vec_bit_wires = WireInit(VecInit(Seq.fill(config.vlen)(0.B)))
  val comp_1b = WireInit(VecInit(Seq.fill(config.vlen)(0.B)))
  val comp_0b = WireInit(VecInit(Seq.fill(config.vlen)(0.B)))
  val comp_vs3 = WireInit(VecInit(Seq.fill(config.vlen)(0.B)))

  dontTouch(comparison_vec_bit_wires)
  dontTouch(comp_0b)
  dontTouch(comp_1b)
  dontTouch(comp_vs3)

  val vs3_bit = io.vs3_in.asUInt
  dontTouch(vs3_bit)

  var counter = 0

  for (i <- 0 until config.count_lanes) {
    for (elem_idx <- 0 until elementsPerLane) {

      val startBit = elem_idx * sew
      val endBit = (elem_idx + 1) * sew - 1

      if (endBit < io.vs1_in(i).getWidth && endBit < io.vs2_in(i).getWidth) {
        val vs1_elem = io.vs1_in(i).asUInt()(endBit, startBit)
        val vs2_elem = io.vs2_in(i).asUInt()(endBit, startBit)

        val comparison = comparison_operators(vs1_elem.asSInt, vs2_elem.asSInt)

        comp_1b(counter) := (io.mask_arith && comparison) || (!io.mask_arith && comparison && vs0_mask(counter))
        comp_vs3(counter) := (!vs0_mask(counter) && !io.mask_arith)
        comp_0b(counter) := (io.mask_arith && !comparison)

        comparison_vec_bit_wires(counter) := MuxCase(0.B, Array(
          (comp_0b(counter) === 1.B) -> 0.B,
          (comp_vs3(counter) === 1.B) -> vs3_bit(counter),
          (comp_1b(counter) === 1.B) -> 1.B
        ))

        counter += 1
      }
    }
  }

  comparison_vec_bit_wires.asUInt
}








// }
  def Arithmatic(vs1_in: SInt, vs2_in: SInt,vs3:SInt,sew:Int,v0_bit_mask:UInt): SInt = {
    val maxUInt32 = "hFFFFFFFF".U(32.W)
    val sum = WireInit(0.S(33.W))
    val sub = WireInit(0.S(33.W))
    sub := (vs2_in -& vs1_in)
    sum := (vs1_in.asUInt +& vs2_in.asUInt).asSInt

    val maxValue = (1.S << (sew - 1)) - 1.S 
  val minValue = -(1.S << (sew - 1)) 


// Overflow for addition
val positiveOverflowAdd = (vs1_in(sew - 1) === 0.U && vs2_in(sew - 1) === 0.U && sum(sew - 1) === 1.U)
val negativeOverflowAdd = (vs1_in(sew - 1) === 1.U && vs2_in(sew - 1) === 1.U && sum(sew - 1) === 0.U)

// Overflow for subtraction
val positiveOverflowSub = (vs2_in(sew - 1) === 0.U && vs1_in(sew - 1) === 1.U && sub(sew - 1) === 1.U)
val negativeOverflowSub = (vs2_in(sew - 1) === 1.U && vs1_in(sew - 1) === 0.U && sub(sew - 1) === 0.U)

    val lookuptable = Seq(
      0.U -> (vs1_in + vs2_in),//add
      2.U -> (vs2_in - vs1_in),//sub
      3.U -> (vs1_in - vs2_in),//rsub
      9.U -> (vs1_in & vs2_in),// and
      10.U -> (vs1_in | vs2_in),//or
      11.U -> (vs1_in ^ vs2_in),//xor
      37.U -> (vs2_in << (vs1_in.asUInt%sew.U)), //vsll

      "b101000".U -> (vs2_in.asUInt >> (vs1_in.asUInt%sew.U)).asSInt, //vsrl
      "b101001".U -> ((vs2_in >> (vs1_in.asUInt%sew.U)).asSInt), //vsra
      "b010111".U -> (vs1_in), //vmv 
      "b000100".U -> Mux(vs1_in.asUInt < vs2_in.asUInt,vs1_in.asUInt,vs2_in.asUInt).asSInt,//minu
      "b000101".U -> Mux(vs1_in < vs2_in,vs1_in,vs2_in),//min
      "b000110".U -> Mux(vs1_in.asUInt > vs2_in.asUInt,vs1_in.asUInt,vs2_in.asUInt).asSInt,//maxu
      "b000111".U -> Mux(vs1_in > vs2_in,vs1_in,vs2_in),//max
      "b100000".U -> Mux(sum(32), "hFFFFFFFF".U, sum(31,0)).asSInt,//vsaddu
      "b100001".U -> (Mux(positiveOverflowAdd, maxValue, Mux(negativeOverflowAdd, minValue, sum))),//vsadd
      "b100010".U -> Mux(vs2_in.asUInt < vs1_in.asUInt, 0.U,vs2_in.asUInt - vs1_in.asUInt ).asSInt,//vssubu
      "b100011".U -> Mux(positiveOverflowSub, maxValue, Mux(negativeOverflowSub, minValue, sub(31, 0).asSInt)),//vssub
      "b010000".U -> (vs1_in.asUInt + vs2_in.asUInt + v0_bit_mask).asSInt, //vadc
      "b010010".U -> (vs2_in.asUInt - vs1_in.asUInt - v0_bit_mask).asSInt //vsbc
      // "b101010".U ->   vs1_in,//vnsra
      // "b101010".U ->  vs1_in,//vadc
      // "b101010".U ->  vs1_in,//vsbc
      // "b101010".U ->  vs1_in
    )
    MuxLookup(io.alu_opcode, 0.S, lookuptable)
  }
   def arith_32(vs1:SInt , vs2:SInt,vs3:SInt,mask_vs0:Bool):SInt={
    dontTouch(mask_vs0)
    val vsetvli_mask = 0.B
    val mask_bit_active_element = (mask_vs0===1.B && io.mask_arith===0.B) || io.mask_arith===1.B
    val mask_bit_undisturb = mask_vs0===0.B && io.mask_arith===0.B && vsetvli_mask===0.B
    val vec_sew32_b = WireInit(0.S(32.W))
    val vec_sew32_result = WireInit(0.S(config.XLEN.W))
    when(io.alu_opcode==="b010000".U || io.alu_opcode==="b010010".U){
    vec_sew32_b := (Arithmatic(vs1, vs2,vs3,32,mask_vs0.asUInt)).asSInt
    }.otherwise{
    vec_sew32_b := Mux(mask_bit_active_element===1.B,Arithmatic(vs1, vs2,vs3,32,mask_vs0.asUInt),Mux(mask_bit_undisturb===1.B,vs3,Fill(32,1.U).asSInt)).asSInt
    }
    vec_sew32_result := vec_sew32_b
    vec_sew32_result
  }

     def arith_16(vs1:SInt , vs2:SInt,vs3:SInt,mask_vs0:Bool):SInt={
    dontTouch(mask_vs0)
    val vsetvli_mask = 0.B
    val mask_bit_active_element = (mask_vs0===1.B && io.mask_arith===0.B) || io.mask_arith===1.B
    val mask_bit_undisturb = mask_vs0===0.B && io.mask_arith===0.B && vsetvli_mask===0.B
    val vec_sew16_result = WireInit(0.S(16.W))
    dontTouch(vec_sew16_result)
    when(io.alu_opcode==="b010000".U || io.alu_opcode==="b010010".U){
    vec_sew16_result := (Arithmatic(vs1.asSInt, vs2.asSInt,vs3,16,mask_vs0.asUInt)).asSInt
    }.otherwise{
    vec_sew16_result := Mux(mask_bit_active_element===1.B,Arithmatic(vs1.asSInt, vs2.asSInt,vs3,16,mask_vs0.asUInt),Mux(mask_bit_undisturb===1.B,vs3,Fill(16,1.U).asSInt)).asSInt
    }
    vec_sew16_result
  }

  def arith_8(vs1:SInt , vs2:SInt,vs3:SInt,mask_vs0:Bool):SInt={
    dontTouch(mask_vs0)
    val vsetvli_mask = 0.B
    val mask_bit_active_element = (mask_vs0===1.B && io.mask_arith===0.B) || io.mask_arith===1.B
    val mask_bit_undisturb = mask_vs0===0.B && io.mask_arith===0.B && vsetvli_mask===0.B
    val vec_sew8_result = WireInit(0.S(8.W))
    dontTouch(vec_sew8_result)
    when(io.alu_opcode==="b010000".U || io.alu_opcode==="b010010".U){
    vec_sew8_result := (Arithmatic(vs1.asSInt, vs2.asSInt,vs3,8,mask_vs0.asUInt)).asSInt
    }.otherwise{
    vec_sew8_result := Mux(mask_bit_active_element===1.B,Arithmatic(vs1.asSInt, vs2.asSInt,vs3,8,mask_vs0.asUInt),Mux(mask_bit_undisturb===1.B,vs3,Fill(16,1.U).asSInt)).asSInt
    }
    vec_sew8_result 
  }

  // Function for vslideup
def vslideup(input: SInt, slide_amount:UInt,mask_vs0:Bool,vs3:SInt,i_incre:UInt,sew:Int,count_sew:Int): SInt = {
  val shifted = Wire(SInt(32.W))
  val in_i = WireInit(0.U(33.W))
  val in_j = WireInit(0.U(10.W))
  val sew1 = WireInit(0.U(10.W))
  val count_sew1 = WireInit(0.U(10.W))
  val vs2_element = WireInit(0.S(sew.W))
  val sewup = WireInit(0.U(10.W))
  val sewdown = WireInit(0.U(10.W))
  sewup := ((count_sew*sew)-1).U
  sewdown := ((count_sew-1)*sew).U
  val vsetvli_mask = 0.B
    val mask_bit_active_element = (mask_vs0===1.B && io.mask_arith===0.B) || io.mask_arith===1.B
    val mask_bit_undisturb = mask_vs0===0.B && io.mask_arith===0.B && vsetvli_mask===0.B
    dontTouch(in_i)
    dontTouch(in_j)
    dontTouch(slide_amount)
    dontTouch(vs2_element)
    sew1 := sew.U
    count_sew1 := count_sew.U

    dontTouch(count_sew1)
    dontTouch(sew1)
    dontTouch(sewup)
    dontTouch(sewdown)
  // Mux(mask_bit_active_element===1.B,Arithmatic(vs1, vs2,vs3,32,mask_vs0.asUInt),Mux(mask_bit_undisturb===1.B,vs3,Fill(32,1.U).asSInt)).asSInt
  when(count_sew1===1.U && io.sew==="b000".U){
    in_i := ((slide_amount)/(config.vlen.U/32.U)+i_incre)
    in_j := (slide_amount)%(config.vlen.U/32.U)
  }.elsewhen(count_sew1===2.U && io.sew==="b000".U){
    in_i := (slide_amount)/(config.vlen.U/32.U)+i_incre
    in_j := (slide_amount)%(config.vlen.U/32.U)
  }
  .otherwise{
  in_i := (slide_amount)/(config.vlen.U/32.U)+i_incre
  in_j := (slide_amount)%(config.vlen.U/32.U)
  }
  vs2_element := (io.vs2_in(in_i)(in_j)((count_sew*sew)-1,(count_sew-1)*sew).asSInt)
  shifted := Mux(mask_bit_active_element===1.B,vs2_element,Mux(mask_bit_undisturb===1.B,vs3,Fill(sew,1.U).asSInt)).asSInt
  shifted
}



// Function for vslidedown
def vslidedown(input: SInt, slide_amount: UInt): SInt = {
  val shifted = WireInit(0.S(32.W))
  // shifted := Mux(mask===1.B && io.mask_arith===0.B || io.mask_arith===1.B,(io.vs2_in(in_i)(in_j)).asSInt, vs3)
  shifted
}

// // Function for vrgather
// def vrgather(input: Vec[Vec[SInt]], indices: Vec[Vec[UInt]], i: Int, j: Int): SInt = {
//   // Gathers element from input based on indices provided in indices vector
//   // Ensures index is within bounds, else returns zero
//   val index = indices(i)(j)
//   Mux(index < input.length.U, input(i)(index), 0.S)
// }

val slide_instr =  "b001110".U === io.alu_opcode || "b001111".U === io.alu_opcode

val vl= 4
val tail = 0.B
// call main function
var count_mask = 0.U
val comp_bit = "b011000".U === io.alu_opcode || "b011001".U === io.alu_opcode || "b011010".U === io.alu_opcode || "b011011".U === io.alu_opcode || "b011100".U === io.alu_opcode || "b011101".U === io.alu_opcode || "b011110".U === io.alu_opcode || "b011111".U === io.alu_opcode

//code changes
when(io.sew==="b000".U && slide_instr===0.B){
  when(comp_bit===0.B){
var vl_counter = 0
    for (i <- 0 until 8) {
      for (j <- 0 until config.count_lanes) {
        val idx = (i * config.count_lanes) + j
          io.vsd_out(i)(j) := Cat(Mux(io.vl_in > vl_counter.U+3.U,
          arith_8(io.vs1_in(i)(j)(31,24).asSInt, io.vs2_in(i)(j)(31,24).asSInt, io.vs3_in(i)(j)(31,24).asSInt, vs0_mask(vl_counter+3)),
          Mux(tail === 0.B, io.vs3_in(i)(j)(31,24).asSInt, Fill(8, 1.U).asSInt)),

          Mux(io.vl_in > vl_counter.U +2.U,
          arith_8(io.vs1_in(i)(j)(23,16).asSInt, io.vs2_in(i)(j)(23,16).asSInt, io.vs3_in(i)(j)(23,16).asSInt, vs0_mask(vl_counter+2)),
          Mux(tail === 0.B, io.vs3_in(i)(j)(23,16).asSInt, Fill(8, 1.U).asSInt)),
          
          Mux(io.vl_in > vl_counter.U+1.U,
          arith_8(io.vs1_in(i)(j)(15,8).asSInt, io.vs2_in(i)(j)(15,8).asSInt, io.vs3_in(i)(j)(15,8).asSInt, vs0_mask(vl_counter+1)),
          Mux(tail === 0.B, io.vs3_in(i)(j)(15,8).asSInt, Fill(8, 1.U).asSInt)),

          Mux(io.vl_in > vl_counter.U,
          arith_8(io.vs1_in(i)(j)(7,0).asSInt, io.vs2_in(i)(j)(7,0).asSInt, io.vs3_in(i)(j)(7,0).asSInt, vs0_mask(vl_counter)),
          Mux(tail === 0.B, io.vs3_in(i)(j)(7,0).asSInt, Fill(8, 1.U).asSInt))
          ).asSInt
          vl_counter = vl_counter + 4
      }}}
      .otherwise{
      var vl_counter1 = 1
      var counter2 = 0
      for (j <- 0 until config.count_lanes) {
            io.vsd_out(0)(j) := Mux(io.vl_in > vl_counter1.U,comp_element_fn(8,counter2.U), Mux(tail === 0.B, io.vs3_in(0)(j), Fill(32, 1.U).asSInt))
            vl_counter1    = vl_counter1 + 32
            counter2 = counter2 + 1
            }
        for (i <- 1 until 8) {
          for (j <- 0 until config.count_lanes) {
            io.vsd_out(i)(j) := Mux(tail === 0.B, io.vs3_in(i)(j), Fill(32, 1.U).asSInt)
            }
      }}
  }.elsewhen(io.sew==="b001".U && slide_instr===0.B){//sew=16
  when(comp_bit===0.B){
    val vec_sew16_b = WireInit(0.S(16.W))
    dontTouch(vec_sew16_b)
    val sew16_result = WireInit(0.S(config.XLEN.W))
    var vl_counter = 0
    for (i <- 0 until 8) {
      for (j <- 0 until config.count_lanes) {
        val idx = (i * config.count_lanes) + j
          io.vsd_out(i)(j) := Cat(Mux(io.vl_in > vl_counter.U+1.U,
          arith_16(io.vs1_in(i)(j)(31,16).asSInt, io.vs2_in(i)(j)(31,16).asSInt, io.vs3_in(i)(j)(31,16).asSInt, vs0_mask(vl_counter+1)),
          Mux(tail === 0.B, io.vs3_in(i)(j)(31,16).asSInt, Fill(16, 1.U).asSInt)),

          Mux(io.vl_in > vl_counter.U,
          arith_16(io.vs1_in(i)(j)(15,0).asSInt, io.vs2_in(i)(j)(15,0).asSInt, io.vs3_in(i)(j)(15,0).asSInt, vs0_mask(vl_counter)),
          Mux(tail === 0.B, io.vs3_in(i)(j)(15,0).asSInt, Fill(16, 1.U).asSInt))).asSInt
          vl_counter = vl_counter + 2 //counter_of_2 * 2
      }}}
      .otherwise{
      var vl_counter1 = 1
      var counter2 = 0
      for (j <- 0 until config.count_lanes) {
            io.vsd_out(0)(j) := Mux(io.vl_in > vl_counter1.U,comp_element_fn(16,counter2.U), Mux(tail === 0.B, io.vs3_in(0)(j), Fill(32, 1.U).asSInt))
            vl_counter1    = vl_counter1 + 32
            counter2 = counter2 + 1
            }
        for (i <- 1 until 8) {
          for (j <- 0 until config.count_lanes) {
            io.vsd_out(i)(j) := Mux(tail === 0.B, io.vs3_in(i)(j), Fill(32, 1.U).asSInt)
            }
      }}
  }.elsewhen(io.sew==="b010".U  && slide_instr===0.B){//sew = 32    
    when(comp_bit === 0.B) {
  var vl_counter = 1
  for (i <- 0 until 8) {
    for (j <- 0 until config.count_lanes) {
      val idx = (i * config.count_lanes) + j
      val mask = vs0_mask(idx)
      // val slide_value = WireInit(0.U(33.W))
      // val vs1_value = WireInit(0.U(33.W))
      // vs1_value := io.vs1_in(0)(0).asUInt
      // slide_value := (vs1_value+vl_counter.U-1.U)
      io.vsd_out(i)(j) := Mux(io.vl_in >= vl_counter.U,
        MuxLookup(io.alu_opcode, arith_32(io.vs1_in(i)(j),io.vs2_in(i)(j), io.vs3_in(i)(j), mask), Seq(
          "b001110".U ->    0.S//Mux(io.vs1_in(i)(j) < 8.S,vslideup(io.vs2_in(i)(j), slide_value,mask,io.vs3_in(i)(j),"b001110".U),io.vs3_in(i)(j)),
          // "b001111".U -> vslidedown(io.vs1_in(i)(j), vl_counter.U)
          // "b001100".U -> vrgather(io.vs1_in(i)(j), io.vs2_in(i)(j))
        )),//in this place use the comparison logic
        Mux(tail === 0.B, io.vs3_in(i)(j), Fill(32, 1.U).asSInt)
      )     
      vl_counter = vl_counter + 1
    }
  }
}
      .otherwise{
      var vl_counter1 = 1
      var counter2 = 0
      for (j <- 0 until config.count_lanes) {
            io.vsd_out(0)(j) := Mux(io.vl_in > vl_counter1.U,comp_element_fn(32,counter2.U), Mux(tail === 0.B, io.vs3_in(0)(j), Fill(32, 1.U).asSInt))
            vl_counter1    = vl_counter1 + 32
            counter2 = counter2 + 1
            }
        for (i <- 1 until 8) {
          for (j <- 0 until config.count_lanes) {
            io.vsd_out(i)(j) := Mux(tail === 0.B, io.vs3_in(i)(j), Fill(32, 1.U).asSInt)
            }
      }}
        
  }.elsewhen(slide_instr===1.B && io.sew === "b000".U){ //for slide instructions and sew 8
    var vl_counter = 1  
    var vstart = 0  
    val slidedown_value = WireInit(0.U(33.W))
    var j_slide_count = 0
    val vs1_value = WireInit(0.U(33.W))
    vs1_value := io.vs1_in(0)(0).asUInt
    val slide_vec_wire11 = WireInit(VecInit(Seq.fill(8){VecInit(Seq.fill(8) {0.U(32.W)})}))
    // val maxStartOffset = WireInit(VecInit(Seq.fill(8){VecInit(Seq.fill(8) {0.S(32.W)})}))
    for (i <- 0 until 8) {
      for (j <- 0 until config.count_lanes) {
        slidedown_value := (vs1_value+vl_counter.U-1.U)
        // maxStartOffset(i)(j) := Mux((vstart.S) > vs1_value.asSInt, (vstart.S), vs1_value.asSInt)
        slide_vec_wire11(i)(j) := (j_slide_count.U) - vs1_value(7,0)                        // (vstart.U) - vs1_value
        when(io.alu_opcode==="b001110".U){ // for slide up //   Mux(i.U>0.U && j.U===0.U && vs1_value=/=0.U,,(j.U-vs1_value))("b1111".U).asSInt
          io.vsd_out(i)(j) := Cat(
            slide_sew_selector(vstart+3,Mux((vstart.S + 3.S) > vs1_value.asSInt, (vstart.S + 3.S), vs1_value.asSInt) ,io.vs2_in(i)(j)(31,24).asSInt,io.vs3_in(i)(j)(31,24).asSInt,       (vstart.U) - vs1_value      ,vs1_value,i.U,8,4)(7,0).asSInt,
            slide_sew_selector(vstart+2,Mux((vstart.S + 2.S) > vs1_value.asSInt, (vstart.S + 2.S), vs1_value.asSInt) ,io.vs2_in(i)(j)(23,16).asSInt,io.vs3_in(i)(j)(23,16).asSInt,       (vstart.U) - vs1_value      ,vs1_value,i.U,8,3)(7,0).asSInt,
            slide_sew_selector(vstart+1,Mux((vstart.S + 1.S) > vs1_value.asSInt, (vstart.S + 1.S), vs1_value.asSInt) ,io.vs2_in(i)(j)(15,8).asSInt,io.vs3_in(i)(j)(15,8).asSInt,         (vstart.U) - vs1_value      ,vs1_value,i.U,8,2)(7,0).asSInt,
            slide_sew_selector(vstart,  Mux((vstart.S      ) > vs1_value.asSInt, (vstart.S      ), vs1_value.asSInt) ,io.vs2_in(i)(j)(7,0).asSInt ,io.vs3_in(i)(j)(7,0).asSInt,          (vstart.U) - vs1_value      ,vs1_value,i.U,8,1)(7,0).asSInt).asSInt
            

            // Mux(vs1_value =/=0.U && vstart.S + 3.S >= (Mux((vstart.S + 3.S) > vs1_value.asSInt, (vstart.S + 3.S), vs1_value.asSInt)) && vstart.S + 3.S < io.vl_in.asSInt,slide_vec_wire11(i)(j)+1.U,slide_vec_wire11(i)(j))                     // Mux((vstart.U + 3.U) - vs1_value(7,0) %4.U===3.U,(vstart.U + 3.U) - vs1_value(7,0)+1.U,(vstart.U + 3.U) - vs1_value(7,0))
  }.otherwise{ /// for slide down
    io.vsd_out(i)(j) := 0.S//Mux(io.vl_in >= vl_counter.U, vslideup(io.vs2_in(i)(j), slidedown_value,(vs0_mask((i * config.count_lanes) + j)),io.vs3_in(i)(j),i.U),
        // Mux(tail === 0.B, io.vs3_in(i)(j), Fill(32, 1.U).asSInt))
        }
        
      vl_counter = vl_counter + 4
      vstart = vstart + 4
      j_slide_count = j_slide_count + 1
      }
    }

  }.elsewhen(slide_instr===1.B && io.sew === "b01".U){ //for slide instructions and sew 16
    var vl_counter = 1  
    var vstart = 0  
    val slidedown_value = WireInit(0.U(33.W))
    val vs1_value = WireInit(0.U(33.W))
    vs1_value := io.vs1_in(0)(0).asUInt
    val slide_vec_wire = WireInit(VecInit(Seq.fill(8){VecInit(Seq.fill(8) {0.U(32.W)})}))
    val maxStartOffset = WireInit(VecInit(Seq.fill(8){VecInit(Seq.fill(8) {0.S(32.W)})}))
    for (i <- 0 until 8) {
      for (j <- 0 until config.count_lanes) {
        slidedown_value := (vs1_value+vl_counter.U-1.U)
        maxStartOffset(i)(j) := Mux((vstart.S) > vs1_value.asSInt, (vstart.S), vs1_value.asSInt)
        slide_vec_wire(i)(j) := (vstart.U) - vs1_value
        when(io.alu_opcode==="b001110".U){ // for slide up //   Mux(i.U>0.U && j.U===0.U && vs1_value=/=0.U,,(j.U-vs1_value))("b1111".U).asSInt
        
          io.vsd_out(i)(j) := Cat(
            slide_sew_selector(vstart+1,Mux((vstart.S + 1.S) > vs1_value.asSInt, (vstart.S + 1.S), vs1_value.asSInt) ,io.vs2_in(i)(j)(15,8).asSInt,io.vs3_in(i)(j)(31,0).asSInt,  (vstart.U+1.U) - vs1_value,vs1_value,i.U,16,2)(31,16),
            slide_sew_selector(vstart,  Mux((vstart.S      ) > vs1_value.asSInt, (vstart.S      ), vs1_value.asSInt) ,io.vs2_in(i)(j)(7,0).asSInt ,io.vs3_in(i)(j)(15,0).asSInt,   (vstart.U   ) - vs1_value,vs1_value,i.U,16,1)(15,0)).asSInt
                       
  }.otherwise{ /// for slide down
    io.vsd_out(i)(j) := 0.S//Mux(io.vl_in >= vl_counter.U, vslideup(io.vs2_in(i)(j), slidedown_value,(vs0_mask((i * config.count_lanes) + j)),io.vs3_in(i)(j),i.U),
        // Mux(tail === 0.B, io.vs3_in(i)(j), Fill(32, 1.U).asSInt))
        }
        
      vl_counter = vl_counter + 2
      vstart = vstart + 2
      }
    }

  }.elsewhen(slide_instr===1.B && io.sew === "b10".U){ //for slide instructions and sew 32
    var vl_counter = 1  
    var vstart = 0  
    val slidedown_value = WireInit(0.U(33.W))
    val vs1_value = WireInit(0.U(33.W))
    vs1_value := io.vs1_in(0)(0).asUInt
    val slide_vec_wire = WireInit(VecInit(Seq.fill(8){VecInit(Seq.fill(8) {0.U(32.W)})}))
    val maxStartOffset = WireInit(VecInit(Seq.fill(8){VecInit(Seq.fill(8) {0.S(32.W)})}))
    for (i <- 0 until 8) {
      for (j <- 0 until config.count_lanes) {
        slidedown_value := (vs1_value+vl_counter.U-1.U)
        maxStartOffset(i)(j) := Mux((vstart.S) > vs1_value.asSInt, (vstart.S), vs1_value.asSInt)
        slide_vec_wire(i)(j) := (vstart.U) - vs1_value
        when(io.alu_opcode==="b001110".U){ // for slide up //   Mux(i.U>0.U && j.U===0.U && vs1_value=/=0.U,,(j.U-vs1_value))("b1111".U).asSInt
        
          io.vsd_out(i)(j) := slide_sew_selector(vstart,maxStartOffset(i)(j) ,io.vs2_in(i)(j),io.vs3_in(i)(j),slide_vec_wire(i)(j),vs1_value,i.U,32,1)
            
  }.otherwise{ /// for slide down
    io.vsd_out(i)(j) := 0.S//Mux(io.vl_in >= vl_counter.U, vslideup(io.vs2_in(i)(j), slidedown_value,(vs0_mask((i * config.count_lanes) + j)),io.vs3_in(i)(j),i.U),
        // Mux(tail === 0.B, io.vs3_in(i)(j), Fill(32, 1.U).asSInt))
        }
        
      vl_counter = vl_counter + 1
      vstart = vstart + 1
      }
    }

  }
  .otherwise{
    var vl_counter = 1
    for (i <- 0 until 8) {
      for (j <- 0 until config.count_lanes) {
        val idx = (i * config.count_lanes) + j
        val mask = vs0_mask(idx)
            io.vsd_out(i)(j) :=0.S //Mux(io.vl_in >= vl_counter.U,
          // arith_32(io.vs1_in(i)(j), io.vs2_in(i)(j), io.vs3_in(i)(j), mask),
          // Mux(tail === 0.B, io.vs3_in(i)(j), Fill(32, 1.U).asSInt)
        // )
        vl_counter = vl_counter + 1
      }}
}
}