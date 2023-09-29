// package singlecycle
// import chisel3._ 
// import chisel3.util._ 


// object ALUOP1 {
// 	val ALU_ADD = "b000000000".U
//   val LUI = "b000000000".U
//     val ALU_ADDI = "b000000000".U
//     val ALU_SUB = "b000001000".U
//     val ALU_AND = "b000000111".U
//     val ALU_ANDI = "b000000111".U
//     val ALU_OR  = "b000000110".U
//     val ALU_ORI  = "b000000110".U
//     val ALU_XOR = "b000000100".U
//     val ALU_XORI = "b000000100".U
//     val ALU_SLT = "b000000010".U
//     val ALU_SLTI = "b000000010".U
//     val ALU_SLL = "b000000001".U
//     val ALU_SLLI = "b000000001".U
//     val ALU_SLTU= "b000000011".U
//     val ALU_SLTIU = "b000000011".U
//     val ALU_SRL = "b000000101".U
//     val ALU_SRLI = "b000000101".U
//     val ALU_SRA = "b000001101".U
//     val ALU_SRAI = "b000000101".U
//     val ALU_COPY_A = "b000011111".U  //JAL
//     val V_ADDI = "b000000011".U
//     val V_ADD = "b000000000".U
//     val Vaddvx = "b000000100".U
//     val VMVx = "b010111100".U
//     val VMVvv = "b010111100".U
//     val VMVvi = 187.U
//     val Vsubvv  = 184.U 
//     val Vsubvx = "b000010100".U 
//     val Vrsubvx = "b000011100".U
//     val Vrsubvi = "b000011011".U
//     val Vminuvv = "b000100000".U
//     val Vminuvx = "b000100100".U
//     val Vminvv = "b000101000".U
//     val Vminvx =  "b000101100".U
//     val Vmaxuvv = "b000110000".U
//     val Vmaxuvx = "b000110100".U
//     val Vmaxvv  = "b000111000".U
//     val Vmaxvx = "b000111100".U
//     val Vandvv = "b001001000".U
//     val Vandvx = "b001001100".U
//     val Vandvi = "b001001011".U
//     val Vorvv = "b001010000".U
//     val Vorvx = "b001010100".U
//     val Vorvi = "b00101011".U
//     val Vxorvv = "b001011000".U
//     val Vxorvx = "b001011100".U
//     val Vxorvi = "b001011011".U

  
// 	}

// trait Config{
//     val WLEN = 32
//     val ALUOP_SIG_LEN = 9
// }

// import ALUOP1._

// class ALUIO extends Bundle with Config {
//   val in_A = Input(SInt(WLEN.W))
//   val in_B = Input(SInt(WLEN.W))
//   val vs1 = Input(SInt(128.W))
//   val vs2 = Input(SInt(128.W))
  
//   val vl = Input(UInt(32.W))   
//   val vd = Input (SInt(128.W))
//   val vma =Input(UInt(1.W)) //vtype
//   val vta = Input(UInt(128.W))
//   val vm =Input(UInt(1.W)) //umasked=0  , masked = 1
//   val vs0 = Input(SInt(128.W))

//   val vd_addr = Input(UInt(5.W))
//   val aluc = Input(UInt(ALUOP_SIG_LEN.W))
// 	val sew = Input(UInt(3.W))
// 	val v_ins = Input(Bool())
//   val output = Output(SInt(WLEN.W))
// 	val v_output = Output(SInt(128.W))
// }

// class ALU_ extends Module with Config {
//     val io = IO(new ALUIO)

// val sew_8_a = VecInit((0 until 16).map(i => io.vs1(8*i+7, 8*i).asSInt))
// val sew_8_b = VecInit((0 until 16).map(i => io.vs2(8*i+7, 8*i).asSInt))
// val sew_16_a = VecInit((0 until 8).map(i => io.vs1(16*i+15, 16*i).asSInt))
// val sew_16_b = VecInit((0 until 8).map(i => io.vs2(16*i+15, 16*i).asSInt))
// val sew_32_a = VecInit((0 until 4).map(i => io.vs1(32*i+31, 32*i).asSInt))
// val sew_32_b = VecInit((0 until 4).map(i => io.vs2(32*i+31, 32*i).asSInt))
// val sew_64_a = VecInit((0 until 2).map(i => io.vs1(64*i+63, 64*i).asSInt))
// val sew_64_b = VecInit((0 until 2).map(i => io.vs2(64*i+63, 64*i).asSInt))

// val out8 = VecInit(Seq.fill(16)(0.S(8.W)))
// val out16 = VecInit(Seq.fill(8)(0.S(16.W)))
// val out32 = VecInit(Seq.fill(4)(0.S(32.W)))
// val out64 = VecInit(Seq.fill(2)(0.S(64.W)))

// val sew_8_vd = VecInit((0 until 16).map(i => io.vd(8*i+7, 8*i).asSInt))
// val sew_16_vd = VecInit((0 until 8).map(i => io.vd(16*i+15, 16*i).asSInt))
// val sew_32_vd = VecInit((0 until 4).map(i => io.vd(32*i+31, 32*i).asSInt))
// val sew_64_vd = VecInit((0 until 2).map(i => io.vd(64*i+63, 64*i).asSInt))
  

// def VectorOp_vv( in_A: Vec[SInt], in_B: Vec[SInt], vlmax:UInt, vd:Vec[SInt]) :SInt = {
  
//     val out = vd.zipWithIndex.map{ case(elem,i) => 
//        Mux(i.U < vstart,elem,
//        Mux(i.U >= vstart && i.U < io.vl,
//                             Mux(io.vm === 0.U && io.vs0(i) === 0.U && io.vma === 0.U, elem,
//                                 Mux(io.vm === 0.U && io.vs0(i) === 0.U && io.vma === 1.U, (-1).S,MuxLookup(
//             io.aluc,
//             0.S, // Default case (no operation)
//             Seq(
//                 V_ADD -> (in_A(i) + in_B(i)).asSInt,
//                 Vsubvv  -> (in_A(i) - in_B(i)).asSInt,
//                 Vandvv -> (in_A(i) & in_B(i)).asSInt,
//                 Vorvv -> (in_A(i) | in_B(i)).asSInt,
//                 Vxorvv -> (in_A(i) ^ in_B(i)).asSInt
//             )
//         ))
//                             ), 
//         Mux(io.vta === 0.U && i.U > io.vl && i.U < vlmax, elem, (-1).S)
//         ))
//         }
//       Cat(out.reverse).asSInt
// }

// def VectorOp_vi( in_A: Vec[SInt], imm: SInt, vlmax:UInt, vd:Vec[SInt]) :SInt = {
  
//     val out = vd.zipWithIndex.map{ case(elem,i) => 
//        Mux(i.U < vstart,elem,
//        Mux(i.U >= vstart && i.U < io.vl,
//                             Mux(io.vm === 0.U && io.vs0(i) === 0.U && io.vma === 0.U, elem,
//                                 Mux(io.vm === 0.U && io.vs0(i) === 0.U && io.vma === 1.U, (-1).S,MuxLookup(
//             io.aluc,
//             0.S, // Default case (no operation)
//             Seq(
//                 V_ADDI  -> (in_A(i) + imm).asSInt,
//                 Vrsubvi -> ( imm-in_A(i)).asSInt,
//                 Vandvi -> (in_A(i) & imm).asSInt,
//                 Vorvi -> (in_A(i) | imm).asSInt,
//                 Vxorvi -> (in_A(i) ^ imm).asSInt,
//             )
//         ))
//                             ), 
//         Mux(io.vta === 0.U && i.U > io.vl && i.U < vlmax, elem, (-1).S)
//         ))
//         }
//       Cat(out.reverse).asSInt
// }
// def VectorOp_vx( in_A: Vec[SInt], imm: SInt, vlmax:UInt, vd:Vec[SInt]) :SInt = {
  
//     val out = vd.zipWithIndex.map{ case(elem,i) => 
//        Mux(i.U < vstart,elem,
//        Mux(i.U >= vstart && i.U < io.vl,
//                             Mux(io.vm === 0.U && io.vs0(i) === 0.U && io.vma === 0.U, elem,
//                                 Mux(io.vm === 0.U && io.vs0(i) === 0.U && io.vma === 1.U, (-1).S,MuxLookup(
//             io.aluc,
//             0.S, // Default case (no operation)
//             Seq(
//                 Vaddvx  -> (in_A(i) + imm).asSInt,
//                 Vsubvx -> ( in_A(i)-imm).asSInt,
//                 Vrsubvx -> ( imm-in_A(i)).asSInt,
//                 Vandvx -> (in_A(i) & imm).asSInt,
//                 Vorvx -> (in_A(i) | imm).asSInt,
//                 Vxorvx -> (in_A(i) ^ imm).asSInt,
//             )
//         ))
//                             ), 
//         Mux(io.vta === 0.U && i.U > io.vl && i.U < vlmax, elem, (-1).S)
//         ))
//         }
//       Cat(out.reverse).asSInt
// }

// def Vectorminu_vv(in_A: Vec[SInt], in_B: Vec[SInt], vlmax:UInt, vd:Vec[SInt]) :SInt = {
//   val out = vd.zipWithIndex.map{case (elem ,i )=>
//       Mux(i.U < vstart,elem.asUInt,
//       Mux(i.U >= vstart && i.U < io.vl,
//                             Mux(io.vm === 0.U && io.vs0(i) === 0.U && io.vma === 0.U, elem.asUInt,
//                                 Mux(io.vm === 0.U && io.vs0(i) === 0.U && io.vma === 1.U, "b1111111111111111111111111111111111111111111111111111111111111111".U,Mux(in_B(i).asUInt <= in_A(i).asUInt, in_B(i).asUInt, in_A(i).asUInt))
//                             ), 
//         Mux(io.vta === 0.U && i.U > io.vl && i.U < 2.U, elem.asUInt, "b1111111111111111111111111111111111111111111111111111111111111111".U)
//         ))
//         }
//         Cat(out.reverse).asSInt
// }

// def Vectorminu_vxvi(in_A: Vec[SInt], imm: SInt, vlmax:UInt, vd:Vec[SInt]) :SInt = {
//   val out = vd.zipWithIndex.map{case (elem ,i )=>
//       Mux(i.U < vstart,elem.asUInt,
//       Mux(i.U >= vstart && i.U < io.vl,
//                             Mux(io.vm === 0.U && io.vs0(i) === 0.U && io.vma === 0.U, elem.asUInt,
//                                 Mux(io.vm === 0.U && io.vs0(i) === 0.U && io.vma === 1.U, "b1111111111111111111111111111111111111111111111111111111111111111".U,Mux(imm.asUInt <= in_A(i).asUInt, imm.asUInt, in_A(i).asUInt))
//                             ), 
//         Mux(io.vta === 0.U && i.U > io.vl && i.U < 2.U, elem.asUInt, "b1111111111111111111111111111111111111111111111111111111111111111".U)
//         ))
//         }
//         Cat(out.reverse).asSInt
// }

// def Vectormin_vv(in_A: Vec[SInt], in_B: Vec[SInt], vlmax:UInt, vd:Vec[SInt]) :SInt = {
//   val out = vd.zipWithIndex.map{case (elem ,i )=>
//       Mux(i.U < vstart,elem.asUInt,
//       Mux(i.U >= vstart && i.U < io.vl,
//                             Mux(io.vm === 0.U && io.vs0(i) === 0.U && io.vma === 0.U, elem.asUInt,
//                                 Mux(io.vm === 0.U && io.vs0(i) === 0.U && io.vma === 1.U, "b1111111111111111111111111111111111111111111111111111111111111111".U,Mux(in_B(i) <= in_A(i), in_B(i).asUInt, in_A(i).asUInt))
//                             ), 
//         Mux(io.vta === 0.U && i.U > io.vl && i.U < 2.U, elem.asUInt, "b1111111111111111111111111111111111111111111111111111111111111111".U)
//         ))
//         }
//         Cat(out.reverse).asSInt
// }
// def Vectormin_vx(in_A: Vec[SInt], imm: SInt, vlmax:UInt, vd:Vec[SInt]) :SInt = {
//   val out = vd.zipWithIndex.map{case (elem ,i )=>
//       Mux(i.U < vstart,elem.asUInt,
//       Mux(i.U >= vstart && i.U < io.vl,
//                             Mux(io.vm === 0.U && io.vs0(i) === 0.U && io.vma === 0.U, elem.asUInt,
//                                 Mux(io.vm === 0.U && io.vs0(i) === 0.U && io.vma === 1.U, "b1111111111111111111111111111111111111111111111111111111111111111".U,Mux(imm <= in_A(i), imm.asUInt, in_A(i).asUInt))
//                             ), 
//         Mux(io.vta === 0.U && i.U > io.vl && i.U < 2.U, elem.asUInt, "b1111111111111111111111111111111111111111111111111111111111111111".U)
//         ))
//         }
//         Cat(out.reverse).asSInt
// }


// def Vectormaxu_vv(in_A: Vec[SInt], in_B: Vec[SInt], vlmax:UInt, vd:Vec[SInt]) :SInt = {
//   val out = vd.zipWithIndex.map{case (elem ,i )=>
//       Mux(i.U < vstart,elem.asUInt,
//       Mux(i.U >= vstart && i.U < io.vl,
//                             Mux(io.vm === 0.U && io.vs0(i) === 0.U && io.vma === 0.U, elem.asUInt,
//                                 Mux(io.vm === 0.U && io.vs0(i) === 0.U && io.vma === 1.U, "b1111111111111111111111111111111111111111111111111111111111111111".U,Mux(in_B(i).asUInt <= in_A(i).asUInt, in_A(i).asUInt, in_B(i).asUInt))
//                             ), 
//         Mux(io.vta === 0.U && i.U > io.vl && i.U < 2.U, elem.asUInt, "b1111111111111111111111111111111111111111111111111111111111111111".U)
//         ))
//         }
//         Cat(out.reverse).asSInt
// }

// def Vectormaxu_vx(in_A: Vec[SInt], imm: SInt, vlmax:UInt, vd:Vec[SInt]) :SInt = {
//   val out = vd.zipWithIndex.map{case (elem ,i )=>
//       Mux(i.U < vstart,elem.asUInt,
//       Mux(i.U >= vstart && i.U < io.vl,
//                             Mux(io.vm === 0.U && io.vs0(i) === 0.U && io.vma === 0.U, elem.asUInt,
//                                 Mux(io.vm === 0.U && io.vs0(i) === 0.U && io.vma === 1.U, "b1111111111111111111111111111111111111111111111111111111111111111".U,Mux(imm.asUInt <= in_A(i).asUInt, in_A(i).asUInt, imm.asUInt))
//                             ), 
//         Mux(io.vta === 0.U && i.U > io.vl && i.U < 2.U, elem.asUInt, "b1111111111111111111111111111111111111111111111111111111111111111".U)
//         ))
//         }
//         Cat(out.reverse).asSInt
// }

// def Vectormax_vv(in_A: Vec[SInt], in_B: Vec[SInt], vlmax:UInt, vd:Vec[SInt]) :SInt = {
//   val out = vd.zipWithIndex.map{case (elem ,i )=>
//       Mux(i.U < vstart,elem.asUInt,
//       Mux(i.U >= vstart && i.U < io.vl,
//                             Mux(io.vm === 0.U && io.vs0(i) === 0.U && io.vma === 0.U, elem.asUInt,
//                                 Mux(io.vm === 0.U && io.vs0(i) === 0.U && io.vma === 1.U, "b1111111111111111111111111111111111111111111111111111111111111111".U,Mux(in_B(i) <= in_A(i),in_A(i).asUInt, in_B(i).asUInt))
//                             ), 
//         Mux(io.vta === 0.U && i.U > io.vl && i.U < 2.U, elem.asUInt, "b1111111111111111111111111111111111111111111111111111111111111111".U)
//         ))
//         }
//         Cat(out.reverse).asSInt
// }
// def Vectormax_vx(in_A: Vec[SInt], imm: SInt, vlmax:UInt, vd:Vec[SInt]) :SInt = {
//   val out = vd.zipWithIndex.map{case (elem ,i )=>
//       Mux(i.U < vstart,elem.asUInt,
//       Mux(i.U >= vstart && i.U < io.vl,
//                             Mux(io.vm === 0.U && io.vs0(i) === 0.U && io.vma === 0.U, elem.asUInt,
//                                 Mux(io.vm === 0.U && io.vs0(i) === 0.U && io.vma === 1.U, "b1111111111111111111111111111111111111111111111111111111111111111".U,Mux(imm <= in_A(i),in_A(i).asUInt, imm.asUInt))
//                             ), 
//         Mux(io.vta === 0.U && i.U > io.vl && i.U < 2.U, elem.asUInt, "b1111111111111111111111111111111111111111111111111111111111111111".U)
//         ))
//         }
//         Cat(out.reverse).asSInt
// }
// def Vectormove_vxvi( imm: SInt, vlmax:UInt, vd:Vec[SInt]) :SInt = {
//   val out = vd.zipWithIndex.map{ case(elem,i) => 
//        Mux(i.U < vstart,elem.asUInt,
//        Mux(i.U >= vstart && i.U < io.vl,
//                             Mux(io.vm === 0.U && io.vs0(i) === 0.U && io.vma === 0.U, elem.asUInt,
//                                 Mux(io.vm === 0.U && io.vs0(i) === 0.U && io.vma === 1.U, "b1111111111111111111111111111111111111111111111111111111111111111".U,(imm).asUInt)
//                             ), 
//         Mux(io.vta === 0.U && i.U > io.vl && i.U < vlmax, elem.asUInt, "b1111111111111111111111111111111111111111111111111111111111111111".U)
//         ))
//         }
//       Cat(out.reverse).asSInt
// }
// def Vectormove_vv( in_A: Vec[SInt], vlmax:UInt, vd:Vec[SInt]) :SInt = {
//   val out = vd.zipWithIndex.map{ case(elem,i) => 
//        Mux(i.U < vstart,elem.asUInt,
//        Mux(i.U >= vstart && i.U < io.vl,
//                             Mux(io.vm === 0.U && io.vs0(i) === 0.U && io.vma === 0.U, elem.asUInt,
//                                 Mux(io.vm === 0.U && io.vs0(i) === 0.U && io.vma === 1.U, "b1111111111111111111111111111111111111111111111111111111111111111".U,(in_A(i)).asUInt)
//                             ), 
//         Mux(io.vta === 0.U && i.U > io.vl && i.U < vlmax, elem.asUInt, "b1111111111111111111111111111111111111111111111111111111111111111".U)
//         ))
//         }
//       Cat(out.reverse).asSInt
// }



// io.v_output := 0.S

//    io.output := 0.S
//   //  val imm = 0.S
//   val vstart = 0.U
//     when (io.v_ins =/= 1.B) {
//     io.output := MuxLookup(io.aluc, io.in_A, Seq(
//       ALU_ADD -> (io.in_A + io.in_B),
//       ALU_SLL -> (io.in_A << io.in_B(4, 0)).asSInt,
//       ALU_SLT -> Mux(io.in_A < io.in_B, 1.S, 0.S),
//       ALU_SLTU -> Mux(io.in_A.asUInt < io.in_B.asUInt, 1.S, 0.S),
//       ALU_XOR -> (io.in_A ^ io.in_B),
//       ALU_SRL -> (io.in_A.asUInt >> io.in_B(4, 0).asUInt).asSInt,
//       ALU_OR -> (io.in_A | io.in_B),
//       ALU_AND -> (io.in_A & io.in_B),
//       ALU_SUB -> (io.in_A - io.in_B),
//       ALU_SRA -> (io.in_A >> io.in_B(4, 0)).asSInt,
//       ALU_COPY_A -> io.in_A
//     ))

//    }.otherwise{ //VectorAddvv
// 	when (io.sew === "b011".U && io.aluc === V_ADD){  //sew = 64
//      io.v_output := VectorOp_vv(sew_64_a,sew_64_b,2.U,sew_64_vd)
// 	}
// 	 .elsewhen (io.sew === "b010".U && io.aluc === V_ADD){ // sew = 32
//         io.v_output := VectorOp_vv(sew_32_a,sew_32_b,4.U,sew_32_vd)
//       }
// 	  .elsewhen(io.sew === "b001".U && io.aluc === V_ADD){ //sew = 16
//         io.v_output := VectorOp_vv(sew_16_a,sew_16_b,8.U,sew_16_vd)
// 	  }
// 	  .elsewhen(io.sew === "b000".U && io.aluc === V_ADD){ //sew = 8
//         io.v_output := VectorOp_vv(sew_8_a,sew_8_b,16.U,sew_8_vd)
// 	 //vectoraddvv end
//    //vector add vi start
//     }.elsewhen(io.sew === "b011".U && io.aluc(2,0) === "b011".U){
//       val imm = Cat(0.S(32.W), io.in_B).asSInt
//       io.v_output := VectorOp_vi(sew_64_b,imm,2.U,sew_64_vd)
    
//     }.elsewhen(io.sew === "b010".U && io.aluc(2,0) === "b011".U){
//        val imm = io.in_B(31,0).asSInt        
//         io.v_output := VectorOp_vi(sew_32_b,imm,4.U,sew_32_vd)
	  
//     }.elsewhen(io.sew === "b000".U && io.aluc(2,0) === "b011".U){
//       val imm = io.in_B(7,0).asSInt
//       io.v_output := VectorOp_vi(sew_8_b,imm,16.U,sew_8_vd)
//     }
// 	  .elsewhen (io.sew === "b001".U && io.aluc(2,0) === "b011".U){
//       val imm = io.in_B(15,0).asSInt
//       io.v_output := VectorOp_vi(sew_16_b,imm,8.U,sew_16_vd)

// 	  }  //vector vi end
//         //vector  vx
//     .elsewhen(io.sew === "b011".U && io.aluc === Vaddvx){
// 		  val imm = Cat(0.S(32.W), io.in_A).asSInt
//       io.v_output := VectorOp_vx(sew_64_b,imm,2.U,sew_64_vd)
    
//     }.elsewhen(io.sew === "b010".U && io.aluc === Vaddvx){
//        val imm = io.in_A(31,0).asSInt
//         io.v_output := VectorOp_vx(sew_32_b,imm,4.U,sew_32_vd)
	  
//     }.elsewhen(io.sew === "b000".U && io.aluc === Vaddvx){
// 		val imm = io.in_A(7,0).asSInt
//       io.v_output := VectorOp_vx(sew_8_b,imm,16.U,sew_8_vd)
//     }.elsewhen (io.sew === "b001".U && io.aluc === Vaddvx){
// 		val imm = io.in_A(15,0).asSInt
//        io.v_output := VectorOp_vx(sew_16_b,imm,8.U,sew_16_vd)
// 	  }  //vector  vx end




//     //vector move vx instruction
//     .elsewhen (io.aluc === VMVx){
//       when(io.vd_addr === 0.U){
//         io.v_output := Cat((0.S(96.W)),io.in_A).asSInt
//   	  }.otherwise{
//       when (io.sew === "b011".U){
//         val imm = Cat(0.S(32.W), io.in_A).asSInt
//           io.v_output := Vectormove_vxvi(imm,2.U,sew_64_vd)
      
//       }.elsewhen(io.sew === "b010".U ){
//        val imm = io.in_A(31,0).asSInt
//           io.v_output := Vectormove_vxvi(imm,4.U,sew_32_vd)
	  
//       }.elsewhen (io.sew === "b001".U){
// 		   val imm = io.in_A(15,0).asSInt
//     	    io.v_output := Vectormove_vxvi(imm,8.U,sew_16_vd)

//       }.elsewhen(io.sew === "b000".U ){
//         val imm = io.in_A(7,0).asSInt
//           io.v_output := Vectormove_vxvi(imm,16.U,sew_8_vd)


//      } }}//vmv vx end
    
//     //vector move vi instruction
//     .elsewhen (io.aluc === VMVvi){
//       when(io.vd_addr === 0.U){
//         io.v_output := Cat((0.S(96.W)),io.in_B).asSInt
//   	  }.otherwise{
//         when (io.sew === "b011".U){
//         val imm = Cat(0.S(32.W), io.in_B).asSInt
//         io.v_output := Vectormove_vxvi(imm,2.U,sew_64_vd)
    
//         }.elsewhen(io.sew === "b010".U ){
//           val imm = io.in_B(31,0).asSInt
//           io.v_output := Vectormove_vxvi(imm,4.U,sew_32_vd)
//         }.elsewhen(io.sew === "b000".U ){
// 		      val imm = io.in_B(7,0).asSInt
//     	    io.v_output := Vectormove_vxvi(imm,16.U,sew_8_vd)
// 	      }.elsewhen (io.sew === "b001".U){
// 		      val imm = io.in_B(15,0).asSInt
//           io.v_output := Vectormove_vxvi(imm,8.U,sew_16_vd)      
//         }}
//     }//vmv vi end
    
//     //vector move vv instruction
//     .elsewhen (io.aluc === VMVvv){
//       when(io.vd_addr === 0.U){
//     io.v_output := io.vs1
//   	  }.otherwise{
//         when (io.sew === "b011".U){
//         io.v_output := Vectormove_vv(sew_64_a,2.U,sew_64_vd)
    
//         }.elsewhen(io.sew === "b010".U ){
//          io.v_output := Vectormove_vv(sew_32_a,4.U,sew_32_vd)
	  
//         }.elsewhen(io.sew === "b000".U ){
// 			    io.v_output := Vectormove_vv(sew_8_a,16.U,sew_8_vd)
// 	      }.elsewhen (io.sew === "b001".U){
//           io.v_output := Vectormove_vv(sew_16_a,8.U,sew_16_vd)
//         }
//         }
//     }//vmv vv end
//         //vector sub vv
//     .elsewhen (io.sew === "b011".U && io.aluc === Vsubvv){
//       io.v_output := VectorOp_vv(sew_64_a,sew_64_b,2.U,sew_64_vd)
// 	}
// 	 .elsewhen (io.sew === "b010".U && io.aluc === Vsubvv){ // sew = 32
//         io.v_output := VectorOp_vv(sew_32_a,sew_32_b,4.U,sew_32_vd)
//       }
// 	  .elsewhen(io.sew === "b001".U && io.aluc === Vsubvv){ //sew = 16
//         io.v_output := VectorOp_vv(sew_16_a,sew_16_b,8.U,sew_16_vd)
// 	  }
// 	  .elsewhen(io.sew === "b000".U && io.aluc === Vsubvv){ //sew = 8
//         io.v_output := VectorOp_vv(sew_8_a,sew_8_b,16.U,sew_8_vd)
//     }
//     //vector sub vv end
//     //vector sub vx
//     .elsewhen(io.sew === "b011".U && io.aluc === Vsubvx){
// 		val imm = Cat(0.S(32.W), io.in_A).asSInt
//       io.v_output := VectorSub_vxvi(sew_64_b,imm,2.U,sew_64_vd)
    
//     }.elsewhen(io.sew === "b010".U && io.aluc === Vsubvx){
//        val imm = io.in_A(31,0).asSInt
//         io.v_output := VectorSub_vxvi(sew_32_b,imm,4.U,sew_32_vd)
	  
//     }.elsewhen(io.sew === "b000".U && io.aluc === Vsubvx){
// 		val imm = io.in_A(7,0).asSInt
//       io.v_output := VectorSub_vxvi(sew_8_b,imm,16.U,sew_8_vd)
// 	  }
// 	  .elsewhen (io.sew === "b001".U && io.aluc === Vsubvx){
// 		val imm = io.in_A(15,0).asSInt
//       io.v_output := VectorSub_vxvi(sew_16_b,imm,16.U,sew_16_vd)

// 	  }  
//     //vector sub vx end
//     //vector rsub vx
//     .elsewhen(io.sew === "b011".U && io.aluc === Vrsubvx){
// 		  val imm = Cat(0.S(32.W), io.in_A).asSInt
//       io.v_output := VectorRSub_vxvi(sew_64_b,imm,2.U,sew_64_vd)

    
//     }.elsewhen(io.sew === "b010".U && io.aluc === Vrsubvx){
//        val imm = io.in_A(31,0).asSInt
//       io.v_output := VectorRSub_vxvi(sew_32_b,imm,4.U,sew_32_vd)
	  
//     }.elsewhen(io.sew === "b000".U && io.aluc === Vrsubvx){
// 		val imm = io.in_A(7,0).asSInt
//       io.v_output := VectorRSub_vxvi(sew_8_b,imm,16.U,sew_8_vd)
// 	  }
// 	  .elsewhen (io.sew === "b001".U && io.aluc === Vrsubvx){
// 		val imm = io.in_A(15,0).asSInt
//       io.v_output := VectorRSub_vxvi(sew_16_b,imm,8.U,sew_16_vd)
// 	  }
//     //vector rsub vx end
//     //vector rsub vi
//     .elsewhen(io.sew === "b011".U && io.aluc === Vrsubvi){
// 		val imm = Cat(0.S(32.W), io.in_B).asSInt
//       io.v_output := VectorRSub_vxvi(sew_64_b,imm,2.U,sew_64_vd)
    
//     }.elsewhen(io.sew === "b010".U && io.aluc === Vrsubvi){
//       val imm = io.in_B(31,0).asSInt
//         io.v_output := VectorRSub_vxvi(sew_32_b,imm,4.U,sew_32_vd)  
//     }.elsewhen(io.sew === "b000".U && io.aluc === Vrsubvi){
// 		val imm = io.in_B(7,0).asSInt
//       io.v_output := VectorRSub_vxvi(sew_8_b,imm,16.U,sew_8_vd)
// 	  }
// 	  .elsewhen (io.sew === "b001".U && io.aluc === Vrsubvi){
// 		val imm = io.in_B(15,0).asSInt
//       io.v_output := VectorRSub_vxvi(sew_16_b,imm,8.U,sew_16_vd)
// 	  }//vrsub vi end
//     // vandvv start
//     .elsewhen (io.sew === "b011".U && io.aluc === Vandvv){  //sew = 64
//      io.v_output := VectorAnd_vv(sew_64_a,sew_64_b,2.U,sew_64_vd)
// 	}
// 	 .elsewhen (io.sew === "b010".U && io.aluc === Vandvv){ // sew = 32
//         io.v_output := VectorAnd_vv(sew_32_a,sew_32_b,4.U,sew_32_vd)
//   }
// 	  .elsewhen(io.sew === "b001".U && io.aluc === Vandvv){ //sew = 16
//         io.v_output := VectorAnd_vv(sew_16_a,sew_16_b,8.U,sew_16_vd)
//   }
// 	  .elsewhen(io.sew === "b000".U && io.aluc === Vandvv){ //sew = 8
//         io.v_output := VectorAnd_vv(sew_8_a,sew_8_b,16.U,sew_8_vd)
//   }
//     //vand vv end
//     //vand vx start
//     .elsewhen(io.sew === "b011".U && io.aluc === Vandvx){
// 		  val imm = Cat(0.S(32.W), io.in_A).asSInt
//       io.v_output := VectorAnd_vxvi(sew_64_b,imm,2.U,sew_64_vd)
//     }.elsewhen(io.sew === "b010".U && io.aluc === Vandvx){
//        val imm = io.in_A(31,0).asSInt
//       io.v_output := VectorAnd_vxvi(sew_32_b,imm,4.U,sew_32_vd)
	  
//     }.elsewhen(io.sew === "b000".U && io.aluc === Vandvx){
// 		val imm = io.in_A(7,0).asSInt
//       io.v_output := VectorAnd_vxvi(sew_8_b,imm,16.U,sew_8_vd)
// 	  }
// 	  .elsewhen (io.sew === "b001".U && io.aluc === Vandvx){
// 		val imm = io.in_A(15,0).asSInt
//       io.v_output := VectorAnd_vxvi(sew_16_b,imm,8.U,sew_16_vd)
// 	  }
//     //vand vx end
//     //vand vi start
//     .elsewhen(io.sew === "b011".U && io.aluc === Vandvi){
// 		val imm = Cat(0.S(32.W), io.in_B).asSInt
//       io.v_output := VectorAnd_vxvi(sew_64_b,imm,2.U,sew_64_vd)
    
//     }.elsewhen(io.sew === "b010".U && io.aluc === Vandvi){
//        val imm = io.in_B(31,0).asSInt
//          io.v_output := VectorAnd_vxvi(sew_32_b,imm,4.U,sew_32_vd)
	  
//     }.elsewhen(io.sew === "b000".U && io.aluc === Vandvi){
// 		val imm = io.in_B(7,0).asSInt
//       io.v_output := VectorAnd_vxvi(sew_8_b,imm,16.U,sew_8_vd)
// 	  }
// 	  .elsewhen (io.sew === "b001".U && io.aluc === Vandvi){
// 		val imm = io.in_B(15,0).asSInt
// 		io.v_output := VectorAnd_vxvi(sew_16_b,imm,8.U,sew_16_vd)
// 	  } 
//     //vand vi end
//     //vor vv start
//         .elsewhen (io.sew === "b011".U && io.aluc === Vorvv){  //sew = 64
//      io.v_output := VectorOr_vv(sew_64_a,sew_64_b,2.U,sew_64_vd)
// 	}
// 	 .elsewhen (io.sew === "b010".U && io.aluc === Vorvv){ // sew = 32
//         io.v_output := VectorOr_vv(sew_32_a,sew_32_b,4.U,sew_32_vd)
//   }
// 	  .elsewhen(io.sew === "b001".U && io.aluc === Vorvv){ //sew = 16
//         io.v_output := VectorOr_vv(sew_16_a,sew_16_b,8.U,sew_16_vd)
//   }
// 	  .elsewhen(io.sew === "b000".U && io.aluc === Vorvv){ //sew = 8
//         io.v_output := VectorOr_vv(sew_8_a,sew_8_b,16.U,sew_8_vd)
//   }
//       //vor vv end
//     //vor vx start
//     .elsewhen(io.sew === "b011".U && io.aluc === Vorvx){
// 		  val imm = Cat(0.S(32.W), io.in_A).asSInt
//       io.v_output := VectorOr_vxvi(sew_64_b,imm,2.U,sew_64_vd)
//     }.elsewhen(io.sew === "b010".U && io.aluc === Vorvx){
//        val imm = io.in_A(31,0).asSInt
//       io.v_output := VectorOr_vxvi(sew_32_b,imm,4.U,sew_32_vd)
	  
//     }.elsewhen(io.sew === "b000".U && io.aluc === Vorvx){
// 		val imm = io.in_A(7,0).asSInt
//       io.v_output := VectorOr_vxvi(sew_8_b,imm,16.U,sew_8_vd)
// 	  }
// 	  .elsewhen (io.sew === "b001".U && io.aluc === Vorvx){
// 		val imm = io.in_A(15,0).asSInt
//       io.v_output := VectorOr_vxvi(sew_16_b,imm,8.U,sew_16_vd)
// 	  }
//     //vor vx end
//     //vor vi start
//       .elsewhen(io.sew === "b011".U && io.aluc === Vorvi){
// 		val imm = Cat(0.S(32.W), io.in_B).asSInt
//       io.v_output := VectorOr_vxvi(sew_64_b,imm,2.U,sew_64_vd)
    
//     }.elsewhen(io.sew === "b010".U && io.aluc === Vorvi){
//        val imm = io.in_B(31,0).asSInt
//          io.v_output := VectorOr_vxvi(sew_32_b,imm,4.U,sew_32_vd)
	  
//     }.elsewhen(io.sew === "b000".U && io.aluc === Vorvi){
// 		val imm = io.in_B(7,0).asSInt
//       io.v_output := VectorOr_vxvi(sew_8_b,imm,16.U,sew_8_vd)
// 	  }
// 	  .elsewhen (io.sew === "b001".U && io.aluc === Vorvi){
// 		val imm = io.in_B(15,0).asSInt
// 		io.v_output := VectorOr_vxvi(sew_16_b,imm,8.U,sew_16_vd)
// 	  }  //vor vi end

//     //vxor vv start
//   .elsewhen (io.sew === "b011".U && io.aluc === Vxorvv){  //sew = 64
//      io.v_output := VectorXor_vv(sew_64_a,sew_64_b,2.U,sew_64_vd)
// 	}
// 	 .elsewhen (io.sew === "b010".U && io.aluc === Vxorvv){ // sew = 32
//         io.v_output := VectorXor_vv(sew_32_a,sew_32_b,4.U,sew_32_vd)
//   }
// 	  .elsewhen(io.sew === "b001".U && io.aluc === Vxorvv){ //sew = 16
//         io.v_output := VectorXor_vv(sew_16_a,sew_16_b,8.U,sew_16_vd)
//   }
// 	  .elsewhen(io.sew === "b000".U && io.aluc === Vxorvv){ //sew = 8
//         io.v_output := VectorXor_vv(sew_8_a,sew_8_b,16.U,sew_8_vd)
//   }
//     //vxor vv end
//     //vxor vx start
//   .elsewhen(io.sew === "b011".U && io.aluc === Vxorvx){
// 		  val imm = Cat(0.S(32.W), io.in_A).asSInt
//       io.v_output := VectorXor_vxvi(sew_64_b,imm,2.U,sew_64_vd)
//   }.elsewhen(io.sew === "b010".U && io.aluc === Vxorvx){
//        val imm = io.in_A(31,0).asSInt
//       io.v_output := VectorXor_vxvi(sew_32_b,imm,4.U,sew_32_vd)
	  
//   }.elsewhen(io.sew === "b000".U && io.aluc === Vxorvx){
// 		val imm = io.in_A(7,0).asSInt
//       io.v_output := VectorXor_vxvi(sew_8_b,imm,16.U,sew_8_vd)
//   }
// 	  .elsewhen (io.sew === "b001".U && io.aluc === Vxorvx){
// 		val imm = io.in_A(15,0).asSInt
//       io.v_output := VectorXor_vxvi(sew_16_b,imm,8.U,sew_16_vd)
//   }
//     //vxor vx end
//     //vxor vi start
//         .elsewhen(io.sew === "b011".U && io.aluc === Vxorvi){
// 		val imm = Cat(0.S(32.W), io.in_B).asSInt
//       io.v_output := VectorXor_vxvi(sew_64_b,imm,2.U,sew_64_vd)
    
//     }.elsewhen(io.sew === "b010".U && io.aluc === Vxorvi){
//        val imm = io.in_B(31,0).asSInt
//          io.v_output := VectorXor_vxvi(sew_32_b,imm,4.U,sew_32_vd)
	  
//     }.elsewhen(io.sew === "b000".U && io.aluc === Vxorvi){
// 		val imm = io.in_B(7,0).asSInt
//       io.v_output := VectorXor_vxvi(sew_8_b,imm,16.U,sew_8_vd)
// 	  }
// 	  .elsewhen (io.sew === "b001".U && io.aluc === Vxorvi){
// 		val imm = io.in_B(15,0).asSInt
// 		io.v_output := VectorXor_vxvi(sew_16_b,imm,8.U,sew_16_vd)
// 	  }  //vxor vi end

//     //vminu vv start
//     .elsewhen (io.sew === "b011".U && io.aluc === Vminuvv){
//      io.v_output := Vectorminu_vv(sew_64_a,sew_64_b,2.U,sew_64_vd)

// 	}
// 	 .elsewhen (io.sew === "b010".U && io.aluc === Vminuvv){ // sew = 32
//      io.v_output := Vectorminu_vv(sew_32_a,sew_32_b,4.U,sew_64_vd)

//   }
// 	  .elsewhen(io.sew === "b001".U && io.aluc === Vminuvv){ //sew = 16
//      io.v_output := Vectorminu_vv(sew_16_a,sew_16_b,8.U,sew_16_vd)
// 	}
// 	  .elsewhen(io.sew === "b000".U && io.aluc === Vminuvv){ //sew = 8
//      io.v_output := Vectorminu_vv(sew_8_a,sew_8_b,16.U,sew_8_vd)
//   }
//     //vminu vv end

//      //vminu vx start
//     .elsewhen (io.sew === "b011".U && io.aluc === Vminuvx){
//       val imm = Cat(0.S(32.W), io.in_A).asSInt
//      io.v_output := Vectorminu_vxvi(sew_64_a,imm,2.U,sew_64_vd)
	
//   }.elsewhen (io.sew === "b010".U && io.aluc === Vminuvx){ // sew = 32
//     val imm = io.in_A(31,0).asSInt
//      io.v_output := Vectorminu_vxvi(sew_32_a,imm,4.U,sew_32_vd)
        
//   }.elsewhen(io.sew === "b001".U && io.aluc === Vminuvx){ //sew = 16
// 		val imm = io.in_A(15,0).asSInt
//      io.v_output := Vectorminu_vxvi(sew_16_a,imm,8.U,sew_16_vd)
 
//   }.elsewhen(io.sew === "b000".U && io.aluc === Vminuvx){ //sew = 8
// 		val imm = io.in_A(7,0).asSInt
//      io.v_output := Vectorminu_vxvi(sew_8_a,imm,16.U,sew_8_vd)
//   }
//     //vminu vx end
    

//     //vmin vv start
//     .elsewhen (io.sew === "b011".U && io.aluc === Vminvv){
//      io.v_output := Vectormin_vv(sew_64_a,sew_64_b,2.U,sew_64_vd)

// 	}
// 	 .elsewhen (io.sew === "b010".U && io.aluc === Vminvv){ // sew = 32
//      io.v_output := Vectormin_vv(sew_32_a,sew_32_b,4.U,sew_64_vd)

//   }
// 	  .elsewhen(io.sew === "b001".U && io.aluc === Vminvv){ //sew = 16
//      io.v_output := Vectormin_vv(sew_16_a,sew_16_b,8.U,sew_16_vd)
// 	}
// 	  .elsewhen(io.sew === "b000".U && io.aluc === Vminvv){ //sew = 8
//      io.v_output := Vectormin_vv(sew_8_a,sew_8_b,16.U,sew_8_vd)
//   }
  
//     //vmin vv end

//      //vmin vx start
//        .elsewhen (io.sew === "b011".U && io.aluc === Vminvx){
//       val imm = Cat(0.S(32.W), io.in_A).asSInt
//      io.v_output := Vectormin_vx(sew_64_a,imm,2.U,sew_64_vd)
	
//   }.elsewhen (io.sew === "b010".U && io.aluc === Vminvx){ // sew = 32
//     val imm = io.in_A(31,0).asSInt
//      io.v_output := Vectormin_vx(sew_32_a,imm,4.U,sew_32_vd)
        
//   }.elsewhen(io.sew === "b001".U && io.aluc === Vminvx){ //sew = 16
// 		val imm = io.in_A(15,0).asSInt
//      io.v_output := Vectormin_vx(sew_16_a,imm,8.U,sew_16_vd)
 
//   }.elsewhen(io.sew === "b000".U && io.aluc === Vminvx){ //sew = 8
// 		val imm = io.in_A(7,0).asSInt
//      io.v_output := Vectormin_vx(sew_8_a,imm,16.U,sew_8_vd)
//   }
//     //vmin vx end

//       //vmaxu vv start
//     .elsewhen (io.sew === "b011".U && io.aluc === Vmaxuvv){
//      io.v_output := Vectormaxu_vv(sew_64_a,sew_64_b,2.U,sew_64_vd)

// 	}
// 	 .elsewhen (io.sew === "b010".U && io.aluc === Vmaxuvv){ // sew = 32
//      io.v_output := Vectormaxu_vv(sew_32_a,sew_32_b,4.U,sew_64_vd)

//   }
// 	  .elsewhen(io.sew === "b001".U && io.aluc === Vmaxuvv){ //sew = 16
//      io.v_output := Vectormaxu_vv(sew_16_a,sew_16_b,8.U,sew_16_vd)
// 	}
// 	  .elsewhen(io.sew === "b000".U && io.aluc === Vmaxuvv){ //sew = 8
//      io.v_output := Vectormaxu_vv(sew_8_a,sew_8_b,16.U,sew_8_vd)
//   }
//     //vmaxu vv end

//      //vmaxu vx start
//     .elsewhen (io.sew === "b011".U && io.aluc === Vmaxuvx){
//       val imm = Cat(0.S(32.W), io.in_A).asSInt
//      io.v_output := Vectormaxu_vx(sew_64_a,imm,2.U,sew_64_vd)
	
//   }.elsewhen (io.sew === "b010".U && io.aluc === Vmaxuvx){ // sew = 32
//     val imm = io.in_A(31,0).asSInt
//      io.v_output := Vectormaxu_vx(sew_32_a,imm,4.U,sew_32_vd)
        
//   }.elsewhen(io.sew === "b001".U && io.aluc === Vmaxuvx){ //sew = 16
// 		val imm = io.in_A(15,0).asSInt
//      io.v_output := Vectormaxu_vx(sew_16_a,imm,8.U,sew_16_vd)
 
//   }.elsewhen(io.sew === "b000".U && io.aluc === Vmaxuvx){ //sew = 8
// 		val imm = io.in_A(7,0).asSInt
//      io.v_output := Vectormaxu_vx(sew_8_a,imm,16.U,sew_8_vd)
//   }
//     //vaxu vx end
    

//     //vmax vv start
//     .elsewhen (io.sew === "b011".U && io.aluc === Vmaxvv){
//      io.v_output := Vectormax_vv(sew_64_a,sew_64_b,2.U,sew_64_vd)

// 	}
// 	 .elsewhen (io.sew === "b010".U && io.aluc === Vmaxvv){ // sew = 32
//      io.v_output := Vectormax_vv(sew_32_a,sew_32_b,4.U,sew_64_vd)

//   }
// 	  .elsewhen(io.sew === "b001".U && io.aluc === Vmaxvv){ //sew = 16
//      io.v_output := Vectormax_vv(sew_16_a,sew_16_b,8.U,sew_16_vd)
// 	}
// 	  .elsewhen(io.sew === "b000".U && io.aluc === Vmaxvv){ //sew = 8
//      io.v_output := Vectormax_vv(sew_8_a,sew_8_b,16.U,sew_8_vd)
//   }
  
//     //vmax vv end

//      //vmax vx start
//   .elsewhen (io.sew === "b011".U && io.aluc === Vmaxvx){
//       val imm = Cat(0.S(32.W), io.in_A).asSInt
//      io.v_output := Vectormax_vx(sew_64_a,imm,2.U,sew_64_vd)
	
//   }.elsewhen (io.sew === "b010".U && io.aluc === Vmaxvx){ // sew = 32
//     val imm = io.in_A(31,0).asSInt
//      io.v_output := Vectormax_vx(sew_32_a,imm,4.U,sew_32_vd)
        
//   }.elsewhen(io.sew === "b001".U && io.aluc === Vmaxvx){ //sew = 16
// 		val imm = io.in_A(15,0).asSInt
//      io.v_output := Vectormax_vx(sew_16_a,imm,8.U,sew_16_vd)
 
//   }.elsewhen(io.sew === "b000".U && io.aluc === Vmaxvx){ //sew = 8
// 		val imm = io.in_A(7,0).asSInt
//      io.v_output := Vectormax_vx(sew_8_a,imm,16.U,sew_8_vd)
//   }
//     // vmax vx end
	// }}