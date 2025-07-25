; NOTE: Assertions have been autogenerated by utils/update_llc_test_checks.py
; RUN: llc < %s -mtriple=aarch64 -verify-machineinstrs -global-isel=0 | FileCheck %s --check-prefixes=CHECK,CHECK-SD
; RUN: llc < %s -mtriple=aarch64 -verify-machineinstrs -global-isel=1 | FileCheck %s --check-prefixes=CHECK,CHECK-GI

define ptr @scalar_gep_i32(ptr %b, i32 %off) {
; CHECK-LABEL: scalar_gep_i32:
; CHECK:       // %bb.0: // %entry
; CHECK-NEXT:    add x0, x0, w1, sxtw
; CHECK-NEXT:    ret
entry:
  %g = getelementptr i8, ptr %b, i32 %off
  ret ptr %g
}

define ptr @scalar_gep_i64(ptr %b, i64 %off) {
; CHECK-LABEL: scalar_gep_i64:
; CHECK:       // %bb.0: // %entry
; CHECK-NEXT:    add x0, x0, x1
; CHECK-NEXT:    ret
entry:
  %g = getelementptr i8, ptr %b, i64 %off
  ret ptr %g
}

define ptr @scalar_gep_c10(ptr %b) {
; CHECK-LABEL: scalar_gep_c10:
; CHECK:       // %bb.0: // %entry
; CHECK-NEXT:    add x0, x0, #10
; CHECK-NEXT:    ret
entry:
  %g = getelementptr i8, ptr %b, i64 10
  ret ptr %g
}

define ptr @scalar_gep_cm10(ptr %b) {
; CHECK-LABEL: scalar_gep_cm10:
; CHECK:       // %bb.0: // %entry
; CHECK-NEXT:    sub x0, x0, #10
; CHECK-NEXT:    ret
entry:
  %g = getelementptr i8, ptr %b, i64 -10
  ret ptr %g
}

define <1 x ptr> @vector_gep_v1i32(<1 x ptr> %b, <1 x i32> %off) {
; CHECK-SD-LABEL: vector_gep_v1i32:
; CHECK-SD:       // %bb.0: // %entry
; CHECK-SD-NEXT:    shl d1, d1, #32
; CHECK-SD-NEXT:    ssra d0, d1, #32
; CHECK-SD-NEXT:    ret
;
; CHECK-GI-LABEL: vector_gep_v1i32:
; CHECK-GI:       // %bb.0: // %entry
; CHECK-GI-NEXT:    // kill: def $d1 killed $d1 def $q1
; CHECK-GI-NEXT:    fmov w8, s1
; CHECK-GI-NEXT:    fmov x9, d0
; CHECK-GI-NEXT:    add x8, x9, w8, sxtw
; CHECK-GI-NEXT:    fmov d0, x8
; CHECK-GI-NEXT:    ret
entry:
  %g = getelementptr i8, <1 x ptr> %b, <1 x i32> %off
  ret <1 x ptr> %g
}

define <2 x ptr> @vector_gep_v2i32(<2 x ptr> %b, <2 x i32> %off) {
; CHECK-LABEL: vector_gep_v2i32:
; CHECK:       // %bb.0: // %entry
; CHECK-NEXT:    saddw v0.2d, v0.2d, v1.2s
; CHECK-NEXT:    ret
entry:
  %g = getelementptr i8, <2 x ptr> %b, <2 x i32> %off
  ret <2 x ptr> %g
}

define <3 x ptr> @vector_gep_v3i32(<3 x ptr> %b, <3 x i32> %off) {
; CHECK-SD-LABEL: vector_gep_v3i32:
; CHECK-SD:       // %bb.0: // %entry
; CHECK-SD-NEXT:    // kill: def $d0 killed $d0 def $q0
; CHECK-SD-NEXT:    // kill: def $d1 killed $d1 def $q1
; CHECK-SD-NEXT:    ext v4.16b, v3.16b, v3.16b, #8
; CHECK-SD-NEXT:    // kill: def $d2 killed $d2 def $q2
; CHECK-SD-NEXT:    mov v0.d[1], v1.d[0]
; CHECK-SD-NEXT:    saddw v2.2d, v2.2d, v4.2s
; CHECK-SD-NEXT:    saddw v0.2d, v0.2d, v3.2s
; CHECK-SD-NEXT:    // kill: def $d2 killed $d2 killed $q2
; CHECK-SD-NEXT:    ext v1.16b, v0.16b, v0.16b, #8
; CHECK-SD-NEXT:    // kill: def $d0 killed $d0 killed $q0
; CHECK-SD-NEXT:    // kill: def $d1 killed $d1 killed $q1
; CHECK-SD-NEXT:    ret
;
; CHECK-GI-LABEL: vector_gep_v3i32:
; CHECK-GI:       // %bb.0: // %entry
; CHECK-GI-NEXT:    smov x9, v3.s[0]
; CHECK-GI-NEXT:    smov x10, v3.s[1]
; CHECK-GI-NEXT:    // kill: def $d0 killed $d0 def $q0
; CHECK-GI-NEXT:    fmov x8, d1
; CHECK-GI-NEXT:    mov v0.d[1], x8
; CHECK-GI-NEXT:    mov w8, v3.s[2]
; CHECK-GI-NEXT:    fmov d1, x9
; CHECK-GI-NEXT:    fmov x9, d2
; CHECK-GI-NEXT:    mov v1.d[1], x10
; CHECK-GI-NEXT:    add x8, x9, w8, sxtw
; CHECK-GI-NEXT:    fmov d2, x8
; CHECK-GI-NEXT:    add v0.2d, v0.2d, v1.2d
; CHECK-GI-NEXT:    mov d1, v0.d[1]
; CHECK-GI-NEXT:    ret
entry:
  %g = getelementptr i8, <3 x ptr> %b, <3 x i32> %off
  ret <3 x ptr> %g
}

define <4 x ptr> @vector_gep_v4i32(<4 x ptr> %b, <4 x i32> %off) {
; CHECK-SD-LABEL: vector_gep_v4i32:
; CHECK-SD:       // %bb.0: // %entry
; CHECK-SD-NEXT:    saddw2 v1.2d, v1.2d, v2.4s
; CHECK-SD-NEXT:    saddw v0.2d, v0.2d, v2.2s
; CHECK-SD-NEXT:    ret
;
; CHECK-GI-LABEL: vector_gep_v4i32:
; CHECK-GI:       // %bb.0: // %entry
; CHECK-GI-NEXT:    saddw v0.2d, v0.2d, v2.2s
; CHECK-GI-NEXT:    saddw2 v1.2d, v1.2d, v2.4s
; CHECK-GI-NEXT:    ret
entry:
  %g = getelementptr i8, <4 x ptr> %b, <4 x i32> %off
  ret <4 x ptr> %g
}

define <1 x ptr> @vector_gep_v1i64(<1 x ptr> %b, <1 x i64> %off) {
; CHECK-SD-LABEL: vector_gep_v1i64:
; CHECK-SD:       // %bb.0: // %entry
; CHECK-SD-NEXT:    add d0, d0, d1
; CHECK-SD-NEXT:    ret
;
; CHECK-GI-LABEL: vector_gep_v1i64:
; CHECK-GI:       // %bb.0: // %entry
; CHECK-GI-NEXT:    fmov x8, d0
; CHECK-GI-NEXT:    fmov x9, d1
; CHECK-GI-NEXT:    add x8, x8, x9
; CHECK-GI-NEXT:    fmov d0, x8
; CHECK-GI-NEXT:    ret
entry:
  %g = getelementptr i8, <1 x ptr> %b, <1 x i64> %off
  ret <1 x ptr> %g
}

define <2 x ptr> @vector_gep_v2i64(<2 x ptr> %b, <2 x i64> %off) {
; CHECK-LABEL: vector_gep_v2i64:
; CHECK:       // %bb.0: // %entry
; CHECK-NEXT:    add v0.2d, v0.2d, v1.2d
; CHECK-NEXT:    ret
entry:
  %g = getelementptr i8, <2 x ptr> %b, <2 x i64> %off
  ret <2 x ptr> %g
}

define <3 x ptr> @vector_gep_v3i64(<3 x ptr> %b, <3 x i64> %off) {
; CHECK-SD-LABEL: vector_gep_v3i64:
; CHECK-SD:       // %bb.0: // %entry
; CHECK-SD-NEXT:    add d0, d0, d3
; CHECK-SD-NEXT:    add d1, d1, d4
; CHECK-SD-NEXT:    add d2, d2, d5
; CHECK-SD-NEXT:    ret
;
; CHECK-GI-LABEL: vector_gep_v3i64:
; CHECK-GI:       // %bb.0: // %entry
; CHECK-GI-NEXT:    fmov x8, d1
; CHECK-GI-NEXT:    // kill: def $d0 killed $d0 def $q0
; CHECK-GI-NEXT:    // kill: def $d3 killed $d3 def $q3
; CHECK-GI-NEXT:    // kill: def $d4 killed $d4 def $q4
; CHECK-GI-NEXT:    fmov x9, d5
; CHECK-GI-NEXT:    mov v3.d[1], v4.d[0]
; CHECK-GI-NEXT:    mov v0.d[1], x8
; CHECK-GI-NEXT:    fmov x8, d2
; CHECK-GI-NEXT:    add x8, x8, x9
; CHECK-GI-NEXT:    add v0.2d, v0.2d, v3.2d
; CHECK-GI-NEXT:    fmov d2, x8
; CHECK-GI-NEXT:    mov d1, v0.d[1]
; CHECK-GI-NEXT:    ret
entry:
  %g = getelementptr i8, <3 x ptr> %b, <3 x i64> %off
  ret <3 x ptr> %g
}

define <4 x ptr> @vector_gep_v4i64(<4 x ptr> %b, <4 x i64> %off) {
; CHECK-SD-LABEL: vector_gep_v4i64:
; CHECK-SD:       // %bb.0: // %entry
; CHECK-SD-NEXT:    add v1.2d, v1.2d, v3.2d
; CHECK-SD-NEXT:    add v0.2d, v0.2d, v2.2d
; CHECK-SD-NEXT:    ret
;
; CHECK-GI-LABEL: vector_gep_v4i64:
; CHECK-GI:       // %bb.0: // %entry
; CHECK-GI-NEXT:    add v0.2d, v0.2d, v2.2d
; CHECK-GI-NEXT:    add v1.2d, v1.2d, v3.2d
; CHECK-GI-NEXT:    ret
entry:
  %g = getelementptr i8, <4 x ptr> %b, <4 x i64> %off
  ret <4 x ptr> %g
}

define <2 x ptr> @vector_gep_v4i128(<2 x ptr> %b, <2 x i128> %off) {
; CHECK-LABEL: vector_gep_v4i128:
; CHECK:       // %bb.0: // %entry
; CHECK-NEXT:    fmov d1, x0
; CHECK-NEXT:    mov v1.d[1], x2
; CHECK-NEXT:    add v0.2d, v0.2d, v1.2d
; CHECK-NEXT:    ret
entry:
  %g = getelementptr i8, <2 x ptr> %b, <2 x i128> %off
  ret <2 x ptr> %g
}


define <1 x ptr> @vector_gep_v1i64_base(ptr %b, <1 x i64> %off) {
; CHECK-SD-LABEL: vector_gep_v1i64_base:
; CHECK-SD:       // %bb.0: // %entry
; CHECK-SD-NEXT:    fmov d1, x0
; CHECK-SD-NEXT:    add d0, d1, d0
; CHECK-SD-NEXT:    ret
;
; CHECK-GI-LABEL: vector_gep_v1i64_base:
; CHECK-GI:       // %bb.0: // %entry
; CHECK-GI-NEXT:    fmov x8, d0
; CHECK-GI-NEXT:    add x8, x0, x8
; CHECK-GI-NEXT:    fmov d0, x8
; CHECK-GI-NEXT:    ret
entry:
  %g = getelementptr i8, ptr %b, <1 x i64> %off
  ret <1 x ptr> %g
}

define <2 x ptr> @vector_gep_v2i64_base(ptr %b, <2 x i64> %off) {
; CHECK-LABEL: vector_gep_v2i64_base:
; CHECK:       // %bb.0: // %entry
; CHECK-NEXT:    dup v1.2d, x0
; CHECK-NEXT:    add v0.2d, v1.2d, v0.2d
; CHECK-NEXT:    ret
entry:
  %g = getelementptr i8, ptr %b, <2 x i64> %off
  ret <2 x ptr> %g
}

define <3 x ptr> @vector_gep_v3i64_base(ptr %b, <3 x i64> %off) {
; CHECK-SD-LABEL: vector_gep_v3i64_base:
; CHECK-SD:       // %bb.0: // %entry
; CHECK-SD-NEXT:    // kill: def $d0 killed $d0 def $q0
; CHECK-SD-NEXT:    // kill: def $d1 killed $d1 def $q1
; CHECK-SD-NEXT:    fmov d3, x0
; CHECK-SD-NEXT:    mov v0.d[1], v1.d[0]
; CHECK-SD-NEXT:    dup v1.2d, x0
; CHECK-SD-NEXT:    add d2, d3, d2
; CHECK-SD-NEXT:    add v0.2d, v1.2d, v0.2d
; CHECK-SD-NEXT:    ext v1.16b, v0.16b, v0.16b, #8
; CHECK-SD-NEXT:    // kill: def $d0 killed $d0 killed $q0
; CHECK-SD-NEXT:    // kill: def $d1 killed $d1 killed $q1
; CHECK-SD-NEXT:    ret
;
; CHECK-GI-LABEL: vector_gep_v3i64_base:
; CHECK-GI:       // %bb.0: // %entry
; CHECK-GI-NEXT:    // kill: def $d0 killed $d0 def $q0
; CHECK-GI-NEXT:    // kill: def $d1 killed $d1 def $q1
; CHECK-GI-NEXT:    fmov x8, d2
; CHECK-GI-NEXT:    mov v0.d[1], v1.d[0]
; CHECK-GI-NEXT:    dup v1.2d, x0
; CHECK-GI-NEXT:    add x8, x0, x8
; CHECK-GI-NEXT:    fmov d2, x8
; CHECK-GI-NEXT:    add v0.2d, v1.2d, v0.2d
; CHECK-GI-NEXT:    mov d1, v0.d[1]
; CHECK-GI-NEXT:    ret
entry:
  %g = getelementptr i8, ptr %b, <3 x i64> %off
  ret <3 x ptr> %g
}

define <4 x ptr> @vector_gep_v4i64_base(ptr %b, <4 x i64> %off) {
; CHECK-LABEL: vector_gep_v4i64_base:
; CHECK:       // %bb.0: // %entry
; CHECK-NEXT:    dup v2.2d, x0
; CHECK-NEXT:    add v0.2d, v2.2d, v0.2d
; CHECK-NEXT:    add v1.2d, v2.2d, v1.2d
; CHECK-NEXT:    ret
entry:
  %g = getelementptr i8, ptr %b, <4 x i64> %off
  ret <4 x ptr> %g
}

define <1 x ptr> @vector_gep_v1i64_c10(ptr %b) {
; CHECK-SD-LABEL: vector_gep_v1i64_c10:
; CHECK-SD:       // %bb.0: // %entry
; CHECK-SD-NEXT:    mov w8, #10 // =0xa
; CHECK-SD-NEXT:    fmov d0, x0
; CHECK-SD-NEXT:    fmov d1, x8
; CHECK-SD-NEXT:    add d0, d0, d1
; CHECK-SD-NEXT:    ret
;
; CHECK-GI-LABEL: vector_gep_v1i64_c10:
; CHECK-GI:       // %bb.0: // %entry
; CHECK-GI-NEXT:    add x8, x0, #10
; CHECK-GI-NEXT:    fmov d0, x8
; CHECK-GI-NEXT:    ret
entry:
  %g = getelementptr i8, ptr %b, <1 x i64> <i64 10>
  ret <1 x ptr> %g
}

define <2 x ptr> @vector_gep_v2i64_c10(ptr %b) {
; CHECK-SD-LABEL: vector_gep_v2i64_c10:
; CHECK-SD:       // %bb.0: // %entry
; CHECK-SD-NEXT:    mov w8, #10 // =0xa
; CHECK-SD-NEXT:    dup v0.2d, x0
; CHECK-SD-NEXT:    dup v1.2d, x8
; CHECK-SD-NEXT:    add v0.2d, v0.2d, v1.2d
; CHECK-SD-NEXT:    ret
;
; CHECK-GI-LABEL: vector_gep_v2i64_c10:
; CHECK-GI:       // %bb.0: // %entry
; CHECK-GI-NEXT:    adrp x8, .LCPI18_0
; CHECK-GI-NEXT:    dup v0.2d, x0
; CHECK-GI-NEXT:    ldr q1, [x8, :lo12:.LCPI18_0]
; CHECK-GI-NEXT:    add v0.2d, v0.2d, v1.2d
; CHECK-GI-NEXT:    ret
entry:
  %g = getelementptr i8, ptr %b, <2 x i64> <i64 10, i64 10>
  ret <2 x ptr> %g
}

define <3 x ptr> @vector_gep_v3i64_c10(ptr %b) {
; CHECK-SD-LABEL: vector_gep_v3i64_c10:
; CHECK-SD:       // %bb.0: // %entry
; CHECK-SD-NEXT:    mov w8, #10 // =0xa
; CHECK-SD-NEXT:    dup v0.2d, x0
; CHECK-SD-NEXT:    fmov d3, x0
; CHECK-SD-NEXT:    dup v2.2d, x8
; CHECK-SD-NEXT:    add v0.2d, v0.2d, v2.2d
; CHECK-SD-NEXT:    add d2, d3, d2
; CHECK-SD-NEXT:    ext v1.16b, v0.16b, v0.16b, #8
; CHECK-SD-NEXT:    // kill: def $d0 killed $d0 killed $q0
; CHECK-SD-NEXT:    // kill: def $d1 killed $d1 killed $q1
; CHECK-SD-NEXT:    ret
;
; CHECK-GI-LABEL: vector_gep_v3i64_c10:
; CHECK-GI:       // %bb.0: // %entry
; CHECK-GI-NEXT:    adrp x8, .LCPI19_0
; CHECK-GI-NEXT:    dup v0.2d, x0
; CHECK-GI-NEXT:    ldr q1, [x8, :lo12:.LCPI19_0]
; CHECK-GI-NEXT:    add x8, x0, #10
; CHECK-GI-NEXT:    fmov d2, x8
; CHECK-GI-NEXT:    add v0.2d, v0.2d, v1.2d
; CHECK-GI-NEXT:    mov d1, v0.d[1]
; CHECK-GI-NEXT:    ret
entry:
  %g = getelementptr i8, ptr %b, <3 x i64> <i64 10, i64 10, i64 10>
  ret <3 x ptr> %g
}

define <4 x ptr> @vector_gep_v4i64_c10(ptr %b) {
; CHECK-SD-LABEL: vector_gep_v4i64_c10:
; CHECK-SD:       // %bb.0: // %entry
; CHECK-SD-NEXT:    mov w8, #10 // =0xa
; CHECK-SD-NEXT:    dup v0.2d, x0
; CHECK-SD-NEXT:    dup v1.2d, x8
; CHECK-SD-NEXT:    add v0.2d, v0.2d, v1.2d
; CHECK-SD-NEXT:    mov v1.16b, v0.16b
; CHECK-SD-NEXT:    ret
;
; CHECK-GI-LABEL: vector_gep_v4i64_c10:
; CHECK-GI:       // %bb.0: // %entry
; CHECK-GI-NEXT:    adrp x8, .LCPI20_0
; CHECK-GI-NEXT:    dup v0.2d, x0
; CHECK-GI-NEXT:    ldr q1, [x8, :lo12:.LCPI20_0]
; CHECK-GI-NEXT:    add v0.2d, v0.2d, v1.2d
; CHECK-GI-NEXT:    mov v1.16b, v0.16b
; CHECK-GI-NEXT:    ret
entry:
  %g = getelementptr i8, ptr %b, <4 x i64> <i64 10, i64 10, i64 10, i64 10>
  ret <4 x ptr> %g
}

define <1 x ptr> @vector_gep_v1i64_cm10(ptr %b) {
; CHECK-SD-LABEL: vector_gep_v1i64_cm10:
; CHECK-SD:       // %bb.0: // %entry
; CHECK-SD-NEXT:    mov x8, #-10 // =0xfffffffffffffff6
; CHECK-SD-NEXT:    fmov d1, x0
; CHECK-SD-NEXT:    fmov d0, x8
; CHECK-SD-NEXT:    add d0, d1, d0
; CHECK-SD-NEXT:    ret
;
; CHECK-GI-LABEL: vector_gep_v1i64_cm10:
; CHECK-GI:       // %bb.0: // %entry
; CHECK-GI-NEXT:    sub x8, x0, #10
; CHECK-GI-NEXT:    fmov d0, x8
; CHECK-GI-NEXT:    ret
entry:
  %g = getelementptr i8, ptr %b, <1 x i64> <i64 -10>
  ret <1 x ptr> %g
}

define <2 x ptr> @vector_gep_v2i64_cm10(ptr %b) {
; CHECK-SD-LABEL: vector_gep_v2i64_cm10:
; CHECK-SD:       // %bb.0: // %entry
; CHECK-SD-NEXT:    mov x8, #-10 // =0xfffffffffffffff6
; CHECK-SD-NEXT:    dup v1.2d, x0
; CHECK-SD-NEXT:    dup v0.2d, x8
; CHECK-SD-NEXT:    add v0.2d, v1.2d, v0.2d
; CHECK-SD-NEXT:    ret
;
; CHECK-GI-LABEL: vector_gep_v2i64_cm10:
; CHECK-GI:       // %bb.0: // %entry
; CHECK-GI-NEXT:    adrp x8, .LCPI22_0
; CHECK-GI-NEXT:    dup v0.2d, x0
; CHECK-GI-NEXT:    ldr q1, [x8, :lo12:.LCPI22_0]
; CHECK-GI-NEXT:    add v0.2d, v0.2d, v1.2d
; CHECK-GI-NEXT:    ret
entry:
  %g = getelementptr i8, ptr %b, <2 x i64> <i64 -10, i64 -10>
  ret <2 x ptr> %g
}

define <3 x ptr> @vector_gep_v3i64_cm10(ptr %b) {
; CHECK-SD-LABEL: vector_gep_v3i64_cm10:
; CHECK-SD:       // %bb.0: // %entry
; CHECK-SD-NEXT:    mov x8, #-10 // =0xfffffffffffffff6
; CHECK-SD-NEXT:    dup v0.2d, x0
; CHECK-SD-NEXT:    fmov d3, x0
; CHECK-SD-NEXT:    dup v2.2d, x8
; CHECK-SD-NEXT:    add v0.2d, v0.2d, v2.2d
; CHECK-SD-NEXT:    add d2, d3, d2
; CHECK-SD-NEXT:    ext v1.16b, v0.16b, v0.16b, #8
; CHECK-SD-NEXT:    // kill: def $d0 killed $d0 killed $q0
; CHECK-SD-NEXT:    // kill: def $d1 killed $d1 killed $q1
; CHECK-SD-NEXT:    ret
;
; CHECK-GI-LABEL: vector_gep_v3i64_cm10:
; CHECK-GI:       // %bb.0: // %entry
; CHECK-GI-NEXT:    adrp x8, .LCPI23_0
; CHECK-GI-NEXT:    dup v0.2d, x0
; CHECK-GI-NEXT:    ldr q1, [x8, :lo12:.LCPI23_0]
; CHECK-GI-NEXT:    sub x8, x0, #10
; CHECK-GI-NEXT:    fmov d2, x8
; CHECK-GI-NEXT:    add v0.2d, v0.2d, v1.2d
; CHECK-GI-NEXT:    mov d1, v0.d[1]
; CHECK-GI-NEXT:    ret
entry:
  %g = getelementptr i8, ptr %b, <3 x i64> <i64 -10, i64 -10, i64 -10>
  ret <3 x ptr> %g
}

define <4 x ptr> @vector_gep_v4i64_cm10(ptr %b) {
; CHECK-SD-LABEL: vector_gep_v4i64_cm10:
; CHECK-SD:       // %bb.0: // %entry
; CHECK-SD-NEXT:    mov x8, #-10 // =0xfffffffffffffff6
; CHECK-SD-NEXT:    dup v1.2d, x0
; CHECK-SD-NEXT:    dup v0.2d, x8
; CHECK-SD-NEXT:    add v0.2d, v1.2d, v0.2d
; CHECK-SD-NEXT:    mov v1.16b, v0.16b
; CHECK-SD-NEXT:    ret
;
; CHECK-GI-LABEL: vector_gep_v4i64_cm10:
; CHECK-GI:       // %bb.0: // %entry
; CHECK-GI-NEXT:    adrp x8, .LCPI24_0
; CHECK-GI-NEXT:    dup v0.2d, x0
; CHECK-GI-NEXT:    ldr q1, [x8, :lo12:.LCPI24_0]
; CHECK-GI-NEXT:    add v0.2d, v0.2d, v1.2d
; CHECK-GI-NEXT:    mov v1.16b, v0.16b
; CHECK-GI-NEXT:    ret
entry:
  %g = getelementptr i8, ptr %b, <4 x i64> <i64 -10, i64 -10, i64 -10, i64 -10>
  ret <4 x ptr> %g
}
