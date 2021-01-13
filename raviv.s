;;; All the macros and the scheme-object printing procedure
;;; are defined in compiler.s
%include "compiler.s"

section .bss
;;; This pointer is used to manage allocations on our heap.
malloc_pointer:
    resq 1

;;; here we REServe enough Quad-words (64-bit "cells") for the free variables
;;; each free variable has 8 bytes reserved for a 64-bit pointer to its value
fvar_tbl:
    resq 35

section .data
const_tbl:
MAKE_VOID
MAKE_NIL
MAKE_LITERAL_BOOL (0)
MAKE_LITERAL_BOOL (1)
MAKE_LITERAL_STRING {119,104,97,116,101,118,101,114} 
MAKE_LITERAL_SYMBOL(const_tbl+6)
MAKE_LITERAL_RATIONAL(0, 1)  ;32
MAKE_LITERAL_RATIONAL(11, 1)  ;49

;;; These macro definitions are required for the primitive
;;; definitions in the epilogue to work properly
%define SOB_VOID_ADDRESS const_tbl+0
%define SOB_NIL_ADDRESS const_tbl+1
%define SOB_FALSE_ADDRESS const_tbl+2
%define SOB_TRUE_ADDRESS const_tbl+4

global main
section .text
main:
    ;; set up the heap
    mov rdi, GB(2)
    call malloc
    mov [malloc_pointer], rax

    ;; Set up the dummy activation frame
    ;; The dummy return address is T_UNDEFINED
    ;; (which a is a macro for 0) so that returning
    ;; from the top level (which SHOULD NOT HAPPEN
    ;; AND IS A BUG) will cause a segfault.
    push SOB_NIL_ADDRESS  ; oded and raviv addition, the magic argument for the dummy frame as well 
    push 0                ; argument count
    push SOB_NIL_ADDRESS  ; lexical environment address
    push T_UNDEFINED      ; return address
    push rbp                    
    mov rbp, rsp                ; anchor the dummy frame

    ;; Set up the primitive stdlib fvars:
    ;; Since the primtive procedures are defined in assembly,
    ;; they are not generated by scheme (define ...) expressions.
    ;; This is where we simulate the missing (define ...) expressions
    ;; for all the primitive procedures.
MAKE_CLOSURE(rax, SOB_NIL_ADDRESS, boolean?)
mov [fvar_tbl+24], rax
MAKE_CLOSURE(rax, SOB_NIL_ADDRESS, flonum?)
mov [fvar_tbl+32], rax
MAKE_CLOSURE(rax, SOB_NIL_ADDRESS, rational?)
mov [fvar_tbl+40], rax
MAKE_CLOSURE(rax, SOB_NIL_ADDRESS, pair?)
mov [fvar_tbl+48], rax
MAKE_CLOSURE(rax, SOB_NIL_ADDRESS, null?)
mov [fvar_tbl+56], rax
MAKE_CLOSURE(rax, SOB_NIL_ADDRESS, char?)
mov [fvar_tbl+64], rax
MAKE_CLOSURE(rax, SOB_NIL_ADDRESS, string?)
mov [fvar_tbl+72], rax
MAKE_CLOSURE(rax, SOB_NIL_ADDRESS, procedure?)
mov [fvar_tbl+80], rax
MAKE_CLOSURE(rax, SOB_NIL_ADDRESS, symbol?)
mov [fvar_tbl+88], rax
MAKE_CLOSURE(rax, SOB_NIL_ADDRESS, string_length)
mov [fvar_tbl+96], rax
MAKE_CLOSURE(rax, SOB_NIL_ADDRESS, string_ref)
mov [fvar_tbl+104], rax
MAKE_CLOSURE(rax, SOB_NIL_ADDRESS, string_set)
mov [fvar_tbl+112], rax
MAKE_CLOSURE(rax, SOB_NIL_ADDRESS, make_string)
mov [fvar_tbl+120], rax
MAKE_CLOSURE(rax, SOB_NIL_ADDRESS, symbol_to_string)
mov [fvar_tbl+128], rax
MAKE_CLOSURE(rax, SOB_NIL_ADDRESS, char_to_integer)
mov [fvar_tbl+136], rax
MAKE_CLOSURE(rax, SOB_NIL_ADDRESS, integer_to_char)
mov [fvar_tbl+144], rax
MAKE_CLOSURE(rax, SOB_NIL_ADDRESS, exact_to_inexact)
mov [fvar_tbl+152], rax
MAKE_CLOSURE(rax, SOB_NIL_ADDRESS, eq?)
mov [fvar_tbl+160], rax
MAKE_CLOSURE(rax, SOB_NIL_ADDRESS, add)
mov [fvar_tbl+168], rax
MAKE_CLOSURE(rax, SOB_NIL_ADDRESS, mul)
mov [fvar_tbl+176], rax
MAKE_CLOSURE(rax, SOB_NIL_ADDRESS, div)
mov [fvar_tbl+184], rax
MAKE_CLOSURE(rax, SOB_NIL_ADDRESS, eq)
mov [fvar_tbl+192], rax
MAKE_CLOSURE(rax, SOB_NIL_ADDRESS, lt)
mov [fvar_tbl+200], rax
MAKE_CLOSURE(rax, SOB_NIL_ADDRESS, numerator)
mov [fvar_tbl+208], rax
MAKE_CLOSURE(rax, SOB_NIL_ADDRESS, denominator)
mov [fvar_tbl+216], rax
MAKE_CLOSURE(rax, SOB_NIL_ADDRESS, gcd)
mov [fvar_tbl+224], rax
MAKE_CLOSURE(rax, SOB_NIL_ADDRESS, car)
mov [fvar_tbl+0], rax
MAKE_CLOSURE(rax, SOB_NIL_ADDRESS, cdr)
mov [fvar_tbl+8], rax
MAKE_CLOSURE(rax, SOB_NIL_ADDRESS, cons)
mov [fvar_tbl+232], rax
MAKE_CLOSURE(rax, SOB_NIL_ADDRESS, set_car)
mov [fvar_tbl+240], rax
MAKE_CLOSURE(rax, SOB_NIL_ADDRESS, set_cdr)
mov [fvar_tbl+248], rax

user_code_fragment:
;;; The code you compiled will be added here.
;;; It will be executed immediately after the closures for 
;;; the primitive procedures are set up.
;working on Applic
push SOB_NIL_ADDRESS 
;working on const
mov rax, const_tbl+23 
;finishing working on const
push rax
;working on const
mov rax, const_tbl+23 
;finishing working on const
push rax
mov rbx, 2
push rbx
;working on LambdaSimple
push rbx
  push rcx
  push rdx
  push rsi
  mov rbx, 0
  cmp rbx,0
  jne not_empty0
  mov rbx, SOB_NIL_ADDRESS
  MAKE_CLOSURE(rax, rbx, Lcode0)
  jmp after_closure0
  not_empty0:
  MALLOC rax, WORD_SIZE*0; allocate new enviorment 
mov rbx,[rbp +2 *WORD_SIZE]
  mov rcx,0 
  env_copy0:
  cmp rcx, 0
je finish_env_copy0
mov rdx, [rbx + rcx * WORD_SIZE]
  inc rcx
  mov [rax + rcx * WORD_SIZE], rdx
  jmp env_copy0
finish_env_copy0:
  mov rbx, [rbp + 3 * WORD_SIZE]
  cmp rbx,0
  jne allocate_args0
mov rdx, SOB_NIL_ADDRESS
  jmp finish_copy_args0
allocate_args0:
  shl rbx,3
  MALLOC rdx, rbx
  shr rbx,3
  mov rcx,0
  copy_args0:
  cmp rcx,rbx
  je finish_copy_args0

  mov rsi, PVAR(rcx)
  mov [rdx + rcx *WORD_SIZE ],rsi
  inc rcx
  jmp copy_args0
finish_copy_args0:
  mov [rax + 0 * WORD_SIZE] , rdx ;place at envorment 0
  mov rbx,rax
  MAKE_CLOSURE(rax, rbx, Lcode0)

  after_closure0:
  pop rsi
  pop rdx
  pop rcx
  pop rbx
  jmp Lcont0
Lcode0:
    push rbp
    mov rbp,rsp 
;working on seq
;working on Set VarParam
;working on Box
MALLOC rbx, WORD_SIZE
;working on Var param
mov rax, qword [rbp + WORD_SIZE * (4 + 0)] 
;finishing working on Var param
mov qword [rbx] , rax
mov rax, rbx 
;finishing working on Box
mov qword [rbp + WORD_SIZE * (4 + 0)], rax
mov rax, SOB_VOID_ADDRESS
;finishing working on Set VarParam

;working on Set VarParam
;working on Box
MALLOC rbx, WORD_SIZE
;working on Var param
mov rax, qword [rbp + WORD_SIZE * (4 + 1)] 
;finishing working on Var param
mov qword [rbx] , rax
mov rax, rbx 
;finishing working on Box
mov qword [rbp + WORD_SIZE * (4 + 1)], rax
mov rax, SOB_VOID_ADDRESS
;finishing working on Set VarParam

;working on BoxSet
;working on LambdaSimple
push rbx
  push rcx
  push rdx
  push rsi
  mov rbx, 1
  cmp rbx,0
  jne not_empty1
  mov rbx, SOB_NIL_ADDRESS
  MAKE_CLOSURE(rax, rbx, Lcode1)
  jmp after_closure1
  not_empty1:
  MALLOC rax, WORD_SIZE*1; allocate new enviorment 
mov rbx,[rbp +2 *WORD_SIZE]
  mov rcx,0 
  env_copy1:
  cmp rcx, 1
je finish_env_copy1
mov rdx, [rbx + rcx * WORD_SIZE]
  inc rcx
  mov [rax + rcx * WORD_SIZE], rdx
  jmp env_copy1
finish_env_copy1:
  mov rbx, [rbp + 3 * WORD_SIZE]
  cmp rbx,0
  jne allocate_args1
mov rdx, SOB_NIL_ADDRESS
  jmp finish_copy_args1
allocate_args1:
  shl rbx,3
  MALLOC rdx, rbx
  shr rbx,3
  mov rcx,0
  copy_args1:
  cmp rcx,rbx
  je finish_copy_args1

  mov rsi, PVAR(rcx)
  mov [rdx + rcx *WORD_SIZE ],rsi
  inc rcx
  jmp copy_args1
finish_copy_args1:
  mov [rax + 0 * WORD_SIZE] , rdx ;place at envorment 0
  mov rbx,rax
  MAKE_CLOSURE(rax, rbx, Lcode1)

  after_closure1:
  pop rsi
  pop rdx
  pop rcx
  pop rbx
  jmp Lcont1
Lcode1:
    push rbp
    mov rbp,rsp 
;working on seq
;working on Set VarParam
;working on Box
MALLOC rbx, WORD_SIZE
;working on Var param
mov rax, qword [rbp + WORD_SIZE * (4 + 0)] 
;finishing working on Var param
mov qword [rbx] , rax
mov rax, rbx 
;finishing working on Box
mov qword [rbp + WORD_SIZE * (4 + 0)], rax
mov rax, SOB_VOID_ADDRESS
;finishing working on Set VarParam

;working on Or
;working on Applic
push SOB_NIL_ADDRESS 
;working on BoxGet
;working on Var param
mov rax, qword [rbp + WORD_SIZE * (4 + 0)] 
;finishing working on Var param
mov rax, qword [rax]
;finishing working on BoxGet
push rax
;working on const
mov rax, const_tbl+32 
;finishing working on const
push rax
mov rbx, 2
push rbx
;working on VarFree
mov rax, qword [fvar_tbl+192]
;finishing working on VarFree
cmp qword[rax + 0 * WORD_SIZE], T_CLOSURE
                                  CLOSURE_ENV rbx, rax
                                  push rbx
                                  CLOSURE_CODE rbx, rax
                                  call rbx
                                  add rsp, 8 ; delete env from stack
                                  pop rbx ; keep arg_count in rbx
                                  inc rbx ; add 1 for the magic to clean
                                  shl rbx, 3
                                  add rsp, rbx ; delete args and magic 

                                          ;                   pop rbx ; restore rbx value
;finishing working on Applic
cmp rax, SOB_FALSE_ADDRESS
jne Lexit2
;working on Applic
push SOB_NIL_ADDRESS 
;working on Applic
push SOB_NIL_ADDRESS 
;working on BoxGet
;working on Var param
mov rax, qword [rbp + WORD_SIZE * (4 + 0)] 
;finishing working on Var param
mov rax, qword [rax]
;finishing working on BoxGet
push rax
mov rbx, 1
push rbx
;working on VarFree
mov rax, qword [fvar_tbl+256]
;finishing working on VarFree
cmp qword[rax + 0 * WORD_SIZE], T_CLOSURE
                                  CLOSURE_ENV rbx, rax
                                  push rbx
                                  CLOSURE_CODE rbx, rax
                                  call rbx
                                  add rsp, 8 ; delete env from stack
                                  pop rbx ; keep arg_count in rbx
                                  inc rbx ; add 1 for the magic to clean
                                  shl rbx, 3
                                  add rsp, rbx ; delete args and magic 

                                          ;                   pop rbx ; restore rbx value
;finishing working on Applic
push rax
mov rbx, 1
push rbx
;working on BoxGet
;working on VarBound
mov rax, qword [rbp + WORD_SIZE * 2]
mov rax, qword [rax + WORD_SIZE * 0]
mov rax, qword [rax + WORD_SIZE * 1]
;finishing working on VarBound
mov rax, qword [rax]
;finishing working on BoxGet
cmp qword[rax + 0 * WORD_SIZE], T_CLOSURE
                                  CLOSURE_ENV rbx, rax
                                  push rbx
                                  CLOSURE_CODE rbx, rax
                                  call rbx
                                  add rsp, 8 ; delete env from stack
                                  pop rbx ; keep arg_count in rbx
                                  inc rbx ; add 1 for the magic to clean
                                  shl rbx, 3
                                  add rsp, rbx ; delete args and magic 

                                          ;                   pop rbx ; restore rbx value
;finishing working on Applic
Lexit2:
;finishing working on Or
;finishing working on seq

leave
    ret
    Lcont1:
;finishing working on LambdaSimple
push rax
;working on Var param
mov rax, qword [rbp + WORD_SIZE * (4 + 0)] 
;finishing working on Var param
pop qword [rax]
mov rax, SOB_VOID_ADDRESS
;finishing working on BoxSet

;working on BoxSet
;working on LambdaSimple
push rbx
  push rcx
  push rdx
  push rsi
  mov rbx, 1
  cmp rbx,0
  jne not_empty3
  mov rbx, SOB_NIL_ADDRESS
  MAKE_CLOSURE(rax, rbx, Lcode3)
  jmp after_closure3
  not_empty3:
  MALLOC rax, WORD_SIZE*1; allocate new enviorment 
mov rbx,[rbp +2 *WORD_SIZE]
  mov rcx,0 
  env_copy3:
  cmp rcx, 1
je finish_env_copy3
mov rdx, [rbx + rcx * WORD_SIZE]
  inc rcx
  mov [rax + rcx * WORD_SIZE], rdx
  jmp env_copy3
finish_env_copy3:
  mov rbx, [rbp + 3 * WORD_SIZE]
  cmp rbx,0
  jne allocate_args3
mov rdx, SOB_NIL_ADDRESS
  jmp finish_copy_args3
allocate_args3:
  shl rbx,3
  MALLOC rdx, rbx
  shr rbx,3
  mov rcx,0
  copy_args3:
  cmp rcx,rbx
  je finish_copy_args3

  mov rsi, PVAR(rcx)
  mov [rdx + rcx *WORD_SIZE ],rsi
  inc rcx
  jmp copy_args3
finish_copy_args3:
  mov [rax + 0 * WORD_SIZE] , rdx ;place at envorment 0
  mov rbx,rax
  MAKE_CLOSURE(rax, rbx, Lcode3)

  after_closure3:
  pop rsi
  pop rdx
  pop rcx
  pop rbx
  jmp Lcont3
Lcode3:
    push rbp
    mov rbp,rsp 
;working on seq
;working on Set VarParam
;working on Box
MALLOC rbx, WORD_SIZE
;working on Var param
mov rax, qword [rbp + WORD_SIZE * (4 + 0)] 
;finishing working on Var param
mov qword [rbx] , rax
mov rax, rbx 
;finishing working on Box
mov qword [rbp + WORD_SIZE * (4 + 0)], rax
mov rax, SOB_VOID_ADDRESS
;finishing working on Set VarParam

;working on Applic
push SOB_NIL_ADDRESS 
;working on Applic
push SOB_NIL_ADDRESS 
;working on BoxGet
;working on Var param
mov rax, qword [rbp + WORD_SIZE * (4 + 0)] 
;finishing working on Var param
mov rax, qword [rax]
;finishing working on BoxGet
push rax
;working on const
mov rax, const_tbl+32 
;finishing working on const
push rax
mov rbx, 2
push rbx
;working on VarFree
mov rax, qword [fvar_tbl+192]
;finishing working on VarFree
cmp qword[rax + 0 * WORD_SIZE], T_CLOSURE
                                  CLOSURE_ENV rbx, rax
                                  push rbx
                                  CLOSURE_CODE rbx, rax
                                  call rbx
                                  add rsp, 8 ; delete env from stack
                                  pop rbx ; keep arg_count in rbx
                                  inc rbx ; add 1 for the magic to clean
                                  shl rbx, 3
                                  add rsp, rbx ; delete args and magic 

                                          ;                   pop rbx ; restore rbx value
;finishing working on Applic
push rax
mov rbx, 1
push rbx
;working on VarFree
mov rax, qword [fvar_tbl+264]
;finishing working on VarFree
cmp qword[rax + 0 * WORD_SIZE], T_CLOSURE
                                  CLOSURE_ENV rbx, rax
                                  push rbx
                                  CLOSURE_CODE rbx, rax
                                  call rbx
                                  add rsp, 8 ; delete env from stack
                                  pop rbx ; keep arg_count in rbx
                                  inc rbx ; add 1 for the magic to clean
                                  shl rbx, 3
                                  add rsp, rbx ; delete args and magic 

                                          ;                   pop rbx ; restore rbx value
;finishing working on Applic
;working on if
cmp rax, SOB_FALSE_ADDRESS
je Lelse4
;working on Applic
push SOB_NIL_ADDRESS 
;working on Applic
push SOB_NIL_ADDRESS 
;working on BoxGet
;working on Var param
mov rax, qword [rbp + WORD_SIZE * (4 + 0)] 
;finishing working on Var param
mov rax, qword [rax]
;finishing working on BoxGet
push rax
mov rbx, 1
push rbx
;working on VarFree
mov rax, qword [fvar_tbl+256]
;finishing working on VarFree
cmp qword[rax + 0 * WORD_SIZE], T_CLOSURE
                                  CLOSURE_ENV rbx, rax
                                  push rbx
                                  CLOSURE_CODE rbx, rax
                                  call rbx
                                  add rsp, 8 ; delete env from stack
                                  pop rbx ; keep arg_count in rbx
                                  inc rbx ; add 1 for the magic to clean
                                  shl rbx, 3
                                  add rsp, rbx ; delete args and magic 

                                          ;                   pop rbx ; restore rbx value
;finishing working on Applic
push rax
mov rbx, 1
push rbx
;working on VarFree
mov rax, qword [fvar_tbl+272]
;finishing working on VarFree
cmp qword[rax + 0 * WORD_SIZE], T_CLOSURE
                                  CLOSURE_ENV rbx, rax
                                  push rbx
                                  CLOSURE_CODE rbx, rax
                                  call rbx
                                  add rsp, 8 ; delete env from stack
                                  pop rbx ; keep arg_count in rbx
                                  inc rbx ; add 1 for the magic to clean
                                  shl rbx, 3
                                  add rsp, rbx ; delete args and magic 

                                          ;                   pop rbx ; restore rbx value
;finishing working on Applic
jmp Lexit4
Lelse4:
;working on const
mov rax, const_tbl+2 
;finishing working on const
Lexit4:
;finishing working on if
;finishing working on seq

leave
    ret
    Lcont3:
;finishing working on LambdaSimple
push rax
;working on Var param
mov rax, qword [rbp + WORD_SIZE * (4 + 1)] 
;finishing working on Var param
pop qword [rax]
mov rax, SOB_VOID_ADDRESS
;finishing working on BoxSet

;working on Applic
push SOB_NIL_ADDRESS 

mov rbx, 0
push rbx
;working on LambdaSimple
push rbx
  push rcx
  push rdx
  push rsi
  mov rbx, 1
  cmp rbx,0
  jne not_empty5
  mov rbx, SOB_NIL_ADDRESS
  MAKE_CLOSURE(rax, rbx, Lcode5)
  jmp after_closure5
  not_empty5:
  MALLOC rax, WORD_SIZE*1; allocate new enviorment 
mov rbx,[rbp +2 *WORD_SIZE]
  mov rcx,0 
  env_copy5:
  cmp rcx, 1
je finish_env_copy5
mov rdx, [rbx + rcx * WORD_SIZE]
  inc rcx
  mov [rax + rcx * WORD_SIZE], rdx
  jmp env_copy5
finish_env_copy5:
  mov rbx, [rbp + 3 * WORD_SIZE]
  cmp rbx,0
  jne allocate_args5
mov rdx, SOB_NIL_ADDRESS
  jmp finish_copy_args5
allocate_args5:
  shl rbx,3
  MALLOC rdx, rbx
  shr rbx,3
  mov rcx,0
  copy_args5:
  cmp rcx,rbx
  je finish_copy_args5

  mov rsi, PVAR(rcx)
  mov [rdx + rcx *WORD_SIZE ],rsi
  inc rcx
  jmp copy_args5
finish_copy_args5:
  mov [rax + 0 * WORD_SIZE] , rdx ;place at envorment 0
  mov rbx,rax
  MAKE_CLOSURE(rax, rbx, Lcode5)

  after_closure5:
  pop rsi
  pop rdx
  pop rcx
  pop rbx
  jmp Lcont5
Lcode5:
    push rbp
    mov rbp,rsp 
;working on Applic
push SOB_NIL_ADDRESS 
;working on const
mov rax, const_tbl+49 
;finishing working on const
push rax
mov rbx, 1
push rbx
;working on BoxGet
;working on VarBound
mov rax, qword [rbp + WORD_SIZE * 2]
mov rax, qword [rax + WORD_SIZE * 0]
mov rax, qword [rax + WORD_SIZE * 1]
;finishing working on VarBound
mov rax, qword [rax]
;finishing working on BoxGet
cmp qword[rax + 0 * WORD_SIZE], T_CLOSURE
                                  CLOSURE_ENV rbx, rax
                                  push rbx
                                  CLOSURE_CODE rbx, rax
                                  call rbx
                                  add rsp, 8 ; delete env from stack
                                  pop rbx ; keep arg_count in rbx
                                  inc rbx ; add 1 for the magic to clean
                                  shl rbx, 3
                                  add rsp, rbx ; delete args and magic 

                                          ;                   pop rbx ; restore rbx value
;finishing working on Applic

leave
    ret
    Lcont5:
;finishing working on LambdaSimple
cmp qword[rax + 0 * WORD_SIZE], T_CLOSURE
                                  CLOSURE_ENV rbx, rax
                                  push rbx
                                  CLOSURE_CODE rbx, rax
                                  call rbx
                                  add rsp, 8 ; delete env from stack
                                  pop rbx ; keep arg_count in rbx
                                  inc rbx ; add 1 for the magic to clean
                                  shl rbx, 3
                                  add rsp, rbx ; delete args and magic 

                                          ;                   pop rbx ; restore rbx value
;finishing working on Applic
;finishing working on seq

leave
    ret
    Lcont0:
;finishing working on LambdaSimple
cmp qword[rax + 0 * WORD_SIZE], T_CLOSURE
                                  CLOSURE_ENV rbx, rax
                                  push rbx
                                  CLOSURE_CODE rbx, rax
                                  call rbx
                                  add rsp, 8 ; delete env from stack
                                  pop rbx ; keep arg_count in rbx
                                  inc rbx ; add 1 for the magic to clean
                                  shl rbx, 3
                                  add rsp, rbx ; delete args and magic 

                                          ;                   pop rbx ; restore rbx value
;finishing working on Applic

	call write_sob_if_not_void


   ;;; Clean up the dummy frame, set the exit status to 0 ("success"), 
   ;;; and return from main
   pop rbp
   add rsp, 4*8
   mov rax, 0

   ret
boolean?:
       push rbp
       mov rbp, rsp 
       mov rsi, PVAR(0)
	mov sil, byte [rsi]
	cmp sil, T_BOOL
      je .true
       mov rax, SOB_FALSE_ADDRESS
       jmp .return
       .true:
       mov rax, SOB_TRUE_ADDRESS
       .return:
         pop rbp
         ret

flonum?:
       push rbp
       mov rbp, rsp 
       mov rsi, PVAR(0)
	mov sil, byte [rsi]
	cmp sil, T_FLOAT
      je .true
       mov rax, SOB_FALSE_ADDRESS
       jmp .return
       .true:
       mov rax, SOB_TRUE_ADDRESS
       .return:
         pop rbp
         ret

rational?:
       push rbp
       mov rbp, rsp 
       mov rsi, PVAR(0)
	mov sil, byte [rsi]
	cmp sil, T_RATIONAL
      je .true
       mov rax, SOB_FALSE_ADDRESS
       jmp .return
       .true:
       mov rax, SOB_TRUE_ADDRESS
       .return:
         pop rbp
         ret

pair?:
       push rbp
       mov rbp, rsp 
       mov rsi, PVAR(0)
	mov sil, byte [rsi]
	cmp sil, T_PAIR
      je .true
       mov rax, SOB_FALSE_ADDRESS
       jmp .return
       .true:
       mov rax, SOB_TRUE_ADDRESS
       .return:
         pop rbp
         ret

null?:
       push rbp
       mov rbp, rsp 
       mov rsi, PVAR(0)
	mov sil, byte [rsi]
	cmp sil, T_NIL
      je .true
       mov rax, SOB_FALSE_ADDRESS
       jmp .return
       .true:
       mov rax, SOB_TRUE_ADDRESS
       .return:
         pop rbp
         ret

char?:
       push rbp
       mov rbp, rsp 
       mov rsi, PVAR(0)
	mov sil, byte [rsi]
	cmp sil, T_CHAR
      je .true
       mov rax, SOB_FALSE_ADDRESS
       jmp .return
       .true:
       mov rax, SOB_TRUE_ADDRESS
       .return:
         pop rbp
         ret

string?:
       push rbp
       mov rbp, rsp 
       mov rsi, PVAR(0)
	mov sil, byte [rsi]
	cmp sil, T_STRING
      je .true
       mov rax, SOB_FALSE_ADDRESS
       jmp .return
       .true:
       mov rax, SOB_TRUE_ADDRESS
       .return:
         pop rbp
         ret

symbol?:
       push rbp
       mov rbp, rsp 
       mov rsi, PVAR(0)
	mov sil, byte [rsi]
	cmp sil, T_SYMBOL
      je .true
       mov rax, SOB_FALSE_ADDRESS
       jmp .return
       .true:
       mov rax, SOB_TRUE_ADDRESS
       .return:
         pop rbp
         ret

procedure?:
       push rbp
       mov rbp, rsp 
       mov rsi, PVAR(0)
	mov sil, byte [rsi]
	cmp sil, T_CLOSURE
      je .true
       mov rax, SOB_FALSE_ADDRESS
       jmp .return
       .true:
       mov rax, SOB_TRUE_ADDRESS
       .return:
         pop rbp
         ret

div:
       push rbp
       mov rbp, rsp 
       mov rsi, PVAR(0)
	mov rdi, PVAR(1)
	mov dl, byte [rsi]
             cmp dl, T_FLOAT
	     jne .div_rat
             FLOAT_VAL rsi, rsi 
          movq xmm0, rsi
          FLOAT_VAL rdi, rdi 
          movq xmm1, rdi
	  divsd xmm0, xmm1
          movq rsi, xmm0
          MAKE_FLOAT(rax, rsi)
             jmp .op_return
          .div_rat:
             DENOMINATOR rcx, rsi
	  DENOMINATOR rdx, rdi
	  NUMERATOR rsi, rsi
	  NUMERATOR rdi, rdi
          MAKE_RATIONAL(rax, rdx, rdi)
         mov PVAR(1), rax
         pop rbp
         jmp mul
          MAKE_RATIONAL(rax, rsi, rcx)
          .op_return:
         pop rbp
         ret

mul:
       push rbp
       mov rbp, rsp 
       mov rsi, PVAR(0)
	mov rdi, PVAR(1)
	mov dl, byte [rsi]
             cmp dl, T_FLOAT
	     jne .mul_rat
             FLOAT_VAL rsi, rsi 
          movq xmm0, rsi
          FLOAT_VAL rdi, rdi 
          movq xmm1, rdi
	  mulsd xmm0, xmm1
          movq rsi, xmm0
          MAKE_FLOAT(rax, rsi)
             jmp .op_return
          .mul_rat:
             DENOMINATOR rcx, rsi
	  DENOMINATOR rdx, rdi
	  NUMERATOR rsi, rsi
	  NUMERATOR rdi, rdi
          imul rsi, rdi
	 imul rcx, rdx
          MAKE_RATIONAL(rax, rsi, rcx)
          .op_return:
         pop rbp
         ret

add:
       push rbp
       mov rbp, rsp 
       mov rsi, PVAR(0)
	mov rdi, PVAR(1)
	mov dl, byte [rsi]
             cmp dl, T_FLOAT
	     jne .add_rat
             FLOAT_VAL rsi, rsi 
          movq xmm0, rsi
          FLOAT_VAL rdi, rdi 
          movq xmm1, rdi
	  addsd xmm0, xmm1
          movq rsi, xmm0
          MAKE_FLOAT(rax, rsi)
             jmp .op_return
          .add_rat:
             DENOMINATOR rcx, rsi
	  DENOMINATOR rdx, rdi
	  NUMERATOR rsi, rsi
	  NUMERATOR rdi, rdi
          imul rsi, rdx
	 imul rdi, rcx
	 add rsi, rdi
	 imul rcx, rdx
          MAKE_RATIONAL(rax, rsi, rcx)
          .op_return:
         pop rbp
         ret

eq:
       push rbp
       mov rbp, rsp 
       mov rsi, PVAR(0)
	mov rdi, PVAR(1)
	mov dl, byte [rsi]
             cmp dl, T_FLOAT
	     jne .eq_rat
             FLOAT_VAL rsi, rsi
	 FLOAT_VAL rdi, rdi
	 cmp rsi, rdi
             jmp .op_return
          .eq_rat:
             NUMERATOR rcx, rsi
	 NUMERATOR rdx, rdi
	 cmp rcx, rdx
	 jne .false
	 DENOMINATOR rcx, rsi
	 DENOMINATOR rdx, rdi
	 cmp rcx, rdx
         .false:
          .op_return:
      je .true
       mov rax, SOB_FALSE_ADDRESS
       jmp .return
       .true:
       mov rax, SOB_TRUE_ADDRESS
       .return:
         pop rbp
         ret

lt:
       push rbp
       mov rbp, rsp 
       mov rsi, PVAR(0)
	mov rdi, PVAR(1)
	mov dl, byte [rsi]
             cmp dl, T_FLOAT
	     jne .lt_rat
             FLOAT_VAL rsi, rsi
	 movq xmm0, rsi
	 FLOAT_VAL rdi, rdi
	 movq xmm1, rdi
	 ucomisd xmm0, xmm1
             jmp .op_return
          .lt_rat:
             DENOMINATOR rcx, rsi
	 DENOMINATOR rdx, rdi
	 NUMERATOR rsi, rsi
	 NUMERATOR rdi, rdi
	 imul rsi, rdx
	 imul rdi, rcx
	 cmp rsi, rdi
          .op_return:
      jl .true
       mov rax, SOB_FALSE_ADDRESS
       jmp .return
       .true:
       mov rax, SOB_TRUE_ADDRESS
       .return:
         pop rbp
         ret

string_length:
       push rbp
       mov rbp, rsp 
       mov rsi, PVAR(0)
	STRING_LENGTH rsi, rsi
         MAKE_RATIONAL(rax, rsi, 1)
         pop rbp
         ret

string_ref:
       push rbp
       mov rbp, rsp 
       mov rsi, PVAR(0)
	mov rdi, PVAR(1)
	STRING_ELEMENTS rsi, rsi
         NUMERATOR rdi, rdi
         add rsi, rdi
         mov sil, byte [rsi]
         MAKE_CHAR(rax, sil)
         pop rbp
         ret

string_set:
       push rbp
       mov rbp, rsp 
       mov rsi, PVAR(0)
	mov rdi, PVAR(1)
	mov rdx, PVAR(2)
	STRING_ELEMENTS rsi, rsi
         NUMERATOR rdi, rdi
         add rsi, rdi
         CHAR_VAL rax, rdx
         mov byte [rsi], al
         mov rax, SOB_VOID_ADDRESS
         pop rbp
         ret

make_string:
       push rbp
       mov rbp, rsp 
       mov rsi, PVAR(0)
	mov rdi, PVAR(1)
	NUMERATOR rsi, rsi
         CHAR_VAL rdi, rdi
         and rdi, 255
         MAKE_STRING rax, rsi, dil
         pop rbp
         ret

car:
       push rbp
       mov rbp, rsp 
       mov rsi, PVAR(0)
	mov qword rax, [rsi+TYPE_SIZE]
         pop rbp
         ret

cdr:
       push rbp
       mov rbp, rsp 
       mov rsi, PVAR(0)
	mov qword rax, [rsi+TYPE_SIZE+WORD_SIZE]
         pop rbp
         ret

set_car:
       push rbp
       mov rbp, rsp 
       mov rsi, PVAR(0)
	mov rdi, PVAR(1)
	mov qword [rsi+TYPE_SIZE], rdi
         mov rax, SOB_VOID_ADDRESS
         pop rbp
         ret

set_cdr:
       push rbp
       mov rbp, rsp 
       mov rsi, PVAR(0)
	mov rdi, PVAR(1)
	mov qword[rsi+TYPE_SIZE+WORD_SIZE], rdi
         mov rax, SOB_VOID_ADDRESS
         pop rbp
         ret

cons:
       push rbp
       mov rbp, rsp 
       mov rsi, PVAR(0)
	mov rdi, PVAR(1)
	MAKE_PAIR(rax,rsi,rdi)
         pop rbp
         ret

symbol_to_string:
       push rbp
       mov rbp, rsp 
       mov rsi, PVAR(0)
	SYMBOL_VAL rsi, rsi
	       STRING_LENGTH rcx, rsi
	       STRING_ELEMENTS rdi, rsi
	       push rcx
	       push rdi
	       mov dil, byte [rdi]
	       MAKE_CHAR(rax, dil)
	       push rax
	       MAKE_RATIONAL(rax, rcx, 1)
	       push rax
	       push 2
	       push SOB_NIL_ADDRESS
	       call make_string
	       add rsp, 4*8
	       STRING_ELEMENTS rsi, rax   
	       pop rdi
	       pop rcx
	       cmp rcx, 0
	       je .end
        .loop:
	       lea r8, [rdi+rcx]
	       lea r9, [rsi+rcx]
	       mov bl, byte [r8]
	       mov byte [r9], bl
	       loop .loop
               .end:
         pop rbp
         ret

eq?:
       push rbp
       mov rbp, rsp 
       mov rsi, PVAR(0)
	mov rdi, PVAR(1)
	cmp rsi, rdi
      je .true
       mov rax, SOB_FALSE_ADDRESS
       jmp .return
       .true:
       mov rax, SOB_TRUE_ADDRESS
       .return:
         pop rbp
         ret

char_to_integer:
       push rbp
       mov rbp, rsp 
       mov rsi, PVAR(0)
	CHAR_VAL rsi, rsi
	 and rsi, 255
	 MAKE_RATIONAL(rax, rsi, 1)
         pop rbp
         ret

integer_to_char:
       push rbp
       mov rbp, rsp 
       mov rsi, PVAR(0)
	NUMERATOR rsi, rsi
	 and rsi, 255
	 MAKE_CHAR(rax, sil)
         pop rbp
         ret

exact_to_inexact:
       push rbp
       mov rbp, rsp 
       mov rsi, PVAR(0)
	DENOMINATOR rdi, rsi
	 NUMERATOR rsi, rsi 
	 cvtsi2sd xmm0, rsi
	 cvtsi2sd xmm1, rdi
	 divsd xmm0, xmm1
	 movq rsi, xmm0
	 MAKE_FLOAT(rax, rsi)
         pop rbp
         ret

numerator:
       push rbp
       mov rbp, rsp 
       mov rsi, PVAR(0)
	NUMERATOR rsi, rsi
	 mov rdi, 1
	 MAKE_RATIONAL(rax, rsi, rdi)
         pop rbp
         ret

denominator:
       push rbp
       mov rbp, rsp 
       mov rsi, PVAR(0)
	DENOMINATOR rsi, rsi
	 mov rdi, 1
	 MAKE_RATIONAL(rax, rsi, rdi)
         pop rbp
         ret

gcd:
       push rbp
       mov rbp, rsp 
       mov rsi, PVAR(0)
	mov rdi, PVAR(1)
	xor rdx, rdx
	 NUMERATOR rax, rsi
         NUMERATOR rdi, rdi
       .loop:
	 and rdi, rdi
	 jz .end_loop
	 xor rdx, rdx 
	 div rdi
	 mov rax, rdi
	 mov rdi, rdx
	 jmp .loop	
       .end_loop:
	 mov rdx, rax
         MAKE_RATIONAL(rax, rdx, 1)
         pop rbp
         ret