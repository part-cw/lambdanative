#|
LambdaNative - a cross-platform Scheme framework
Copyright (c) 2009-2015, University of British Columbia
All rights reserved.

Redistribution and use in source and binary forms, with or
without modification, are permitted provided that the
following conditions are met:

* Redistributions of source code must retain the above
copyright notice, this list of conditions and the following
disclaimer.

* Redistributions in binary form must reproduce the above
copyright notice, this list of conditions and the following
disclaimer in the documentation and/or other materials
provided with the distribution.

* Neither the name of the University of British Columbia nor
the names of its contributors may be used to endorse or
promote products derived from this software without specific
prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR
CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
|#

;; bindings to lua

(define lua:debuglevel 0)
(define (lua:log level . x)
   (if (>= lua:debuglevel level) (apply log-system (append (list "lua: ") x))))

(c-declare  #<<end-of-c-declare

#include <stdio.h>
#include <string.h>
#include <lua.h>
#include <lauxlib.h>
#include <lualib.h> 

end-of-c-declare
)

(define-macro (lua-constant k)
  `(define ,k ((c-lambda () int
     ,(string-append "___result = " (symbol->string k) ";")))))

(lua-constant LUA_ERRERR)
(lua-constant LUA_ERRFILE)
(lua-constant LUA_ERRGCMM)
(lua-constant LUA_ERRMEM)
(lua-constant LUA_ERRRUN)
(lua-constant LUA_ERRSYNTAX)
(lua-constant LUA_HOOKCALL)
(lua-constant LUA_HOOKCOUNT)
(lua-constant LUA_HOOKLINE)
(lua-constant LUA_HOOKRET)
(lua-constant LUA_HOOKTAILCALL)
(lua-constant LUA_MASKCALL)
(lua-constant LUA_MASKCOUNT)
(lua-constant LUA_MASKLINE)
(lua-constant LUA_MASKRET)
(lua-constant LUA_MINSTACK)
(lua-constant LUA_MULTRET)
(lua-constant LUA_NOREF)
(lua-constant LUA_OK)
(lua-constant LUA_OPADD)
(lua-constant LUA_OPDIV)
(lua-constant LUA_OPEQ)
(lua-constant LUA_OPLE)
(lua-constant LUA_OPLT)
(lua-constant LUA_OPMOD)
(lua-constant LUA_OPMUL)
(lua-constant LUA_OPPOW)
(lua-constant LUA_OPSUB)
(lua-constant LUA_OPUNM)
(lua-constant LUA_REFNIL)
(lua-constant LUA_REGISTRYINDEX)
(lua-constant LUA_RIDX_GLOBALS)
(lua-constant LUA_RIDX_MAINTHREAD)
(lua-constant LUA_TBOOLEAN)
(lua-constant LUA_TFUNCTION)
(lua-constant LUA_TLIGHTUSERDATA)
(lua-constant LUA_TNIL)
(lua-constant LUA_TNONE)
(lua-constant LUA_TNUMBER)
(lua-constant LUA_TSTRING)
(lua-constant LUA_TTABLE)
(lua-constant LUA_TTHREAD)
(lua-constant LUA_TUSERDATA)
;;(lua-constant LUA_USE_APICHECK)
(lua-constant LUA_YIELD)
(lua-constant LUAL_BUFFERSIZE)

(lua-constant LUA_GCSTOP)
(lua-constant LUA_GCRESTART)
(lua-constant LUA_GCCOLLECT)
(lua-constant LUA_GCCOUNT)
(lua-constant LUA_GCCOUNTB)
(lua-constant LUA_GCSTEP)
(lua-constant LUA_GCSETPAUSE)
(lua-constant LUA_GCSETSTEPMUL)
(lua-constant LUA_GCISRUNNING)
;;(lua-constant LUA_GCGEN)
;;(lua-constant LUA_GCINC)

(define-macro (lua-function f args ret)
  `(define ,f (c-lambda ,args ,ret ,(symbol->string f))))

(c-define-type *L (pointer void))

(lua-function lua_absindex (*L int) int)
(lua-function lua_arith (*L int) void)
;; lua_atpanic 
(lua-function lua_call (*L int int) void)
;; lua_callk 
(lua-function lua_checkstack (*L int) int)
(lua-function lua_close (*L) void)
(lua-function lua_compare (*L int int int) int)
(lua-function lua_concat (*L int) void)
(lua-function lua_copy (*L int int) void)
(lua-function lua_createtable (*L int int) void)
;; lua_dump 
(lua-function lua_error (*L) int)
(lua-function lua_gc (*L int int) int)
;; lua_getallocf
;; lua_getctx
(lua-function lua_getfield (*L int char-string) void)
(lua-function lua_getglobal (*L char-string) void)
;; lua_gethook
;; lua_gethookcount
;; lua_gethookmask
;; lua_getinfo
;; lua_getlocal
(lua-function lua_getmetatable (*L int) int)
;; lua_getstack
(lua-function lua_gettable (*L int) void)
(lua-function lua_gettop (*L) int)
;; lua_getupvalue
(lua-function lua_getuservalue (*L int) void)
(lua-function lua_insert (*L int) void)
(lua-function lua_isboolean (*L int) int)
(lua-function lua_iscfunction (*L int) int)
(lua-function lua_isfunction (*L int) int)
(lua-function lua_islightuserdata (*L int) int)
(lua-function lua_isnil (*L int) int)
(lua-function lua_isnone (*L int) int)
(lua-function lua_isnoneornil (*L int) int)
(lua-function lua_isnumber (*L int) int)
(lua-function lua_isstring (*L int) int)
(lua-function lua_istable (*L int) int)
(lua-function lua_isthread (*L int) int)
(lua-function lua_isuserdata (*L int) int)
(lua-function lua_len (*L int) void)
;; lua_load 
;; lua_newstate
(lua-function lua_newtable (*L) void)
(lua-function lua_newthread (*L) *L)
(lua-function lua_newuserdata (*L int) (pointer void))
(lua-function lua_next (*L int) int)
(lua-function lua_pcall (*L int int int) int)
;; lua_pcallk
(lua-function lua_pop (*L int) void)
(lua-function lua_pushboolean (*L int) void)
;; lua_pushcclosure
;; lua_pushcfunction
;; lua_pushfstring
(lua-function lua_pushglobaltable (*L) void)
(lua-function lua_pushinteger (*L int) void)
(lua-function lua_pushlightuserdata (*L (pointer void)) void)
;; lua_pushliteral
(lua-function lua_pushlstring (*L char-string int) char-string)
(lua-function lua_pushnil (*L) void)
(lua-function lua_pushnumber (*L double) void)
(lua-function lua_pushstring (*L char-string) char-string)
(lua-function lua_pushthread (*L) int)
;; lua_pushunsigned
(lua-function lua_pushvalue (*L int) void)
;; lua_pushvfstring 
(lua-function lua_rawequal (*L int int) int)
(lua-function lua_rawget (*L int) void)
(lua-function lua_rawgeti (*L int int) void)
(lua-function lua_rawgetp (*L int (pointer void)) void)
(lua-function lua_rawlen (*L int) unsigned-int)
(lua-function lua_rawset (*L int) void)
(lua-function lua_rawseti (*L int int) void)
(lua-function lua_rawsetp (*L int (pointer void)) void)

;;lua_register
(lua-function lua_remove (*L int) void)
(lua-function lua_replace (*L int) void)
(lua-function lua_resume (*L *L int) int)
;; lua_setallocf
(lua-function lua_setfield (*L int char-string) void)
(lua-function lua_setglobal (*L char-string) void)
;; lua_sethook
;; lua_setlocal
(lua-function lua_setmetatable (*L int) void)
(lua-function lua_settable (*L int) void)
(lua-function lua_settop (*L int) void)
;; lua_setupvalue 
(lua-function lua_setuservalue (*L int) void)
(lua-function lua_status (*L) int)
(lua-function lua_toboolean (*L int) int)
;; lua_tocfunction
(lua-function lua_tointeger (*L int) int)
;; lua_tointegerx 
;;  lua_tolstring 
(lua-function lua_tonumber (*L int) double)
;; lua_tonumberx 
(lua-function lua_topointer (*L int) (pointer void))
(lua-function lua_tostring (*L int) char-string)
(lua-function lua_tothread (*L int) *L)
;; lua_tounsigned 
;; lua_tounsignedx
(lua-function lua_touserdata (*L int) (pointer void))
(lua-function lua_type (*L int) int)
(lua-function lua_typename (*L int) char-string)
;; lua_upvalueid
(lua-function lua_upvalueindex (int) int)
;; lua_upvaluejoin
;; lua_version
(lua-function lua_xmove (*L *L int) void)
(lua-function lua_yield (*L int) int)
;; lua_yieldk

;; luaL_addchar
;; luaL_addlstring
;; luaL_addsize
;; luaL_addstring
;; luaL_addvalue
(lua-function luaL_argcheck (*L int int char-string) void)
(lua-function luaL_argerror (*L int char-string) int)
;; luaL_buffinit
;; luaL_buffinitsize
(lua-function luaL_callmeta (*L int char-string) int)
;; luaL_checkany
;; luaL_checkint
;; luaL_checkinteger
;; luaL_checklong 
;; luaL_checklstring 
;; luaL_checknumber
;; luaL_checkoption
(lua-function luaL_checkstack (*L int char-string) void)
(lua-function luaL_checkstring (*L int) char-string)
(lua-function luaL_checktype (*L int int) void)
(lua-function luaL_checkudata (*L int char-string) (pointer void))
;; luaL_checkunsigned
(lua-function luaL_checkversion (*L) void)
(lua-function luaL_dofile (*L char-string) int)
(lua-function luaL_dostring (*L char-string) int)
;; luaL_error 
(lua-function luaL_execresult (*L int) int)
(lua-function luaL_fileresult (*L int char-string) int)
(lua-function luaL_getmetafield (*L int char-string) int)
(lua-function luaL_getmetatable (*L char-string) void)
(lua-function luaL_getsubtable (*L int char-string) int)
(lua-function luaL_gsub (*L char-string char-string char-string) char-string)
(lua-function luaL_len (*L int) int)
(lua-function luaL_loadbuffer (*L char-string int char-string) int)
(lua-function luaL_loadbufferx (*L char-string int char-string char-string) int)
(lua-function luaL_loadfile (*L char-string) int)
(lua-function luaL_loadfilex (*L char-string char-string) int)
(lua-function luaL_loadstring (*L char-string) int)
;; luaL_newlib
;; luaL_newlibtable
(lua-function luaL_newmetatable (*L char-string) int)
(lua-function luaL_newstate () *L)
(lua-function luaL_openlibs (*L) void)
(lua-function luaL_ref (*L int) int)
;; luaL_optint 
;; luaL_optinteger 
;; luaL_optlong
;; luaL_optlstring
;; luaL_optnumber
;; luaL_optstring
;; luaL_optunsigned
;; luaL_prepbuffer
;; luaL_prepbuffsize
;; luaL_pushresult
;; luaL_pushresultsize
;; luaL_requiref
;; luaL_setfuncs
;; luaL_setmetatable
;; luaL_testudata
;; luaL_tolstring
;; luaL_traceback
;; luaL_typename
;; luaL_unref
;; luaL_where

(define lua_strlen (c-lambda ((pointer void) int) unsigned-int "___result=lua_rawlen(___arg1,(___arg2));"))

;; ------------

;; global lua state
(define LUA (luaL_newstate))
(luaL_openlibs LUA)

(define (string->lua str) 
  (lua:log 1 "string->lua " str)
  (luaL_loadbuffer LUA str (string-length str) "string->lua")
  (lua_call LUA 0 0))

(define (file->lua fname)
  (lua:log 1 "file->lua " fname)
  (luaL_loadfile LUA fname) 
  (lua_call LUA 0 0))

;; eof
