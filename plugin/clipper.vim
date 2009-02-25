"-----------------------------------------------------------------------------
" clipper
" Author: ky
" Version: 0.1
" License: The MIT License
" The MIT License {{{
"
" Copyright (C) 2009 ky
"
" Permission is hereby granted, free of charge, to any person obtaining a
" copy of this software and associated documentation files (the "Software"),
" to deal in the Software without restriction, including without limitation
" the rights to use, copy, modify, merge, publish, distribute, sublicense,
" and/or sell copies of the Software, and to permit persons to whom
" the Software is furnished to do so, subject to the following conditions:
" 
" The above copyright notice and this permission notice shall be included in
" all copies or substantial portions of the Software.
" 
" THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
" IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
" FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
" AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
" LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
" ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
" OTHER DEALINGS IN THE SOFTWARE.
" }}}
"-----------------------------------------------------------------------------

if &compatible || v:version < 700
  finish
endif
if (exists('g:loaded_clipper') && g:loaded_clipper)
  finish
endif

let s:cpoptions = &cpoptions
set cpoptions&vim

let g:loaded_clipper = 1




" operator
nnoremap <silent> <Plug>(clipper_y)
      \ :<C-u>call clipper#do_operator('y', 'clipper#operator_y')<CR>
onoremap <expr> <silent> <Plug>(clipper_y) clipper#linewise('y')
nnoremap <silent> <Plug>(clipper_d)
      \ :<C-u>call clipper#do_operator('d', 'clipper#operator_d')<CR>
onoremap <expr> <silent> <Plug>(clipper_d) clipper#linewise('d')
nnoremap <expr> <silent> <Plug>(clipper_c) clipper#pseudo_operator('c')


" yank or delete
snoremap <silent> <Plug>(clipper_<BS>)
      \ <C-g>:<C-u>call clipper#yank_s("\<lt>Del>")<CR>
xnoremap <silent> <Plug>(clipper_d)
      \ :<C-u>call clipper#yank_x('d')<CR>
nnoremap <silent> <Plug>(clipper_D)
      \ :<C-u>call clipper#yank_n('D')<CR>
xnoremap <silent> <Plug>(clipper_D)
      \ :<C-u>call clipper#yank_x('D')<CR>
nnoremap <silent> <Plug>(clipper_<Del>)
      \ :<C-u>call clipper#yank_n('x')<CR>
snoremap <silent> <Plug>(clipper_<Del>)
      \ <C-g>:<C-u>call clipper#yank_s("\<lt>Del>")<CR>
xnoremap <silent> <Plug>(clipper_<Del>)
      \ :<C-u>call clipper#yank_x("\<lt>Del>")<CR>
snoremap <silent> <Plug>(clipper_<C-h>)
      \ <C-g>:<C-u>call clipper#yank_s("\<lt>Del>")<CR>
nnoremap <silent> <Plug>(clipper_x)
      \ :<C-u>call clipper#yank_n('x')<CR>
xnoremap <silent> <Plug>(clipper_x)
      \ :<C-u>call clipper#yank_x('x')<CR>
nnoremap <silent> <Plug>(clipper_X)
      \ :<C-u>call clipper#yank_n('X')<CR>
xnoremap <silent> <Plug>(clipper_X)
      \ :<C-u>call clipper#yank_x('X')<CR>
xnoremap <silent> <Plug>(clipper_y)
      \ :<C-u>call clipper#yank_x('y')<CR>
nnoremap <silent> <Plug>(clipper_Y)
      \ :<C-u>call clipper#yank_n('Y')<CR>
xnoremap <silent> <Plug>(clipper_Y)
      \ :<C-u>call clipper#yank_x('Y')<CR>


" delete and insert
xnoremap <silent> <Plug>(clipper_c)
      \ :<C-u>call clipper#insert_x('c')<CR>
nnoremap <silent> <Plug>(clipper_C)
      \ :<C-u>call clipper#insert_n('C')<CR>
xnoremap <silent> <Plug>(clipper_C)
      \ :<C-u>call clipper#insert_x('C')<CR>
snoremap <silent> <Plug>(clipper_<CR>)
      \ <C-g>:<C-u>call clipper#insert_s("\<lt>CR>")<CR>
snoremap <silent> <Plug>(clipper_<NL>)
      \ <C-g>:<C-u>call clipper#insert_s("\<lt>NL>")<CR>
nnoremap <silent> <Plug>(clipper_s)
      \ :<C-u>call clipper#insert_n('s')<CR>
xnoremap <silent> <Plug>(clipper_s)
      \ :<C-u>call clipper#insert_x('s')<CR>
nnoremap <silent> <Plug>(clipper_S)
      \ :<C-u>call clipper#insert_n('S')<CR>
xnoremap <silent> <Plug>(clipper_S)
      \ :<C-u>call clipper#insert_x('S')<CR>


" paste
nnoremap <expr> <silent> <Plug>(clipper_p) clipper#paste_n('p')
xnoremap <silent> <Plug>(clipper_p) :<C-u>call clipper#paste_x('p')<CR>
nnoremap <expr> <silent> <Plug>(clipper_P) clipper#paste_n('P')
xnoremap <silent> <Plug>(clipper_P) :<C-u>call clipper#paste_x('P')<CR>
nnoremap <expr> <silent> <Plug>(clipper_gp) clipper#paste_n('gp')
nnoremap <expr> <silent> <Plug>(clipper_gP) clipper#paste_n('gP')
nnoremap <expr> <silent> <Plug>(clipper_[p) clipper#paste_n('[p')
nnoremap <expr> <silent> <Plug>(clipper_]p) clipper#paste_n(']p')
nnoremap <expr> <silent> <Plug>(clipper_[P) clipper#paste_n('[P')
nnoremap <expr> <silent> <Plug>(clipper_]P) clipper#paste_n(']P')


" stack operation
nnoremap <expr> <silent> <Plug>(clipper_stack_prev) clipper#stack_move(-1)
nnoremap <expr> <silent> <Plug>(clipper_stack_next) clipper#stack_move(1)


function! s:default_key_mappings()
  nmap y <Plug>(clipper_y)
  omap y <Plug>(clipper_y)
  nmap d <Plug>(clipper_d)
  omap d <Plug>(clipper_d)
  nmap c <Plug>(clipper_c)
  "omap c <Plug>(clipper_c)

  xmap d     <Plug>(clipper_d)
  nmap D     <Plug>(clipper_D)
  xmap D     <Plug>(clipper_D)
  smap <BS>  <Plug>(clipper_<BS>)
  nmap <Del> <Plug>(clipper_<Del>)
  smap <Del> <Plug>(clipper_<Del>)
  xmap <Del> <Plug>(clipper_<Del>)
  smap <C-h> <Plug>(clipper_<C-h>)
  nmap x     <Plug>(clipper_x)
  xmap x     <Plug>(clipper_x)
  nmap X     <Plug>(clipper_X)
  xmap X     <Plug>(clipper_X)
  xmap y     <Plug>(clipper_y)
  nmap Y     <Plug>(clipper_Y)
  xmap Y     <Plug>(clipper_Y)

  xmap c    <Plug>(clipper_c)
  nmap C    <Plug>(clipper_C)
  xmap C    <Plug>(clipper_C)
  smap <CR> <Plug>(clipper_<CR>)
  smap <NL> <Plug>(clipper_<NL>)
  nmap s    <Plug>(clipper_s)
  xmap s    <Plug>(clipper_s)
  nmap S    <Plug>(clipper_S)
  xmap S    <Plug>(clipper_S)

  nmap p  <Plug>(clipper_p)
  xmap p  <Plug>(clipper_p)
  nmap P  <Plug>(clipper_P)
  xmap P  <Plug>(clipper_P)
  nmap gp <Plug>(clipper_gp)
  nmap gP <Plug>(clipper_gP)
  nmap [p <Plug>(clipper_[p)
  nmap ]p <Plug>(clipper_]p)
  nmap [P <Plug>(clipper_[P)
  nmap ]P <Plug>(clipper_]P)

  nmap <C-p> <Plug>(clipper_stack_prev)
  nmap <C-n> <Plug>(clipper_stack_next)
endfunction


command! -bar -nargs=0 ClipperDump call clipper#dump()
command! -bar -nargs=0 ClipperDefaultKeyMappings call s:default_key_mappings()


if !exists('g:clipper_no_default_key_mappings') ||
      \ !g:clipper_no_default_key_mappings
  ClipperDefaultKeyMappings
endif


let &cpoptions = s:cpoptions
unlet s:cpoptions


" vim: expandtab shiftwidth=2 softtabstop=2 foldmethod=marker
