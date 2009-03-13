"-----------------------------------------------------------------------------
" clipper
" Author: ky
" Version: 0.1.5
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

if &compatible || v:version < 700 || exists('g:loaded_clipper')
  finish
endif

let s:cpoptions = &cpoptions
set cpoptions&vim


let g:loaded_clipper = 1

if !exists('g:clipper_no_default_key_mappings')
  let g:clipper_no_default_key_mappings = 0
endif


nnoremap <silent> <Plug>(clipper_y)
      \ :<C-u>call clipper#execute('n', 'y')<CR>
onoremap <expr> <silent> <Plug>(clipper_y)
      \ clipper#execute('o', 'y')
nnoremap <silent> <Plug>(clipper_d)
      \ :<C-u>call clipper#execute('n', 'd')<CR>
onoremap <expr> <silent> <Plug>(clipper_d)
      \ clipper#execute('o', 'd')
nnoremap <silent> <Plug>(clipper_c)
      \ :<C-u>call clipper#define_operator_autocmd()<CR>
      \:call clipper#execute('n', 'c')<CR>


snoremap <silent> <Plug>(clipper_<BS>)
      \ <C-g>:<C-u>call clipper#execute('s', '<lt>BS>')<CR>
xnoremap <silent> <Plug>(clipper_d)
      \ :<C-u>call clipper#execute('x', 'd')<CR>
nnoremap <silent> <Plug>(clipper_D)
      \ :<C-u>call clipper#execute('n', 'D')<CR>
xnoremap <silent> <Plug>(clipper_D)
      \ :<C-u>call clipper#execute('x', 'D')<CR>
nnoremap <silent> <Plug>(clipper_<Del>)
      \ :<C-u>call clipper#execute('n', '<lt>Del>')<CR>
snoremap <silent> <Plug>(clipper_<Del>)
      \ <C-g>:<C-u>call clipper#execute('s', '<lt>Del>')<CR>
xnoremap <silent> <Plug>(clipper_<Del>)
      \ :<C-u>call clipper#execute('x', '<lt>Del>')<CR>
snoremap <silent> <Plug>(clipper_<C-h>)
      \ <C-g>:<C-u>call clipper#execute('s', '<lt>C-h>')<CR>
nnoremap <silent> <Plug>(clipper_x)
      \ :<C-u>call clipper#execute('n', 'x')<CR>
xnoremap <silent> <Plug>(clipper_x)
      \ :<C-u>call clipper#execute('x', 'x')<CR>
nnoremap <silent> <Plug>(clipper_X)
      \ :<C-u>call clipper#execute('n', 'X')<CR>
xnoremap <silent> <Plug>(clipper_X)
      \ :<C-u>call clipper#execute('x', 'X')<CR>
xnoremap <silent> <Plug>(clipper_y)
      \ :<C-u>call clipper#execute('x', 'y')<CR>
nnoremap <silent> <Plug>(clipper_Y)
      \ :<C-u>call clipper#execute('n', 'Y')<CR>
xnoremap <silent> <Plug>(clipper_Y)
      \ :<C-u>call clipper#execute('x', 'Y')<CR>


xnoremap <silent> <Plug>(clipper_c)
      \ :<C-u>call clipper#execute('x', 'c')<CR>
nnoremap <silent> <Plug>(clipper_C)
      \ :<C-u>call clipper#execute('n', 'C')<CR>
xnoremap <silent> <Plug>(clipper_C)
      \ :<C-u>call clipper#execute('x', 'C')<CR>
snoremap <silent> <Plug>(clipper_<CR>)
      \ <C-g>:<C-u>call clipper#execute('s', '<lt>CR>')<CR>
snoremap <silent> <Plug>(clipper_<NL>)
      \ <C-g>:<C-u>call clipper#execute('s', '<lt>NL>')<CR>
nnoremap <silent> <Plug>(clipper_s)
      \ :<C-u>call clipper#execute('n', 's')<CR>
xnoremap <silent> <Plug>(clipper_s)
      \ :<C-u>call clipper#execute('x', 's')<CR>
nnoremap <silent> <Plug>(clipper_S)
      \ :<C-u>call clipper#execute('n', 'S')<CR>
xnoremap <silent> <Plug>(clipper_S)
      \ :<C-u>call clipper#execute('x', 'S')<CR>


nnoremap <expr> <silent> <Plug>(clipper_p)
      \ clipper#execute('n', 'p')
xnoremap <silent> <Plug>(clipper_p)
      \ :<C-u>call clipper#execute('x', 'p')<CR>
nnoremap <expr> <silent> <Plug>(clipper_P)
      \ clipper#execute('n', 'P')
xnoremap <silent> <Plug>(clipper_P)
      \ :<C-u>call clipper#execute('x', 'P')<CR>
nnoremap <expr> <silent> <Plug>(clipper_gp)
      \ clipper#execute('n', 'gp')
nnoremap <expr> <silent> <Plug>(clipper_gP)
      \ clipper#execute('n', 'gP')
nnoremap <expr> <silent> <Plug>(clipper_[p)
      \ clipper#execute('n', '[p')
nnoremap <expr> <silent> <Plug>(clipper_]p)
      \ clipper#execute('n', ']p')
nnoremap <expr> <silent> <Plug>(clipper_[P)
      \ clipper#execute('n', '[P')
nnoremap <expr> <silent> <Plug>(clipper_]P)
      \ clipper#execute('n', ']P')


" queue operation
nnoremap <silent> <Plug>(clipper_select)
      \ :<C-u>call clipper#select()<CR>
nnoremap <silent> <Plug>(clipper_select_end)
      \ :<C-u>call clipper#select_end()<CR>


function! s:default_key_mappings()
  nmap y <Plug>(clipper_y)
  omap y <Plug>(clipper_y)
  nmap d <Plug>(clipper_d)
  omap d <Plug>(clipper_d)
  nmap c <Plug>(clipper_c)

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

  nmap <C-p> <Plug>(clipper_select)
endfunction


command! -bar -nargs=0 ClipperDump call clipper#dump()
command! -bar -nargs=0 ClipperDefaultKeyMappings call s:default_key_mappings()


if !g:clipper_no_default_key_mappings
  ClipperDefaultKeyMappings
endif


let &cpoptions = s:cpoptions
unlet s:cpoptions


" vim: expandtab shiftwidth=2 softtabstop=2 foldmethod=marker
