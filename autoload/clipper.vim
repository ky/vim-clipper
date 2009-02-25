"-----------------------------------------------------------------------------
" clipper
" Author: ky
" Version: 0.1
" License: The MIT License {{{
" The MIT License
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

function! clipper#dump()
  echo string(s:stack)
endfunction


function! clipper#yank_n(key)
  let s:used_register = s:register()
  let cnt = (v:count == v:count1 ? v:count : '')

  let reg0_save = @0
  execute 'normal! ' . cnt . s:used_register . a:key
  call clipper#auto_push()
  let @0 = reg0_save

  silent! call altrepeat#set_repeat_function(
        \ 'clipper#repeat_yank_n',
        \ 0,
        \ (cnt ==# '' ? 0 : cnt),
        \ [a:key]
        \)
endfunction


function! clipper#repeat_yank_n(count, key)
  if s:used_register ==# ''
    let s:used_register = s:register()
  endif
  execute 'normal! ' . (a:count ? a:count : '') . s:used_register . a:key
  call clipper#auto_push()
endfunction


function! clipper#yank_s(key)
  let reg0_save = @0
  execute "normal! gvy"
  call clipper#auto_push()
  let @0 = reg0_save

  execute "normal! gv\<C-g>"
  call feedkeys(a:key, 'n')

  silent! call altrepeat#set_repeat_function(
        \ 'clipper#repeat_yank_s',
        \ 0,
        \ v:prevcount
        \)
endfunction


function! clipper#repeat_yank_s(count)
  execute 'normal! .'
  call clipper#auto_push()
endfunction


function! clipper#yank_x(key)
  let use_reg = ''
  if v:register ==# '' || v:register ==# '"'
    let s:used_register = ''
    let yank_key = ''

    if visualmode() ==# "\<C-v>" && a:key ==# 'D'
      let firstpos = getpos("'<")
      let lastpos = getpos("'>")
      if firstpos[2] > lastpos[2]
        let yank_key = 'o$y'
      else
        let yank_key = '$y'
      endif
    elseif a:key =~ '\C^\u$'
      let yank_key = 'Y'
    else
      let yank_key = 'y'
    endif

    let reg0_save = @0
    execute printf("normal! gv%s", yank_key)
    call clipper#auto_push()
    let @0 = reg0_save
  else
    let use_reg = '"' . v:register
    let s:used_register = use_reg
  endif

  call feedkeys(printf('gv%s%s', use_reg, a:key), 'n')

  silent! call altrepeat#set_repeat_function(
        \ 'clipper#repeat_yank_x',
        \ 0,
        \ v:prevcount
        \)
endfunction


function! clipper#repeat_yank_x(count)
  let use_reg = ''
  if !empty(s:used_register)
    let use_reg = s:used_register
  elseif v:register !=# '' && v:register !=# '"'
    let use_reg = '"' . v:register
    let s:used_register = use_reg
  endif
  execute 'normal! ' . use_reg . '.'
  call clipper#auto_push()
endfunction


function! clipper#insert_n(key)
  let dic = { 'C' : 'y$', 's' : 'yl', 'S' : 'Y' }
  if !has_key(dic, a:key)
    return
  endif

  let cnt = (v:count == v:count1 ? v:count : 1)
  let use_reg = ''
  if v:register ==# '' || v:register ==# '"'
    let reg0_save = @0
    execute printf('normal! %s%s', cnt, dic[a:key])
    call clipper#auto_push()
    let @0 = reg0_save
  else
    let use_reg = '"' . v:register
  endif

  call feedkeys(printf('%s%s%s', cnt, use_reg, a:key), 'n')

  silent! call altrepeat#set_repeat_function(
        \ 'clipper#repeat_insert_n',
        \ 1,
        \ v:prevcount
        \)
endfunction


function! clipper#repeat_insert_n(count)
  execute printf('normal! %s.', (a:count ? a:count : ''))
  call clipper#auto_push()
endfunction


function! clipper#insert_s(key)
  let reg0_save = @0
  execute "normal! gvy"
  call clipper#auto_push()
  let @0 = reg0_save

  execute "normal! gv\<C-g>"
  call feedkeys(a:key, 'n')

  silent! call altrepeat#set_repeat_function(
        \ 'clipper#repeat_insert_s',
        \ 1,
        \ v:prevcount
        \)
endfunction


function! clipper#repeat_insert_s(count)
  execute 'normal! .'
  call clipper#auto_push()
endfunction


function! clipper#insert_x(key)
  let use_reg = ''
  if v:register ==# '' || v:register ==# '"'
    if a:key =~ '\C^\u$'
      let yank_key = 'Y'
    else
      let yank_key = 'y'
    endif

    let reg0_save = @0
    execute printf('normal! gv%s', yank_key)
    call clipper#auto_push()
    let @0 = reg0_save
  else
    let use_reg = '"' . v:register
  endif

  call feedkeys(printf('gv%s%s', use_reg, a:key), 'n')

  silent! call altrepeat#set_repeat_function(
        \ 'clipper#repeat_insert_x',
        \ 1,
        \ v:prevcount
        \)
endfunction


function! clipper#repeat_insert_x(count)
  execute 'normal! .'
  call clipper#auto_push()
endfunction


function! s:setreg()
  if (v:register ==# '"' || v:register ==# '') && !empty(s:stack)
    let stack = s:stack[s:stack_pos]
    call setreg('"', stack[0], stack[1])
    let s:stack_pos = 0
  endif
endfunction


function! clipper#paste_n(key)
  call s:setreg()
  return a:key
endfunction


function! clipper#paste_x(key)
  let use_reg = ''
  if v:register !=# '' && v:register !=# '"'
    let use_reg = '"' . v:register
    let s:used_register = use_reg
  endif
  let stack_empty = empty(s:stack)

  let unnamed_reg_save = @"
  let reg0_save = @0
  execute 'normal! gv""y'
  call clipper#auto_push()
  let @0 = reg0_save
  let @" = unnamed_reg_save

  if !stack_empty
    let s:stack_pos += 1
    call s:setreg()
  endif

  call feedkeys(printf('gv%s%s', use_reg, a:key), 'n')
  silent! call altrepeat#set_repeat_function(
        \ 'clipper#repeat_paste_x',
        \ 0,
        \ v:prevcount
        \)
endfunction


function! clipper#repeat_paste_x(count)
  let use_reg = ''
  if !empty(s:used_register)
    let use_reg = s:used_register
  elseif v:register !=# '' && v:register !=# '"'
    let use_reg = '"' . v:register
    let s:used_register = use_reg
  endif
  execute 'normal! ' . use_reg . '.'
  call clipper#auto_push()
endfunction


function! clipper#do_operator(key, opfunc)
  let &operatorfunc = a:opfunc
  let s:operator = a:key
  let s:used_register = s:register()
  call feedkeys(printf('%s%sg@', s:count(), s:used_register), 'n')
endfunction


function! clipper#linewise(key)
  if s:operator ==# a:key
    return 'g@'
  endif
  return "\<Esc>"
endfunction


function! clipper#operator_y(optype)
  let view = winsaveview()
  "let save_cursor = getpos('.')
  call s:operator(a:optype, 'y')
  call winrestview(view)
  "call setpos('.', save_cursor)
endfunction


function! clipper#operator_d(optype)
  call s:operator(a:optype, 'd')
endfunction


function! s:operator(optype, cmd)
  let selection_save = &selection
  let &selection = 'inclusive'

  if s:used_register ==# ''
    let s:used_register = s:register()
  endif
  execute printf('normal! %s%s%s', s:range(a:optype), s:used_register, a:cmd)
  call clipper#auto_push()

  let &selection = selection_save
endfunction


function! s:range(optype)
  if a:optype ==# 'line'
    return "'[V']"
  elseif a:optype ==# 'block'
    return "`[\<C-v>`]"
  else
    return "`[v`]"
  endif
endfunction


function! clipper#pseudo_operator(key)
  augroup ClipperOperatorGroup
    autocmd!
  augroup END
  let s:used_register = s:register()
  if s:used_register ==# ''
    autocmd ClipperOperatorGroup InsertEnter * call s:insert_enter()
    autocmd ClipperOperatorGroup CursorMoved * call s:cursor_moved()

    let s:operator_start = 1
    let s:operator_insert_enter = 0
    let s:changedtick = b:changedtick
  endif

  silent! call altrepeat#set_repeat_function(
        \ 'clipper#repeat_pseudo_operator',
        \ 1,
        \ v:prevcount
        \)

  return a:key
endfunction


function! clipper#repeat_pseudo_operator(count)
  if s:used_register ==# ''
    let s:used_register = s:register()
  endif
  execute 'normal! ' . (a:count ? a:count : '') . s:used_register . '.'
  call clipper#auto_push()
endfunction


function! s:insert_enter()
  if s:operator_start
    if !s:operator_insert_enter
      let s:operator_insert_enter = 1
    else
      let s:operator_start = 0
      let s:operator_insert_enter = 0
      autocmd! ClipperOperatorGroup
    endif
  endif
endfunction


function! s:cursor_moved()
  if s:operator_start
    if s:operator_insert_enter
      call clipper#auto_push()
      let s:operator_start = 0
      let s:operator_insert_enter = 0
      autocmd! ClipperOperatorGroup
    elseif s:changedtick != b:changedtick
      let s:operator_start = 0
      let s:operator_insert_enter = 0
      autocmd! ClipperOperatorGroup
    endif
  endif
endfunction


function! clipper#auto_push()
  if v:register ==# '"' || v:register ==# ''
    call insert(s:stack, [getreg('"'), getregtype('"')], 0)
  endif
  return ''
endfunction


function! clipper#stack_move(type)
  redraw
  if empty(s:stack)
    echohl ErrorMsg
    echomsg '[clipper] stack is empty.'
    echohl None
    return
  endif

  if a:type < 0
    if s:stack_pos < len(s:stack) - 1
      let s:stack_pos += 1
    endif
  elseif a:type > 0
    if s:stack_pos
      let s:stack_pos -= 1
    endif
  endif
  echon 'selected clipper stack #' . s:stack_pos
endfunction


function! s:count()
  return v:count == v:count1 ? v:count : ''
endfunction


function! s:register()
  if v:register ==# '' || v:register ==# '"'
    return ''
  endif
  return '"' . v:register 
endfunction


let s:operator_insert_enter = 0
let s:operator_start = 0
let s:stack = []
let s:register = ''
let s:operator = ''
let s:stack_pos = 0
let s:used_register = ''
let s:changedtick = -1




" vim: expandtab shiftwidth=2 softtabstop=2 foldmethod=marker
