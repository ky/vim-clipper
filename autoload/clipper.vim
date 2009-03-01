"-----------------------------------------------------------------------------
" clipper
" Author: ky
" Version: 0.1.1
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


function! clipper#execute(type, key)
  let v = s:FUNCTION_TABLE[a:type . '_' . a:key]
  return call(v[0], v[1])
endfunction


function! s:yank_n(key)
  call s:set_register()
  let cnt = (v:count == v:count1 ? v:count : '')

  execute 'normal! ' . cnt . s:used_register . a:key
  call clipper#auto_push()

  silent! call altrepeat#set_repeat_function(
        \ 'clipper#repeat_yank_n',
        \ 0,
        \ (cnt ==# '' ? 0 : cnt),
        \ [a:key]
        \)
endfunction


function! clipper#repeat_yank_n(count, key)
  call s:update_register()
  execute 'normal! ' . (a:count ? a:count : '') . s:used_register . a:key
  call clipper#auto_push()
endfunction


function! s:yank_s(key)
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


function! s:yank_x(key)
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


function! s:yank_x_y(key)
  let s:ve_save = &virtualedit
  let &virtualedit = 'onemore'
  let reselect = "\<Plug>(clipper_reselect_" .
        \ (a:key ==# 'y' ? "char" : "line") . ")"
  call s:do_operator('y')
  call feedkeys(reselect, 'm')
  call feedkeys(":call clipper#yank_x_y_after()\<CR>", 'n')
  if a:key ==# 'Y' && visualmode() !=# "\<C-v>"
    call feedkeys('0', 'n')
  endif
endfunction


function! clipper#yank_x_y_after()
  let &virtualedit = s:ve_save
  unlet s:ve_save
endfunction


onoremap <silent> <Plug>(clipper_reselect_char)
      \ :<C-u>call <SID>reselect_char()<CR>
onoremap <silent> <Plug>(clipper_reselect_line)
      \ :<C-u>call <SID>reselect_line(visualmode())<CR>


function! s:reselect_char()
  normal! gv
endfunction


function! s:reselect_line(visual)
  normal! gv
  if a:visual ==# 'v'
    normal! V
  endif
endfunction


function! s:insert_n(key)
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


function! s:insert_s(key)
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


function! s:insert_x(key)
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


function! s:paste_n(key)
  call s:setreg()
  return a:key
endfunction


function! s:paste_x(key)
  let use_reg = ''
  if v:register !=# '' && v:register !=# '"'
    let use_reg = '"' . v:register
    let s:used_register = use_reg
  endif
  let stack_empty = empty(s:stack)

  let unnamed_reg_save = @@
  let reg0_save = @0
  execute 'normal! gv""y'
  call clipper#auto_push()
  let @0 = reg0_save
  let @@ = unnamed_reg_save

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


function! s:do_operator(key)
  let &operatorfunc = 'clipper#operator_' . a:key
  let s:operator = a:key
  call s:set_register()
  let s:view = winsaveview()
  call feedkeys(printf('%s%sg@', s:count(), s:used_register), 'n')
endfunction


function! s:linewise(key)
  if s:operator ==# a:key
    return 'g@'
  endif
  return "\<Esc>"
endfunction


function! clipper#operator_y(optype)
  if !exists('s:view')
    let s:view = winsaveview()
  endif
  call s:operator(a:optype, 'y')
  call winrestview(s:view)
  unlet s:view
endfunction


function! clipper#operator_d(optype)
  call s:operator(a:optype, 'd')
endfunction


function! clipper#repeat_operator(count)
  let view = winsaveview()
  call s:update_register()
  execute 'normal! ' . (a:count ? a:count : '') .
        \ s:used_register . '.'
  call winrestview(view)
endfunction


function! s:operator(optype, cmd)
  let selection_save = &selection
  let &selection = 'inclusive'

  call s:update_register()
  execute printf('normal! %s%s%s', s:range(a:optype), s:used_register, a:cmd)
  call clipper#auto_push()

  let &selection = selection_save

  if a:cmd ==# 'y'
    silent! call altrepeat#set_repeat_function(
          \ 'clipper#repeat_operator',
          \ 0,
          \ v:prevcount
          \)
  endif
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


function! s:pseudo_operator(key)
  call s:set_register()

  if s:used_register ==# ''
    let s:pseudo_operator_start = 1
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
  call s:update_register()
  execute 'normal! ' . (a:count ? a:count : '') . s:used_register . '.'
  call clipper#auto_push()
endfunction


function! s:initialize_pseudo_operator()
  let s:pseudo_operator_start = 0
  let s:operator_insert_enter = 0
endfunction


function! clipper#insert_enter()
  if s:pseudo_operator_start
    if s:changedtick != b:changedtick && !s:operator_insert_enter
      let s:operator_insert_enter = 1
    else
      " c<ESC>i
      call s:initialize_pseudo_operator()
    endif
  endif
endfunction


function! clipper#cursor_movedi()
  if s:pseudo_operator_start && s:operator_insert_enter
    call clipper#auto_push()
    call s:initialize_pseudo_operator()
  endif
endfunction


function! clipper#cursor_moved()
  if s:pseudo_operator_start
    " c<ESC>j
    call s:initialize_pseudo_operator()
  endif
endfunction


function! clipper#auto_push()
  if (v:register ==# '"' || v:register ==# '') && g:clipper_max_history > 0
    if len(s:stack) >= g:clipper_max_history
      call remove(s:stack, g:clipper_max_history -1, -1)
    endif
    let s = strpart(getreg('"'), 0, g:clipper_max_text_length)
    call insert(s:stack, [s, getregtype('"')], 0)
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


function! s:get_register()
  if v:register ==# '' || v:register ==# '"'
    return ''
  endif
  return '"' . v:register 
endfunction


function! s:set_register()
  let s:used_register = s:get_register()
endfunction


function! s:update_register()
  if s:used_register ==# ''
    call s:set_register()
  endif
endfunction


let s:FUNCTION_TABLE = {
      \ 'n_y'     : [ 's:do_operator', [ 'y' ] ],
      \ 'o_y'     : [ 's:linewise', [ 'y' ] ],
      \ 'n_d'     : [ 's:do_operator', [ 'd' ] ],
      \ 'o_d'     : [ 's:linewise', [ 'd' ] ],
      \ 'n_c'     : [ 's:pseudo_operator', [ 'c' ] ],
      \ 's_<BS>'  : [ 's:yank_s', [ "\<Del>" ] ],
      \ 'x_d'     : [ 's:yank_x', [ 'd' ] ],
      \ 'n_D'     : [ 's:yank_n', [ 'D' ] ],
      \ 'x_D'     : [ 's:yank_x', [ 'D' ] ],
      \ 'n_<Del>' : [ 's:yank_n', [ 'x' ] ],
      \ 's_<Del>' : [ 's:yank_s', [ "\<Del>" ] ],
      \ 'x_<Del>' : [ 's:yank_x', [ "\<Del>" ] ],
      \ 's_<C-h>' : [ 's:yank_s', [ "\<Del>" ] ],
      \ 'n_x'     : [ 's:yank_n', [ 'x' ] ],
      \ 'x_x'     : [ 's:yank_x', [ 'x' ] ],
      \ 'n_X'     : [ 's:yank_n', [ 'X' ] ],
      \ 'x_X'     : [ 's:yank_x', [ 'X' ] ],
      \ 'x_y'     : [ 's:yank_x_y', [ 'y' ] ],
      \ 'n_Y'     : [ 's:yank_n', [ 'Y' ] ],
      \ 'x_Y'     : [ 's:yank_x_y', [ 'Y' ] ],
      \ 'x_c'     : [ 's:insert_x', [ 'c' ] ],
      \ 'n_C'     : [ 's:insert_n', [ 'C' ] ],
      \ 'x_C'     : [ 's:insert_x', [ 'C' ] ],
      \ 's_<CR>'  : [ 's:insert_s', [ "\<CR>" ] ],
      \ 's_<NL>'  : [ 's:insert_s', [ "\<NL>" ] ],
      \ 'n_s'     : [ 's:insert_n', [ 's' ] ],
      \ 'x_s'     : [ 's:insert_x', [ 's' ] ],
      \ 'n_S'     : [ 's:insert_n', [ 'S' ] ],
      \ 'x_S'     : [ 's:insert_x', [ 'S' ] ],
      \ 'n_p'     : [ 's:paste_n', [ 'p' ] ],
      \ 'x_p'     : [ 's:paste_x', [ 'p' ] ],
      \ 'n_P'     : [ 's:paste_n', [ 'P' ] ],
      \ 'x_P'     : [ 's:paste_x', [ 'P' ] ],
      \ 'n_gp'    : [ 's:paste_n', [ 'gp' ] ],
      \ 'n_gP'    : [ 's:paste_n', [ 'gP' ] ],
      \ 'n_[p'    : [ 's:paste_n', [ '[p' ] ],
      \ 'n_]p'    : [ 's:paste_n', [ ']p' ] ],
      \ 'n_[P'    : [ 's:paste_n', [ '[P' ] ],
      \ 'n_]P'    : [ 's:paste_n', [ ']P' ] ],
      \}


let s:operator_insert_enter = 0
let s:pseudo_operator_start = 0
let s:stack = []
let s:operator = ''
let s:stack_pos = 0
let s:used_register = ''
let s:changedtick = -1


if !exists('g:clipper_max_text_length')
  let g:clipper_max_text_length = 1048576
endif


if !exists('g:clipper_max_history')
  let g:clipper_max_history = 100
endif




" vim: expandtab shiftwidth=2 softtabstop=2 foldmethod=marker
