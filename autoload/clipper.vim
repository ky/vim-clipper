"-----------------------------------------------------------------------------
" clipper
" Author: ky
" Version: 0.1.3
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
  echo string(s:queue)
endfunction


function! clipper#execute(type, key)
  let v = s:FUNCTION_TABLE[a:type . '_' . a:key]
  return call(v[0], v[1])
endfunction


function! s:yank_n(key)
  call s:set_register()
  let cnt = (v:count == v:count1 ? v:count : '')

  execute 'normal! ' . cnt . s:used_register . a:key
  call s:push()

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
  call s:push()
endfunction


function! s:yank_s(key)
  let reg0_save = @0
  execute "normal! gvy"
  call s:push()
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
  call s:push()
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
    call s:push()
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
  call s:push()
endfunction


function! s:yank_x_y(key)
  "let s:ve_save = &virtualedit
  "let &virtualedit = 'onemore'
  "let reselect = "\<Plug>(clipper_reselect_" .
  "      \ (a:key ==# 'y' ? "char" : "line") . ")"
  "call s:do_operator('y')
  "call feedkeys(reselect, 'm')
  "call feedkeys(":call clipper#yank_x_y_after()\<CR>", 'n')
  "if a:key ==# 'Y' && visualmode() !=# "\<C-v>"
  "  call feedkeys('0', 'n')
  "endif
  let s:cpoptions = &cpoptions
  set cpoptions+=y
  call feedkeys('gv' . a:key, 'n')
  call feedkeys(":call clipper#yank_x_y_after()\<CR>", 'n')
  silent! call altrepeat#set_repeat_function(
        \ 'clipper#repeat_yank_x_y',
        \ 0,
        \ v:prevcount
        \)
endfunction


function! clipper#yank_x_y_after()
  "let &virtualedit = s:ve_save
  "unlet s:ve_save
  call s:push()
  let &cpoptions = s:cpoptions
endfunction


function! clipper#repeat_yank_x_y(count)
  let use_reg = ''
  if !empty(s:used_register)
    let use_reg = s:used_register
  elseif v:register !=# '' && v:register !=# '"'
    let use_reg = '"' . v:register
    let s:used_register = use_reg
  endif
  execute 'normal! ' . use_reg . '.'
  call s:push()
endfunction


onoremap <silent> <Plug>(clipper_reselect_char)
      \ :<C-u>call <SID>reselect_char()<CR>
onoremap <silent> <Plug>(clipper_reselect_line)
      \ :<C-u>call <SID>reselect_line(visualmode())<CR>


function! s:reselect_char()
  normal! gv
  normal! $
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
    call s:push()
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
  call s:push()
endfunction


function! s:insert_s(key)
  let reg0_save = @0
  execute "normal! gvy"
  call s:push()
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
  call s:push()
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
    call s:push()
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
  call s:push()
endfunction


function! s:setreg()
  if (v:register ==# '"' || v:register ==# '') && !empty(s:queue)
    let queue = s:queue[s:queue_pos]
    call setreg('"', queue[0], queue[1])
    let s:queue_pos = 0
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
  let queue_empty = empty(s:queue)

  let unnamed_reg_save = @@
  let reg0_save = @0
  execute 'normal! gv""y'
  call s:push()
  let @0 = reg0_save
  let @@ = unnamed_reg_save

  if !queue_empty
    let s:queue_pos += 1
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
  call s:push()
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
  call s:push()

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


function! clipper#define_operator_autocmd()
  augroup ClipperOperatorGroup
    autocmd!
    autocmd InsertEnter * call s:insert_enter()
    autocmd CursorMovedI * call s:cursor_movedi()
    autocmd CursorMoved * call s:cursor_moved()
  augroup END
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

  call feedkeys(a:key, 'n')
endfunction


function! clipper#repeat_pseudo_operator(count)
  call s:update_register()
  execute 'normal! ' . (a:count ? a:count : '') . s:used_register . '.'
  call s:push()
endfunction


function! s:initialize_pseudo_operator()
  let s:pseudo_operator_start = 0
  let s:operator_insert_enter = 0
  autocmd! ClipperOperatorGroup
endfunction


function! s:insert_enter()
  if s:pseudo_operator_start
    if s:changedtick != b:changedtick && !s:operator_insert_enter
      let s:operator_insert_enter = 1
    else
      " c<ESC>i
      call s:initialize_pseudo_operator()
    endif
  endif
endfunction


function! s:cursor_movedi()
  if s:pseudo_operator_start && s:operator_insert_enter
    call s:push()
    call s:initialize_pseudo_operator()
  endif
endfunction


function! s:cursor_moved()
  if s:pseudo_operator_start
    " c<ESC>j
    call s:initialize_pseudo_operator()
  endif
endfunction


function! s:push()
  if (v:register ==# '"' || v:register ==# '') && g:clipper_max_history > 0
    if len(s:queue) >= g:clipper_max_history
      call remove(s:queue, g:clipper_max_history -1, -1)
    endif
    let s = strpart(getreg('"'), 0, g:clipper_max_text_length)
    call insert(s:queue, [s, getregtype('"')], 0)
  endif
  return ''
endfunction


function! clipper#set_queue_pos(line_num)
  if empty(s:queue)
    redraw
    echohl ErrorMsg
    echomsg '[clipper] queue is empty.'
    echohl None
    return
  endif
  let queue_pos = a:line_num - 1
  if queue_pos >= 0
    let s:queue_pos = queue_pos
  endif
endfunction


function! clipper#queue_move(type)
  redraw
  if empty(s:queue)
    echohl ErrorMsg
    echomsg '[clipper] queue is empty.'
    echohl None
    return
  endif

  if a:type < 0
    if s:queue_pos < len(s:queue) - 1
      let s:queue_pos += 1
    endif
  elseif a:type > 0
    if s:queue_pos
      let s:queue_pos -= 1
    endif
  endif
  echon 'selected clipper queue #' . s:queue_pos
endfunction


function! clipper#select()
  if empty(s:queue)
    redraw
    echohl ErrorMsg
    echomsg '[clipper] queue is empty.'
    echohl None
    return
  endif

  if s:winnr != -1
    return
  endif

  let s:winnr = winnr()

  if bufexists(s:bufnr)
    botright split
    silent! execute s:bufnr . 'buffer'
  else
    botright new
    let s:bufnr = bufnr('%')
  endif

  setlocal bufhidden=hide
  setlocal nobuflisted
  setlocal buftype=nofile
  setlocal noswapfile
  setlocal filetype=clipper
  setlocal nowrap
  setlocal modifiable
  execute 'silent! file [clipper]'

  augroup ClipperSelectWindowAugroup
    autocmd!
    autocmd! BufLeave <buffer> call clipper#select_end()
    autocmd! WinLeave <buffer> call clipper#select_end()
  augroup END

  let line_num = 1
  let lines = ''
  let format = '%' . len(string(g:clipper_max_history)) . "d: %s\n"
  for i in s:queue
    let lines .= printf(format, line_num, strtrans(i[0]))
    let line_num += 1
  endfor

  silent! %delete _
  0put =lines
  silent! $delete _

  setlocal nomodifiable

  normal! gg

  call s:select_win_mappings()
endfunction


function! clipper#select_end()
  autocmd! ClipperSelectWindowAugroup
  if s:winnr != -1
    close
    execute s:winnr . 'wincmd w'
    let s:winnr = -1
  endif
endfunction


function! s:select_win_mappings()
  nnoremap <silent> <Plug>(clipper_select_end)
        \ :<C-u>call clipper#select_end()<CR>
  nnoremap <silent> <Plug>(clipper_set_queue_pos)
        \ :<C-u>call clipper#set_queue_pos(line('.'))<CR>
  for x in [ 'p' , 'P', 'gp', 'gP', '[p', ']p', '[P', ']P' ]
    execute printf(
          \ 'nmap <buffer> <silent> <Plug>(clipper_select_win_%s) ' .
          \ '<Plug>(clipper_set_queue_pos)' .
          \ '<Plug>(clipper_select_end)' .
          \ '<Plug>(clipper_%s)',
          \ x, x)
    if !g:clipper_no_default_key_mappings
      execute printf('nmap <buffer> %s <Plug>(clipper_select_win_%s)', x, x)
    endif
  endfor
  if !g:clipper_no_default_key_mappings
    nmap <buffer> <CR> <Plug>(clipper_select_win_p)
  endif
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


let s:bufnr = -1
let s:winnr = -1
let s:operator_insert_enter = 0
let s:pseudo_operator_start = 0
let s:queue = []
let s:operator = ''
let s:queue_pos = 0
let s:used_register = ''
let s:changedtick = -1


if !exists('g:clipper_max_text_length')
  let g:clipper_max_text_length = 1048576
endif


if !exists('g:clipper_max_history')
  let g:clipper_max_history = 100
endif




" vim: expandtab shiftwidth=2 softtabstop=2 foldmethod=marker
