! ==========================================================================
!
! Parser for configuration files that provides a means to read input
! parameters from text files in a relatively free format.
!
! Copyright (C) 2009 Jindrich Kolorenc
!
! Permission is hereby granted, free of charge, to any person
! obtaining a copy of this software and associated documentation files
! (the "Software"), to deal in the Software without restriction,
! including without limitation the rights to use, copy, modify, merge,
! publish, distribute, sublicense, and/or sell copies of the Software,
! and to permit persons to whom the Software is furnished to do so,
! subject to the following conditions:
!
! The above copyright notice and this permission notice shall be
! included in all copies or substantial portions of the Software.
!
! THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
! EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
! MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
! NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
! BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
! ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
! CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
! SOFTWARE.
!
! ==========================================================================

module cfparser
  use list_words
  implicit none
  private

  ! make public the minimum the user needs from 'list_words' module
  public :: t_words, clear

  ! subroutines defined here in this module
  public :: readConf
  public :: readvalue, readsection, readsection_and_erase, haskeyword
  public :: write_default
  public :: string_eq

  interface readvalue
     module procedure readvalue_dp
     module procedure readvalue_dp_array
     module procedure readvalue_dp_2Darray
     module procedure readvalue_int
     module procedure readvalue_int_array
     module procedure readvalue_int_2Darray
     module procedure readvalue_string
  end interface

  ! char(9) is tab
  character,    parameter :: startsec="{"
  character,    parameter :: endsec="}"
  character,    parameter :: comment="#"
  character(*), parameter :: separators=" "//char(9)

  integer, parameter :: line_len=512
  integer, parameter :: word_len=64

  ! iostat return values for non-advancing read (not used here, because
  ! non-advancing read) didn't seem to cut it
  integer, parameter :: EOF=-1
  integer, parameter :: EOL=-2

  integer, parameter :: dp=kind(1.0d0)

contains

  subroutine readConf(words,filename)
    ! {{{ read config file into a linked list 'words'
    type(t_words), intent(inout) :: words
    character(len=*), intent(in) :: filename
    type(t_words_iterator), pointer :: word
    integer :: level
    call clear(words)
    call read_file_to_words(filename,words)

    ! execute includes as long as there are some
    do while ( perform_first_include(words) )
    end do

    ! check syntax of the configuration options
    level=0
    word => begin(words)
    do while ( associated(word) )
       if ( trim(word%val) == startsec ) level=level+1
       if ( trim(word%val) == endsec   ) level=level-1
       if ( level < 0 ) exit    ! section closed before it was opened
       call increment(word)
    end do
    if ( level /= 0 ) then
       print *, "Mismatched section delimitiers '",startsec,"' and '", &
            endsec,"' in the input file '",trim(filename),"'"
       stop
    end if

    !word => begin(words)
    !do while ( associated(word) )
    !   print *, word%val
    !   call increment(word)
    !end do
    !stop

    ! }}}
  end subroutine readConf

  subroutine read_file_to_words(filename,words)
    ! {{{ read content of a file into a linked list 'words'
    character(len=*), intent(in) :: filename
    type(t_words), intent(inout) :: words
    character(len=line_len) :: line
    integer :: status
    open(unit=10,file=filename,action="read",status='old',iostat=status)
    if ( status /= 0 ) then
       close(unit=10)
       print *, "File ", trim(filename), " not found."
       stop
    end if
    readfile: do
       read(10,fmt='(a512)',iostat=status) line
       if ( status /= 0 ) exit readfile
       if ( len_trim(line) > 0 ) then
          call add_line_to_words(line,words)
       end if
    end do readfile
    close(unit=10)
    ! }}}
  end subroutine read_file_to_words

  function perform_first_include(words) result(action_taken)
    ! {{{ search words for an include statements and do the inclusion
    type(t_words), intent(inout) :: words
    logical :: action_taken
    type(t_words) :: new_words
    type(t_words_iterator), pointer :: pos, pos_new
    action_taken=.false.
    pos => begin(words)
    do while ( associated(pos) )
       if ( string_eq(pos%val,"include") ) then
          call erase(words,pos)    ! remove 'include'
          if ( associated(pos) ) then
             call read_file_to_words(pos%val,new_words)
             call erase(words,pos) ! remove the filename
             pos_new => begin(new_words)
             do while ( associated(pos_new) )
                if ( associated(pos) ) then
                   call insert(words,pos,pos_new%val)
                else
                   call push_back(words,pos_new%val)
                end if
                call increment(pos_new)
             end do
             action_taken=.true.
             exit
          else
             print *, "Reading input: expected a file name to follow ", &
                  "'include' but file already ended."
             stop
          end if
       else
          call increment(pos)
       end if
    end do
    call clear(new_words)
    ! }}}
  end function perform_first_include

  subroutine add_line_to_words(line,words)
    ! {{{ decompose 'line' into words and add them to the list 'words';
    !     comments are dropped right away
    character(len=line_len), intent(in) :: line
    type(t_words), intent(inout) :: words
    type(t_words_iterator), pointer :: word => null()
    character(len=word_len) :: empty_word
    integer :: i, Nwords, wordsize
    logical :: inword

    forall ( i=1:word_len ) empty_word(i:i)=" "

    Nwords=0
    wordsize=0
    inword=.false.
    insert_words: do i=1, len_trim(line)
       if ( ( line(i:i) == startsec ) .or. ( line(i:i) == endsec ) ) then
          Nwords=Nwords+1
          call push_back(words,empty_word)
          word => end(words)
          word%val=line(i:i)
          inword=.false.
          cycle
       end if
       if ( line(i:i) == comment ) exit insert_words
       !if ( ( line(i:i) /= " " ) .and. inword ) then
       if ( ( index(separators,line(i:i)) < 1  ) .and. inword ) then
          wordsize=wordsize+1
          if ( wordsize > word_len ) then
             print *, "line2words: encountered too long a word: ", &
                  trim(word%val)
             stop
          end if
          word%val(wordsize:wordsize)=line(i:i)
       end if
       !if ( ( line(i:i) /= " " ) .and. .not.inword ) then
       if ( ( index(separators,line(i:i)) < 1  ) .and. .not.inword ) then
          Nwords=Nwords+1
          wordsize=1
          call push_back(words,empty_word)
          word => end(words)
          word%val(1:1)=line(i:i)
          inword=.true.
       end if
       !if ( ( line(i:i) == " " ) .and. inword ) then
       if ( ( index(separators,line(i:i)) > 0  ) .and. inword ) then
          inword=.false.
       end if
    end do insert_words
    
    ! }}}
  end subroutine add_line_to_words

  function readvalue_dp(words,value,valuename) result(res)
    ! {{{ read double precision real number
    logical :: res
    type(t_words), intent(in) :: words
    ! intent(inout) for 'value' because we do not want to destroy possible
    ! default assigned to 'value' before the call
    real(dp), intent(inout) :: value
    character(len=*), intent(in) :: valuename
    !
    type(t_words_iterator), pointer :: pos
    integer :: level, error
    !
    res=.false.
    level=0
    pos => begin(words)
    do while ( associated(pos) )
       if ( ( level == 0 ) .and. string_eq(pos%val,valuename) ) then
          call increment(pos)
          if ( associated(pos) ) then
             read(unit=pos%val,fmt=*,iostat=error) value
             if ( error /= 0 ) then
                print *, "Reading input: expected a real value to follow '", &
                     valuename, "' but got '", trim(pos%val), "' instead."
                stop
             end if
             res=.true.
             exit
          else
             print *, "Reading input: expected a real value to follow '", &
                  valuename, "' but file already ended."
             stop
          end if
       end if
       level=level+level_adjustment(pos%val)
       call increment(pos)
    end do
    ! }}}
  end function readvalue_dp

  function readvalue_int(words,value,valuename) result(res)
    ! {{{ read integer
    logical :: res
    type(t_words), intent(in) :: words
    ! intent(inout) for 'value' because we do not want to destroy possible
    ! default assigned to 'value' before the call
    integer, intent(inout) :: value
    character(len=*), intent(in) :: valuename
    !
    type(t_words_iterator), pointer :: pos
    integer :: level, error
    !
    res=.false.
    level=0
    pos => begin(words)
    do while ( associated(pos) )
       if ( ( level == 0 ) .and. string_eq(pos%val,valuename) ) then
          call increment(pos)
          if ( associated(pos) ) then
             read(unit=pos%val,fmt=*,iostat=error) value
             if ( error /= 0 ) then
                print *, "Reading input: expected an integer value to ", &
                     "follow '", valuename, "' but got '", trim(pos%val), &
                     "' instead."
                stop
             end if
             res=.true.
             exit
          else
             print *, "Reading input: expected an integer value to ", &
                  "follow '", valuename, "' but file already ended."
             stop
          end if
       end if
       level=level+level_adjustment(pos%val)
       call increment(pos)
    end do
    ! }}}
  end function readvalue_int

  function readvalue_string(words,value,valuename) result(res)
    ! {{{ read string
    logical :: res
    type(t_words), intent(in) :: words
    ! intent(inout) for 'value' because we do not want to destroy possible
    ! default assigned to 'value' before the call
    character(len=*), intent(inout) :: value
    character(len=*), intent(in) :: valuename
    !
    type(t_words_iterator), pointer :: pos
    integer :: level
    !
    res=.false.
    level=0
    pos => begin(words)
    do while ( associated(pos) )
       if ( ( level == 0 ) .and. string_eq(pos%val,valuename) ) then
          call increment(pos)
          if ( associated(pos) ) then
             value=pos%val
             res=.true.
             exit
          else
             print *, "Reading input: expected a string to ", &
                  "follow '", valuename, "' but file already ended."
             stop
          end if
       end if
       level=level+level_adjustment(pos%val)
       call increment(pos)
    end do
    ! }}}
  end function readvalue_string

  function readvalue_dp_array(words,value,valuename) result(res)
    ! {{{ read array of double precision real numbers
    logical :: res
    type(t_words), intent(in) :: words
    ! intent(inout) for 'value' because we do not want to destroy possible
    ! default assigned to 'value' before the call
    real(dp), dimension(:), intent(inout) :: value
    character(len=*), intent(in) :: valuename
    !
    type(t_words_iterator), pointer :: pos
    integer :: i, level, error
    !
    res=.false.
    level=0
    pos => begin(words)
    search_value: do while ( associated(pos) )
       if ( ( level == 0 ) .and. string_eq(pos%val,valuename) ) then
          read_array: do i=1, size(value)
             call increment(pos)
             if ( associated(pos) ) then
                read(unit=pos%val,fmt=*,iostat=error) value(i)
                if ( error /= 0 ) then
                   print '(1x,a,i3,5a)', "Reading input: expected ", &
                        size(value), " real values to follow '", valuename, &
                        "' but got '", trim(pos%val), "' instead."
                   stop
                end if
                res=.true.
             else
                print '(1x,a,i3,5a)', "Reading input: expected ", &
                     size(value), " real values to follow '", valuename, &
                     "' but file already ended."
                stop
             end if
          end do read_array
          exit search_value
       end if
       level=level+level_adjustment(pos%val)
       call increment(pos)
    end do search_value
    ! }}}
  end function readvalue_dp_array

  function readvalue_int_array(words,value,valuename) result(res)
    ! {{{ read array of double precision real numbers
    logical :: res
    type(t_words), intent(in) :: words
    ! intent(inout) for 'value' because we do not want to destroy possible
    ! default assigned to 'value' before the call
    integer, dimension(:), intent(inout) :: value
    character(len=*), intent(in) :: valuename
    !
    type(t_words_iterator), pointer :: pos
    integer :: i, level, error
    !
    res=.false.
    level=0
    pos => begin(words)
    search_value: do while ( associated(pos) )
       if ( ( level == 0 ) .and. string_eq(pos%val,valuename) ) then
          read_array: do i=1, size(value)
             call increment(pos)
             if ( associated(pos) ) then
                read(unit=pos%val,fmt=*,iostat=error) value(i)
                if ( error /= 0 ) then
                   print '(1x,a,i3,5a)', "Reading input: expected ", &
                        size(value), " integer values to follow '", valuename, &
                        "' but got '", trim(pos%val), "' instead."
                   stop
                end if
                res=.true.
             else
                print '(1x,a,i3,5a)', "Reading input: expected ", &
                     size(value), " integer values to follow '", valuename, &
                     "' but file already ended."
                stop
             end if
          end do read_array
          exit search_value
       end if
       level=level+level_adjustment(pos%val)
       call increment(pos)
    end do search_value
    ! }}}
  end function readvalue_int_array

  function readvalue_dp_2Darray(words,value,valuename) result(res)
    ! {{{ read 2D array of double precision real numbers. The implementation
    !     is not optimal because I create a 1D array first and then copy it
    !     to the 2D array. But I cannot figure out how to access multi-D
    !     array as 1D in Fortran in a compliant way (equivalence is not
    !     applicable to automatic and allocatable arrays, passing array bounds
    !     separately and accessing beyond bounds does not look compliant)
    logical :: res
    type(t_words), intent(in) :: words
    ! intent(inout) for 'value' because we do not want to destroy possible
    ! default assigned to 'value' before the call
    real(dp), dimension(:,:), intent(inout) :: value
    character(len=*), intent(in) :: valuename
    !
    real(dp), dimension(size(value)) :: value1d
    res=readvalue_dp_array(words,value1d,valuename)
    if ( res ) then
       value=reshape(value1d,(/ size(value,1),size(value,2) /))
    end if
    ! }}}
  end function readvalue_dp_2Darray

  function readvalue_int_2Darray(words,value,valuename) result(res)
    ! {{{ read 2D array of double precision real numbers. The implementation
    !     is not optimal because I create a 1D array first and then copy it
    !     to the 2D array. But I cannot figure out how to access multi-D
    !     array as 1D in Fortran in a compliant way (equivalence is not
    !     applicable to automatic and allocatable arrays, passing array bounds
    !     separately and accessing beyond bounds does not look compliant)
    logical :: res
    type(t_words), intent(in) :: words
    ! intent(inout) for 'value' because we do not want to destroy possible
    ! default assigned to 'value' before the call
    integer, dimension(:,:), intent(inout) :: value
    character(len=*), intent(in) :: valuename
    !
    integer, dimension(size(value)) :: value1d
    res=readvalue_int_array(words,value1d,valuename)
    if ( res ) then
       value=reshape(value1d,(/ size(value,1),size(value,2) /))
    end if
    ! }}}
  end function readvalue_int_2Darray

  function readsection(words,section,sectionname) result(res)
    ! {{{ extract named section from 'lines' to a new linked list 'section'
    logical :: res
    type(t_words), intent(in) :: words
    ! intent(inout) for 'section' because that allows me to properly
    ! deallocate the list if something non-empty was passed to the function;
    ! we have no destructors in this Fortran, so intent(out) could cause
    ! memory leak
    type(t_words), intent(inout) :: section
    character(len=*), intent(in) :: sectionname
    !
    type(t_words_iterator), pointer :: pos
    integer :: level
    !
    call clear(section)
    res=.false.
    level=0
    pos => begin(words)
    do while ( associated(pos) )
       if ( ( level == 0 ) .and. string_eq(pos%val,sectionname) ) exit
       level=level+level_adjustment(pos%val)
       call increment(pos)
    end do
    if ( associated(pos) ) then
       call increment(pos)
       if ( associated(pos) ) then
          if ( pos%val(1:1)==startsec ) then
             ! integrity checking in 'readConf' should insure that at this
             ! point there really is a whole section here
             res=.true.
             call increment(pos)
             level=0
             do while ( ( pos%val(1:1) /= endsec ) .or. ( level /= 0 ) )
                call push_back(section,pos%val)
                level=level+level_adjustment(pos%val)
                call increment(pos)
             end do
          else
             print '(1x,4a)', "Reading input: expected a section ", &
               "to follow '", sectionname, "' but got '", trim(pos%val), &
               "' instead."
          end if
       else
          print '(1x,4a)', "Reading input: expected a section ", &
               "to follow '", sectionname, "' but file already ended."
          stop 
       end if
    end if
    ! }}}
  end function readsection

  function readsection_and_erase(words,section,sectionname) result(res)
    ! {{{ extract named section from 'lines' to a new linked list 'section';
    !     the section is removed from 'words'
    logical :: res
    type(t_words), intent(inout) :: words
    ! intent(inout) for 'section' because that allows me to properly
    ! deallocate the list if something non-empty was passed to the function;
    ! we have no destructors in this Fortran, so intent(out) could cause
    ! memory leak
    type(t_words), intent(inout) :: section
    character(len=*), intent(in) :: sectionname
    !
    type(t_words_iterator), pointer :: pos
    integer :: level
    !
    call clear(section)
    res=.false.
    level=0
    pos => begin(words)
    do while ( associated(pos) )
       if ( ( level == 0 ) .and. string_eq(pos%val,sectionname) ) exit
       level=level+level_adjustment(pos%val)
       call increment(pos)
    end do
    if ( associated(pos) ) then
       !call increment(pos)
       call erase(words,pos)
       if ( associated(pos) ) then
          if ( pos%val(1:1)==startsec ) then
             ! integrity checking in 'readConf' should insure that at this
             ! point there really is a whole section here
             res=.true.
             !call increment(pos)
             call erase(words,pos)
             level=0
             do while ( ( pos%val(1:1) /= endsec ) .or. ( level /= 0 ) )
                call push_back(section,pos%val)
                level=level+level_adjustment(pos%val)
                !call increment(pos)
                call erase(words,pos)
             end do
             call erase(words,pos)  ! remove 'endsec'
          else
             print '(1x,4a)', "Reading input: expected a section ", &
               "to follow '", sectionname, "' but got '", trim(pos%val), &
               "' instead."
          end if
       else
          print '(1x,4a)', "Reading input: expected a section ", &
               "to follow '", sectionname, "' but file already ended."
          stop 
       end if
    end if
    ! }}}
  end function readsection_and_erase

  function haskeyword(words,keyword) result(res)
    ! {{{ test for presence of a keyword
    logical :: res
    type(t_words), intent(in) :: words
    character(len=*), intent(in) :: keyword
    !
    type(t_words_iterator), pointer :: pos
    integer :: level
    !
    res=.false.
    level=0
    pos => begin(words)
    do while ( associated(pos) )
       if ( ( level == 0 ) .and. string_eq(pos%val,keyword) ) then
          res=.true.
          exit
       end if
       level=level+level_adjustment(pos%val)
       call increment(pos)
    end do
    ! }}}
  end function haskeyword

  function string_eq(string1,string2)
    ! {{{ string comparison
    logical :: string_eq
    character(*), intent(in) :: string1, string2
    integer :: i, j
    i=index(string1,trim(string2))
    j=index(string2,trim(string1))
    if ( ( i > 0 ) .and. ( j > 0 ) ) then
       string_eq=.true.
    else
       string_eq=.false.
    end if
    ! }}}
  end function string_eq

  function level_adjustment(word) result(increment)
    ! {{{ check for section opener and closer, increment level accordingly
    character(len=*), intent(in) :: word
    integer :: increment
    increment=0
    if ( word(1:1)==startsec ) increment=increment+1
    if ( word(1:1)==endsec   ) increment=increment-1
    ! }}}
  end function level_adjustment

  subroutine write_default(test)
    ! {{{ used in 'showinfo_*' routines after non-advancing write, either
    !     just end the line or print "(default)" before that
    logical, intent(in) :: test
    if ( test ) then
       print *
    else
       print *, "  (default)"
    end if
    ! }}}
  end subroutine write_default
  
end module cfparser

! Local variables:
! folded-file: t
! End:
