! ==========================================================================
! 
! Doubly linked list and basic operations on it (preprocessor template).
!
! Copyright (C) 2009 Jindrich Kolorenc
! Copyright (C) 2001 Simon Tatham [ code used in subroutine merge_sort() ]
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
!
! Inspiration by the C++ STL list<T> container is rather obvious (and
! intentional too). Only bare-bones functionality of list<T> is implemented
! in this Fortran version.
!
! This file is a "preprocessor template". To make a list of double
! precision numbers, compile the following piece of code. It is
! assumed that the constant dp is defined in module const. Define
! DATA_TYPE_SORTABLE only if relational operators are defined for the
! given DATA_TYPE.
!
! ------------- beginning driver code -------------
!
! #define MODULE_NAME           dp_list
! #define LIST_NAME             t_dp_list
! #define ITERATOR_NAME         t_dp_list_iterator
! #define DATA_TYPE             real(dp)
! #define DATA_TYPE_MODULE_NAME const
! #define DATA_TYPE_SORTABLE
! #include "list_template.F90.inc"
!
! ---------------- end driver code ----------------
!
! This module could be reduced to a conforming F90 code, if the following
! constructs are removed:
!
!   * [F95] declaration of some of the procedures as pure or elemental
!
!   * [F95] default initialization of pointer components of derived types
!     ( using ' => null() ' ), this relaxes the need to call init() on
!     each list before first use
!
!   * [F2003] private/public specifications for individual components of
!     derived data types (ITERATOR_NAME here)
!
!   * [F2003] intent specifications of pointer dummy arguments. Some of them
!     are 'inout' (hence not really important), but some are 'in' (one cannot
!     overload == operator for ITERATOR_NAME without specifying intent(in).
!
!   * [F2003] finalization of LIST_NAME, this relaxes the need to call
!     clear() every time the list goes out of context
!
! The F2003 features are not compiled in unless preprocessor macros
! HAVE_F2003 and/or HAVE_FINALIZATION are defined. Finalization is separated
! since only a few compilers can handle it.
!
! ==========================================================================

!#define HAVE_F2003
!#define HAVE_FINALIZATION

#ifdef HAVE_F2003
#define DTYPE_COMP_PRIV  , private
#define DTYPE_COMP_PUB   , public
#define PTR_INTENT_IN    , intent(in)
#define PTR_INTENT_INOUT , intent(inout)
#else
#define DTYPE_COMP_PRIV
#define DTYPE_COMP_PUB
#define PTR_INTENT_IN
#define PTR_INTENT_INOUT
#endif

module MODULE_NAME

#ifdef DATA_TYPE_MODULE_NAME
  use DATA_TYPE_MODULE_NAME
#endif

  implicit none
  private
  
  public :: LIST_NAME, ITERATOR_NAME
  public :: init, clear
  public :: push_back, pop_back, push_front, pop_front
  public :: insert, erase
  public :: increment, decrement
  public :: begin, end
  public :: get_value, size
#ifdef DATA_TYPE_SORTABLE
  public :: sort, unique
#endif
#ifdef HAVE_F2003
  public :: operator(==)
#endif

  interface init
     module procedure init_local
  end interface

  interface clear
     module procedure clear_local
  end interface  

  interface push_back
     module procedure push_back_local
  end interface

  interface pop_back
     module procedure pop_back_local
  end interface

  interface push_front
     module procedure push_front_local
  end interface

  interface pop_front
     module procedure pop_front_local
  end interface

  interface insert
     module procedure insert_local
  end interface

  interface erase
     module procedure erase_local
  end interface

  interface increment
     module procedure increment_local
  end interface

  interface decrement
     module procedure decrement_local
  end interface

  interface begin
     module procedure begin_local
  end interface

  interface end
     module procedure end_local
  end interface

  interface get_value
     module procedure get_value_local
  end interface

  interface size
     module procedure size_local
  end interface

#ifdef DATA_TYPE_SORTABLE

  interface sort
     module procedure merge_sort
  end interface

  interface unique
     module procedure unique_local
  end interface
  
! DATA_TYPE_SORTABLE
#endif

  ! it looks like this cannot be done in F95, arguments for the overloader of
  ! '==' must have intent(in), but pointers cannot have any intent specified
  ! at all in F95
#ifdef HAVE_F2003
  interface operator(==)
     module procedure eq_iters_local
  end interface
#endif

  type ITERATOR_NAME
     type(ITERATOR_NAME), pointer DTYPE_COMP_PRIV :: prev => null()
     type(ITERATOR_NAME), pointer DTYPE_COMP_PRIV :: next => null()
     DATA_TYPE DTYPE_COMP_PUB :: val
  end type ITERATOR_NAME  
  
  type LIST_NAME
     private
     type(ITERATOR_NAME), pointer :: first => null()
     type(ITERATOR_NAME), pointer :: last => null()
     integer :: length = 0
#ifdef HAVE_FINALIZATION
   contains
     final :: clear_local
#endif
  end type LIST_NAME

contains

  elemental subroutine init_local(list)
    ! {{{ initialize the list before the first use; this is only needed in F90
    !     that does not allow default initialization of pointer components of
    !     derived types
    type(LIST_NAME), intent(inout) :: list
    nullify(list%first,list%last)
    list%length=0
    ! }}}
  end subroutine init_local

  pure function get_value_local(iterator) result(val)
    ! {{{ return the value the iterator points to; note that one can access
    !     the value also directly, which does not involve any data copy
    DATA_TYPE :: val
    type(ITERATOR_NAME), pointer PTR_INTENT_IN :: iterator
    val=iterator%val
    ! }}}
  end function get_value_local

  pure subroutine push_back_local(list,val)
    ! {{{ add an element at the end of the list
    type(LIST_NAME), intent(inout) :: list
    DATA_TYPE, intent(in) :: val 
    type(ITERATOR_NAME), pointer :: element
    allocate(element)
    nullify(element%next,element%prev)
    if ( associated(list%last) ) then
       element%prev => list%last
       list%last%next => element
    end if
    element%val = val
    list%last => element
    if ( .not.associated(list%first) ) list%first => element
    list%length=list%length+1
    ! }}}
  end subroutine push_back_local

  elemental subroutine pop_back_local(list)
    ! {{{ remove the last element from the list
    type(LIST_NAME), intent(inout) :: list
    type(ITERATOR_NAME), pointer :: element
    if ( associated(list%last) ) then
       element => list%last
       if ( associated(element%prev) ) then
          list%last => element%prev
          nullify(list%last%next)
       else
          nullify(list%last)
          nullify(list%first)
       end if
       deallocate(element)
       list%length=list%length-1
    end if
    ! }}}
  end subroutine pop_back_local

  pure subroutine push_front_local(list,val)
    ! {{{ add an element to the beginning of the list
    type(LIST_NAME), intent(inout) :: list
    DATA_TYPE, intent(in) :: val 
    type(ITERATOR_NAME), pointer :: element
    allocate(element)
    nullify(element%next,element%prev)
    if ( associated(list%first) ) then
       element%next => list%first
       list%first%prev => element
    end if
    element%val = val
    list%first => element
    if ( .not.associated(list%last) ) list%last => element
    list%length=list%length+1
    ! }}}
  end subroutine push_front_local

  elemental subroutine pop_front_local(list)
    ! {{{ remove the first element from the list
    type(LIST_NAME), intent(inout) :: list
    type(ITERATOR_NAME), pointer :: element
    if ( associated(list%first) ) then
       element => list%first
       if ( associated(element%next) ) then
          list%first => element%next
          nullify(list%first%prev)
       else
          nullify(list%last)
          nullify(list%first)
       end if
       deallocate(element)
       list%length=list%length-1
    end if
    ! }}}
  end subroutine pop_front_local

  pure subroutine insert_local(list,iterator,val)
    ! {{{ insert a new element right before the iterator provided;
    !     on output, the iterator still points to the same element it did
    !     before the call
    type(LIST_NAME), intent(inout) :: list
    type(ITERATOR_NAME), pointer PTR_INTENT_INOUT :: iterator
    DATA_TYPE, intent(in) :: val
    type(ITERATOR_NAME), pointer :: element
    if ( associated(iterator) ) then
       allocate(element)
       nullify(element%next,element%prev)
       element%val=val
       if ( associated(iterator%prev) ) then
          ! iterator does not point to the first element of the list
          iterator%prev%next => element
          element%prev => iterator%prev
          iterator%prev => element
          element%next => iterator
          list%length=list%length+1
       else
          ! iterator points to the first element of the list
          call push_front(list,val)
       end if
          
    end if
    ! }}}
  end subroutine insert_local

  pure subroutine erase_local(list,iterator)
    ! {{{ remove the element the iterator points to; on output, the iterator
    !     points to the element following the erased one, or is not associated
    !     if the very last element was erased
    type(LIST_NAME), intent(inout) :: list
    type(ITERATOR_NAME), pointer PTR_INTENT_INOUT :: iterator
    type(ITERATOR_NAME), pointer :: tmp_iter
    if ( associated(iterator) ) then
       tmp_iter => iterator
       ! somewhere in the middle, the easy case
       if ( associated(tmp_iter%prev) .and. associated(tmp_iter%next) ) then
          iterator%prev%next => iterator%next
          iterator%next%prev => iterator%prev
          iterator => iterator%next
       end if
       ! removal of the first element (pop_front could be used)
       if ( .not.associated(tmp_iter%prev) &
            & .and. associated(tmp_iter%next) ) then
           nullify(iterator%next%prev)
          list%first => iterator%next
          iterator => iterator%next
       end if
       ! removal of the last element (pop_back could be used)
       if ( associated(tmp_iter%prev) &
            & .and. .not.associated(tmp_iter%next) ) then
          list%last => iterator%prev
          nullify(iterator%prev%next,iterator)
       end if
       ! removal of the only remaining element in the list (either pop_* would
       ! work here too)
       if ( .not.associated(tmp_iter%prev) &
            & .and. .not.associated(tmp_iter%next) ) then
          nullify(list%first,list%last,iterator)
       end if
       ! after all reconnecting is done, here comes the actual removal
       list%length=list%length-1
       deallocate(tmp_iter)
    end if
    ! }}}
  end subroutine erase_local

  pure subroutine increment_local(iterator)
    ! {{{ shift iterator to the next element, becomes deassociated if a move
    !     past the last element is attempted
    type(ITERATOR_NAME), pointer PTR_INTENT_INOUT :: iterator
    if ( associated(iterator) ) then
       if ( associated(iterator%next) ) then
          iterator => iterator%next
       else
          nullify(iterator)
       end if
    end if
    ! }}}
  end subroutine increment_local

  pure subroutine decrement_local(iterator)
    ! {{{ shift iterator to the previous element, becomes deassociated if
    !     a move before the first element is attempted
    type(ITERATOR_NAME), pointer PTR_INTENT_INOUT :: iterator
    if ( associated(iterator) ) then
       if ( associated(iterator%prev) ) then
          iterator => iterator%prev
       else
          nullify(iterator)
       end if
    end if
    ! }}}
  end subroutine decrement_local
 
  function begin_local(list)
    ! {{{ return iterator pointing to the first element in the list;
    !     cannot be pure because dummy argument cannot be a pointer target
    type(ITERATOR_NAME), pointer :: begin_local
    type(LIST_NAME), intent(in) :: list
    if ( associated(list%first) ) then
       begin_local => list%first
    else
       nullify(begin_local)
    end if
    ! }}}
  end function begin_local

  function end_local(list)
    ! {{{ return iterator pointing to the last element in the list;
    !     cannot be pure because dummy argument cannot be a pointer target
    type(ITERATOR_NAME), pointer :: end_local
    type(LIST_NAME), intent(in) :: list
    if ( associated(list%last) ) then
       end_local => list%last
    else
       nullify(end_local)
    end if
    ! }}}
  end function end_local  

  elemental subroutine clear_local(list)
    ! {{{ remove all elements from the list; should return the list into the
    !     state in which it was right after init(list)
    type(LIST_NAME), intent(inout) :: list
    do while ( associated(list%last) )
       call pop_back(list)
    end do
    !print *, "An instance of a list finalized."
    ! }}}
  end subroutine clear_local

  pure function eq_iters_local(iterator1,iterator2)
    ! {{{ test whether two iterators point to the same element, this is
    !     meant to overload the == operator
    logical :: eq_iters_local
    type(ITERATOR_NAME), pointer PTR_INTENT_IN :: iterator1
    type(ITERATOR_NAME), pointer PTR_INTENT_IN :: iterator2
    if ( associated(iterator1,iterator2) ) then
       eq_iters_local=.true.
    else
       eq_iters_local=.false.
    end if
    ! }}}
  end function eq_iters_local

  elemental function size_local(list)
    ! {{{ return the number of elements stored in the list (its length)
    !integer(i4b) :: size_local
    integer :: size_local
    type(LIST_NAME), intent(in) :: list
    size_local=list%length
    ! }}}
  end function size_local

#ifdef DATA_TYPE_SORTABLE

  elemental subroutine merge_sort(list)
    ! {{{ on output, the list is sorted in ascending order
    !
    !     Sorting algorithm is merge sort, its running time is O(N log N) on
    !     average as well as for the worst case. For a description of the
    !     algorithm see
    !      http://www.chiark.greenend.org.uk/~sgtatham/algorithms/listsort.html
    !     (or Google).
    !
    !     The code itself is
    !      http://www.chiark.greenend.org.uk/~sgtatham/algorithms/listsort.c
    !     translated to Fortran with as little changes as possible. The
    !     "fortranization" was surprisingly straightforward.
    !
    type(LIST_NAME), intent(inout) :: list
    type(ITERATOR_NAME), pointer :: p, q, e, tail, slist
    integer::  insize, nmerges, psize, qsize, i

    ! finish immediately, if there is nothing to do
    if ( ( size(list) == 0 ) .or. ( size(list) == 1 ) ) return

    insize = 1
    ! begin() cannot be pure, it would prevent merge_sort() from being declared
    ! as elemental
    !slist => begin(list)
    slist => list%first    ! list%first surely associated since size(list) > 0

    recursive: do

       p => slist
       nullify(slist,tail)
       nmerges = 0       ! count number of merges done in this pass

       do while ( associated(p) )
          nmerges=nmerges+1
          ! jump over p which is 'insize' long
          q => p
          psize = 0
          do i = 1, insize
             psize=psize+1
             !call increment(q)
             q => q%next
             if ( .not.associated(q) ) exit
          end do

          qsize = insize

          ! we will be merging list p of length 'insize' and q of the same
          ! length, which are located one after another in the previous list
          ! (the result of the preceding pass); of course they can be shorter
          ! if we don't have enough elements to fill them both
          do while ( ( psize > 0 ) .or. &
               & ( ( qsize > 0 ) .and. associated(q) ) )

             ! decide whether next element of merge comes from p or q
             if ( psize == 0 ) then
                ! p is empty; e must come from q.
                e => q
                !call increment(q)
                q => q%next
                qsize=qsize-1
             else 
                if ( ( qsize == 0 ) .or. .not.associated(q) ) then
                   ! q is empty; e must come from p.
                   e => p
                   !call increment(p)
                   p => p%next
                   psize=psize-1
                else 
                   if ( p%val <= q%val ) then
                      ! first element of p is smaller (or same);
                      ! e must come from p.
                      e => p
                      !call increment(p)
                      p => p%next
                      psize=psize-1
                   else
                      ! first element of q is smaller; e must come from q.
                      e => q
                      !call increment(q)
                      q => q%next
                      qsize=qsize-1
                   end if
                end if
             end if

             ! add the next element to the merged list
             if ( associated(tail) ) then
                tail%next => e
             else
                slist => e
             end if
             e%prev => tail  ! our list is linked also backwards
             tail => e

          end do  ! while [ ...merging p and q ]

          ! we have jumped 'insize' times in p as well as in q; the next p
          ! starts just after q
          p => q

       end do ! while [ ...associated(p) ]

       nullify(tail%next)

       ! if we have done only one merge, we're finished (insize was equal or
       ! larger half the list length)
       if ( nmerges <= 1 ) exit recursive

       ! otherwise repeat, merging sub-lists twice the size
       insize=insize*2

    end do recursive  ! outermost loop

    ! update our list 'envelope'
    list%first => slist
    list%last => tail

    ! }}}
  end subroutine merge_sort

  elemental subroutine unique_local(list)
    ! {{{ remove _consecutive_ duplicate values
    type(LIST_NAME), intent(inout) :: list
    type(ITERATOR_NAME), pointer :: p
    DATA_TYPE :: prev_val

    ! finish immediately, if there is nothing to do
    if ( ( size(list) == 0 ) .or. ( size(list) == 1 ) ) return

    ! begin() cannot be pure, it would prevent unique() from being declared
    ! as elemental
    !p => begin(list)
    p => list%first    ! list%first surely associated since size(list) > 0
    prev_val=p%val
    call increment(p)
    do while ( associated(p) )
       if ( p%val == prev_val ) then
          call erase(list,p)
       else
          prev_val=p%val
          call increment(p)
       end if
    end do
    ! }}}
  end subroutine unique_local

! DATA_TYPE_SORTABLE
#endif


end module MODULE_NAME

! Local variables:
! folded-file: t
! End:
