!$Procedure      ZZEKTRLS ( EK tree, linear search )

      integer(ip) function zzektrls ( handle, tree, ival )
      use pepper_kinds, only: ip

! $ Abstract
!
!     Search an EK tree linearly to find a specified data value.  The
!     function returns the index at which the value is found.
!
! $ Required_Reading
!
!     EK
!
! $ Keywords
!
!     EK
!     PRIVATE
!
! $ Declarations

      include 'ektree.inc'

      integer(ip)           handle
      integer(ip)           tree
      integer(ip)           ival

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     HANDLE     I   File handle.
!     TREE       I   Tree root.
!     IVAL       I   Value to search for.
!
!     The function returns the lowest index at which the input value
!     is found, or zero if the value is not found.
!
! $ Detailed_Input
!
!     HANDLE         is a file handle of an EK open for read or write
!                    access.
!
!     TREE           is the root node of the tree to search.
!
!     IVAL           is the value to search for.
!
! $ Detailed_Output
!
!     The function returns the lowest index at which the input value
!     is found, or zero if the value is not found.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If HANDLE is invalid, the error will be diagnosed by routines
!         called by this routine.
!
!     2)  If TREE is invalid, strange errors may result.
!
!     3)  If an I/O error occurs while reading the indicated file,
!         the error will be diagnosed by routines called by this
!         routine.
!
! $ Files
!
!     See the EK Required Reading for a discussion of the EK file
!     format.
!
! $ Particulars
!
!     This searches a tree for a specified value.  It is an approximate
!     inverse of ZZEKTRLK.  However, ZZEKTRLK operates in logarithmic
!     time (as a function of the tree's size), while this function
!     plods along in linear time.
!
! $ Examples
!
!     See ZZEKRP2N.
!
! $ Restrictions
!
!     None.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman   (JPL)
!
! $ Version
!
!-    Beta Version 1.0.0, 11-OCT-1995 (NJB)
!
!-&

!
!     Non-SPICELIB functions
!
      integer(ip)           zzektrsz
!
!     Local variables
!
      integer(ip)           i
      integer(ip)           n
      integer(ip)           value

!
!     Use discovery check-in.
!
      zzektrls  =  0
      n         =  zzektrsz ( handle, tree )

      do i = 1, n

         call zzektrdp ( handle, tree, i, value )

         if ( ival .eq. value ) then
            zzektrls = i
            return
         end if

      end do

      return
      end
