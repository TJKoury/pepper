!$Procedure      ZZEKSTYP ( EK, determine segment type )

      integer(ip) function zzekstyp ( ncols, cdscrs )
      use pepper_kinds, only: ip

! $ Abstract
!
!     Determine the type of segment required to support a specified
!     set of columns.
!
! $ Required_Reading
!
!     EK
!
! $ Keywords
!
!     EK
!
! $ Declarations

      include 'ekcoldsc.inc'
      include 'ekglimit.inc'

      integer(ip)           ncols
      integer(ip)           cdscrs ( cdscsz, * )

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     NCOLS      I   Number of columns in the segment.
!     CDSCRS     I   Descriptors of columns.
!
!     The function returns the type of segment that is compatible with
!     the input column descriptors.
!
! $ Detailed_Input
!
!
!     NCOLS          is the number of columns in a new segment.
!
!     CDSCRS         is an array of column descriptors:  the Ith
!                    descriptor applies to the Ith column in the
!                    segment.
!
! $ Detailed_Output
!
!     The function returns the type of segment that is compatible with
!     the input column descriptors.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If NCOLS is non-positive or greater than the maximum allowed
!         number MXCLSG, the error SPICE(INVALIDCOUNT) is signaled.
!
!     2)  If the input column descriptors do not contain compatible
!         attributes, the error SPICE(BADATTRIBUTES) will be signaled.
!
! $ Files
!
!     See the EK Required Reading for a discussion of the EK file
!     format.
!
! $ Particulars
!
!     This routine determines the appropriate segment type to contain
!     a specified set of columns.  Currently, there are two segment
!     types.  The first type accommodates column classes 1 through 6;
!     the second type accommodates column classes 7 through 9.  The
!     latter set of column classes are `fixed_count' classes:  a column
!     in one of these classes may not have entries added or deleted
!     after the column is created.  Fixed and variable count columns
!     may not coexist in the same segment.
!
! $ Examples
!
!     See EKBSEG.
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
!-    SPICELIB Version 1.0.1, 03-OCT-2021 (NJB)
!
!        Corrected typos in comments.
!
!-    Beta Version 1.0.0, 06-NOV-1995 (NJB)
!
!-&

!
!     SPICELIB functions
!
      logical               return

!
!     Local variables
!
      integer(ip)           i

      logical               fixed
      logical               var

!
!     Standard SPICE error handling.
!
      if ( return () ) then
         zzekstyp = 0
         return
      else
         call chkin ( 'ZZEKSTYP' )
      end if

!
!     FIXED and VAR indicate whether we've seen any fixed or variable
!     column classes so far.
!
      fixed  =  .false.
      var    =  .false.

      do i = 1, ncols

         if (       ( cdscrs(clsidx,i) >= 1 ) &
              .and. ( cdscrs(clsidx,i) <= 6 )  ) then

            var   = .true.

         else if (       ( cdscrs(clsidx,i) >= 7 ) &
                   .and. ( cdscrs(clsidx,i) <= 9 )  ) then

            fixed = .true.

         end if

      end do


      if (  var  .and. ( .not. fixed )  ) then

         zzekstyp = 1

      else if (  fixed  .and. ( .not. var )  ) then

         zzekstyp = 2

      else

         zzekstyp = 0

         call setmsg ( 'Column set contains a mixture of variable ' // &
                       'and fixed-count columns.  Segments must '   // &
                       'contain all variable or all fixed count '   // &
                       'columns.'                                    )
         call sigerr ( 'SPICE(BADATTRIBUTES)'                        )
         call chkout ( 'ZZEKSTYP'                                    )
         return

      end if


      call chkout ( 'ZZEKSTYP' )
      return
      end
