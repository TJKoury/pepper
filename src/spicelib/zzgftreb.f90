!$Procedure ZZGFTREB ( Geometry finder: return body axes )

      subroutine zzgftreb ( body, axes )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     Return the values of the triaxial radii for any body in the
!     kernel pool.
!
! $ Required_Reading
!
!     NAIF_IDS
!     SPK
!
! $ Keywords
!
!     CONSTANTS
!
! $ Declarations

      implicit none

      integer(ip)          body
      real(wp)             axes ( 3 )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     BODY       I   NAIF ID code of body.
!     AXES       O   Length of axes of body (1,2,3, as defined below).
!
! $ Detailed_Input
!
!     BODY     is the NAIF ID code of the body for which the axes are
!              requested. Bodies are numbered according to the
!              standard NAIF numbering scheme described in the
!              required reading (naif_ids.req) document.
!
! $ Detailed_Output
!
!     AXES     are the lengths of the axes of the body, in km.
!
!                    AXES(1)  is the longest equatorial radius of
!                             the body. For satellites, this axis is
!                             typically pointed toward the primary
!                             planet.
!
!                    AXES(2)  is the shortest equatorial radius of
!                             the body.
!
!                    AXES(3)  is the polar radius of the body.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the RADII kernel pool data for body specified does not have
!         exactly 3 axes defined, the error SPICE(INVALIDCOUNT) is
!         signaled.
!
!     2)  If the RADII kernel pool data for body specified contains any
!         zero or negative values, the error SPICE(BADAXISLENGTH) is
!         signaled.
!
! $ Files
!
!     PCK data: triaxial radii for the target body must be loaded
!     into the kernel pool. Typically this is done by loading a
!     text PCK file via LDPOOL or a general kernel loader
!     such as FURNSH.
!
! $ Particulars
!
!     ZZGFTREB returns the lengths of the axes of BODY. It serves
!     as a wrapper to the kernel data access call, the confirms
!     return of reasonable values.
!
!     Appropriate SPK and PCK data must be available to the calling
!     program before this routine is called.for missing kernel data
!     or non physical values.
!
! $ Examples
!
!     The call
!
!         CALL ZZGFTREB ( 399,  VALUE )
!
!     returns the values associated with the variable 'BODY399_RADII',
!     for example,
!
!          VALUE(1) = 6378.140
!          VALUE(2) = 6378.140
!          VALUE(3) = 6356.755
!
! $ Restrictions
!
!     None.
!
! $ Literature_References
!
!     [1]  "Report of the IAU/IAG/COSPAR Working Group on Cartographic
!          Coordinates and Rotational Elements of the Planets and
!          Satellites: 1991", March 3, 1992.
!
! $ Author_and_Institution
!
!     J. Diaz del Rio    (ODC Space)
!     E.D. Wright        (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.1.0, 11-SEP-2021 (EDW) (JDR)
!
!        Corrected short error messages token string. Axes length
!        error is signaled on less-than or equal-to zero.
!
!        Edited the header to comply with NAIF standard. Moved SPK
!        required reading from $Literature_References to
!        $Required_Reading section.
!
!-    SPICELIB Version 1.0.0, 05-MAR-2003 (EDW)
!
!-&


! $ Index_Entries
!
!     Return the values of the triaxial radii
!
!-&


!
!     SPICELIB functions
!
      logical               return
      logical               failed

!
!     Local variables
!
      integer(ip)           i
      integer(ip)           n

!
!     Standard SPICE error handling.
!
      if ( return () ) then
         return
      end if

      call chkin  ( 'ZZGFTREB' )


!
!     Look up BODY radii in the kernel pool.
!
      call bodvcd ( body, 'RADII', 3, n, axes )

      if ( failed() ) then
         call chkout ( 'ZZGFTREB' )
         return
      end if

      if ( n .ne. 3 ) then

         call setmsg ( 'Only # axes were found  ' &
              //       'for ID #. Three axes expected.'    )
         call errint ( '#',   n                            )
         call errint ( '#',   body                         )
         call sigerr ( 'SPICE(INVALIDCOUNT)'               )
         call chkout ( 'ZZGFTREB'                          )
         return

      else

         do i = 1, 3

           if ( axes(i) .le. 0.0_wp ) then

               call setmsg ('Degenerate case. The # axis of body # ' &
               //           'is negative or zero.  Please check ' &
               //           'the text PCK file. You should fix the ' &
               //           '# component of the kernel pool ' &
               //           'variable  BODY#_RADII. '          )

               call errint ( '#', i                            )
               call errint ( '#', body                         )
               call errint ( '#', i                            )
               call errint ( '#', body                         )
               call sigerr ( 'SPICE(BADAXISLENGTH)'            )
               call chkout ( 'ZZGFTREB'                        )
               return

           end if

         end do

      end if

      call chkout ( 'ZZGFTREB' )
      return
      end
