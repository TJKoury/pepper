!$Procedure VHATIP ( "V-Hat", 3-d unit vector along V, in place )

      subroutine vhatip ( v )
      use pepper_kinds, only: wp

! $ Abstract
!
!     Scale a three-dimensional vector to unit length.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     VECTOR
!
! $ Declarations

      implicit none

      real(wp)           v ( 3 )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     V         I-O  Vector to be normalized/unit vector.
!
! $ Detailed_Input
!
!     V        is any double precision, 3-dimensional vector. If this
!              vector is the zero vector, this routine will detect it,
!              and will not attempt to divide by zero.
!
! $ Detailed_Output
!
!     V        is the unit vector in the direction of the input vector.
!              If on input V represents the zero vector, then V will be
!              returned as the zero vector.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     Error free.
!
!     1)  The zero vector is returned if the input value of V is the
!         zero vector.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine is provided for situation where it is convenient
!     to scale a vector to unit length in place rather than store
!     the result in a separate variable. Note that the call
!
!        CALL VHAT ( V, V )
!
!     is not permitted by the ANSI Fortran 77 standard; this routine
!     can be called instead to achieve the same result.
!
!     VHATIP determines the magnitude of V and then, if the magnitude
!     is non-zero, divides each component of V by the magnitude. This
!     process is highly stable over the whole range of 3-dimensional
!     vectors.
!
! $ Examples
!
!     The following table shows how selected vectors are mapped to
!     unit vectors
!
!     V on input            V on output
!     ------------------    ------------------
!     (5, 12, 0)            (5/13, 12/13, 0)
!     (1D-7, 2D-7, 2D-7)    (1/3, 2/3, 2/3)
!
! $ Restrictions
!
!     1)  There is no known case whereby floating point overflow may
!         occur. Thus, no error recovery or reporting scheme is
!         incorporated into this subroutine.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!     H.A. Neilan        (JPL)
!     W.M. Owen          (JPL)
!     W.L. Taber         (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.2.0, 20-AUG-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.1.0, 01-SEP-2005 (NJB) (HAN) (WMO) (WLT)
!
!-&


! $ Index_Entries
!
!     unitize a 3-dimensional vector in place
!
!-&


!
!     SPICELIB functions
!
      real(wp)              vnorm

!
!     Local variables
!
      real(wp)              vmag


!
!     Obtain the magnitude of V.
!
      vmag = vnorm ( v )

!
!     If VMAG is nonzero, then normalize. Note that this process is
!     numerically stable: overflow could only happen if VMAG were
!     small, but this could only happen if each component of V1 were
!     small. In fact, the magnitude of any vector is never less than
!     the magnitude of any component.
!
      if ( vmag .gt. 0.0_wp ) then

         v(1) = v(1) / vmag
         v(2) = v(2) / vmag
         v(3) = v(3) / vmag

      else

         v(1) = 0.0_wp
         v(2) = 0.0_wp
         v(3) = 0.0_wp

      end if

      return
      end
