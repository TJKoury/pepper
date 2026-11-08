!$Procedure TWOVEC ( Two vectors defining an orthonormal frame )

      subroutine twovec ( axdef, indexa, plndef, indexp, mout )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     Find the transformation to the right-handed frame having a
!     given vector as a specified axis and having a second given
!     vector lying in a specified coordinate plane.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     AXES
!     FRAME
!     ROTATION
!     TRANSFORMATION
!
! $ Declarations

      implicit none

      real(wp)           axdef  (   3 )
      integer(ip)        indexa
      real(wp)           plndef (   3 )
      integer(ip)        indexp
      real(wp)           mout   ( 3,3 )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  -------------------------------------------------
!     AXDEF      I   Vector defining a principal axis.
!     INDEXA     I   Principal axis number of AXDEF (X=1, Y=2, Z=3).
!     PLNDEF     I   Vector defining (with AXDEF) a principal plane.
!     INDEXP     I   Second axis number (with INDEXA) of principal
!                    plane.
!     MOUT       O   Output rotation matrix.
!
! $ Detailed_Input
!
!     AXDEF    is a vector defining one of the principle axes of a
!              coordinate frame.
!
!     INDEXA   is a number that determines which of the three
!              coordinate axes contains AXDEF.
!
!              If INDEXA is 1 then AXDEF defines the X axis of the
!              coordinate frame.
!
!              If INDEXA is 2 then AXDEF defines the Y axis of the
!              coordinate frame.
!
!              If INDEXA is 3 then AXDEF defines the Z axis of the
!              coordinate frame.
!
!     PLNDEF   is a vector defining (with AXDEF) a principal plane of
!              the coordinate frame. AXDEF and PLNDEF must be
!              linearly independent.
!
!     INDEXP   is the second axis of the principal frame determined
!              by AXDEF and PLNDEF.  INDEXA, INDEXP must be different
!              and be integers from 1 to 3.
!
!              If INDEXP is 1, the second axis of the principal
!              plane is the X-axis.
!
!              If INDEXP is 2, the second axis of the principal
!              plane is the Y-axis.
!
!              If INDEXP is 3, the second axis of the principal plane
!              is the Z-axis.
!
! $ Detailed_Output
!
!     MOUT     is a rotation matrix that transforms coordinates given
!              in the input frame to the frame determined by AXDEF,
!              PLNDEF, INDEXA and INDEXP.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If INDEXA or INDEXP is not in the set {1,2,3}, the error
!         SPICE(BADINDEX) is signaled.
!
!     2)  If INDEXA and INDEXP are the same, the error
!         SPICE(UNDEFINEDFRAME) is signaled.
!
!     3)  If the cross product of the vectors AXDEF and PLNDEF is zero,
!         the error SPICE(DEPENDENTVECTORS) is signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     Given two linearly independent vectors there is a unique
!     right-handed coordinate frame having:
!
!        AXDEF lying along the INDEXA axis.
!
!        PLNDEF lying in the INDEXA-INDEXP coordinate plane.
!
!     This routine determines the transformation matrix that transforms
!     from coordinates used to represent the input vectors to the
!     the system determined by AXDEF and PLNDEF. Thus a vector
!     (x,y,z) in the input coordinate system will have coordinates
!
!                     t
!        MOUT* (x,y,z)
!
!     in the frame determined by AXDEF and PLNDEF.
!
! $ Examples
!
!     The rotation matrix TICC from inertial to Sun-Canopus
!     (celestial) coordinates is found by the call
!
!        CALL TWOVEC (Sun vector, 3, Canopus vector, 1, TICC)
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
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!     W.M. Owen          (JPL)
!     W.L. Taber         (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.2.0, 06-JUL-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard. Removed
!        unnecessary $Revisions section.
!
!-    SPICELIB Version 1.1.0, 31-AUG-2005 (NJB)
!
!        Updated to remove non-standard use of duplicate arguments
!        in VSCL call.
!
!-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (WMO) (WLT)
!
!-&


! $ Index_Entries
!
!     define an orthonormal frame from two vectors
!
!-&


!
!     SPICELIB functions
!
      logical               return

!
!     Local variables
!
      real(wp)              mtemp  ( 3, 3 )

      integer(ip)           i1
      integer(ip)           i2
      integer(ip)           i3
      integer(ip)           seqnce ( 5 )

!
!     Saved variables
!
      save                  seqnce

!
!     Initial values
!
      data                  seqnce  / 1, 2, 3, 1, 2 /

!
!     Standard SPICE error handling
!
      if ( return() ) then
         return
      else
         call chkin ( 'TWOVEC' )
      end if

!
!     Check for obvious bad inputs.
!
      if (      ( max(indexp,indexa) .gt. 3 ) &
           .or. ( min(indexp,indexa) .lt. 1 ) ) then

         call setmsg ( 'The definition indexes must lie in the range '// &
                       'from 1 to 3.  The value of INDEXA was #. '    // &
                       'The value of INDEXP was #. '                  )

         call errint ( '#', indexa  )
         call errint ( '#', indexp  )
         call sigerr ( 'SPICE(BADINDEX)' )
         call chkout (     'TWOVEC' )
         return

      else if   ( indexa .eq. indexp )  then

         call setmsg ( 'The values of INDEXA and INDEXP were the '    // &
                       'same, namely #.  They are required to be '    // &
                       'different.'                                   )

         call errint ( '#', indexa  )
         call sigerr ( 'SPICE(UNDEFINEDFRAME)' )
         call chkout (     'TWOVEC' )
         return

      end if

!
!     Get indices for right-handed axes
!
!     First AXDEF ...
!
      i1 = indexa
!
!     ... then the other two.
!
      i2 = seqnce ( indexa + 1 )
      i3 = seqnce ( indexa + 2 )

!
!     Row I1 contains normalized AXDEF (store in columns for now)
!
      call vhat ( axdef, mout(1,i1) )

!
!     Obtain rows I2 and I3 using cross products.  Which order to use
!     depends on whether INDEXP = I2 (next axis in right-handed order)
!     or INDEXP = I3 (previous axis in right-handed order).
!
      if ( indexp .eq. i2 ) then

         call ucrss ( axdef,      plndef,        mout(1,i3) )
         call ucrss ( mout(1,i3), axdef,         mout(1,i2) )

      else

         call ucrss ( plndef,     axdef,         mout(1,i2) )
         call ucrss ( axdef,      mout(1,i2),    mout(1,i3) )

      end if

!
!     Finally, check to see that we actually got something non-zero
!     in one of the one columns of MOUT(1,I2) and MOUT(1,I3) (we need
!     only check one of them since they are related by a cross product).
!
      if (       ( mout(1,i2) .eq. 0.0_wp ) &
           .and. ( mout(2,i2) .eq. 0.0_wp ) &
           .and. ( mout(3,i2) .eq. 0.0_wp ) ) then

         call setmsg ( 'The input vectors AXDEF and PLNDEF are '      // &
                       'linearly dependent.'                          )
         call sigerr ( 'SPICE(DEPENDENTVECTORS)' )

      end if
!
!     Transpose MOUT.
!
      call xpose ( mout,     mtemp )
      call moved ( mtemp, 9, mout  )

      call chkout ( 'TWOVEC' )
      return
      end
