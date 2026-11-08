!$Procedure ZZRTNMAT ( RTN transformation matrix )

      subroutine zzrtnmat ( v, m )

! $ Abstract
!
!     Given a vector, return a transformation matrix that maps from the
!     vector's base reference frame to the RTN
!     (radial-tangential-normal) frame associated with the vector.
!
! $ Required_Reading
!
!     ROTATION
!
! $ Keywords
!
!     FRAMES
!     MATRIX
!     ROTATION
!
! $ Declarations

      implicit none

      double precision      v ( 3 )
      double precision      m ( 3, 3 )


! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     V          I   A 3-dimensional vector.
!     M          O   Base frame to RTN frame rotation matrix.
!
! $ Detailed_Input
!
!     V              is any vector that does not lie on the Z-axis
!                    of the reference frame relative to which the
!                    vector is expressed: at least one of V's X or
!                    Y components must be non-zero.
!
! $ Detailed_Output
!
!     M              is a rotation matrix that transforms vectors
!                    from the base frame of V---that is, the reference
!                    frame relative to which V is expressed---to
!                    the RTN (radial, tangential, normal) frame
!                    defined by V.
!
!                    The basis vectors of the RTN frame are defined
!                    as follows:
!
!                       Axis 1: radial direction R. This axis is
!                               parallel to V.
!
!                       Axis 2: tangential direction T. This axis
!                               is parallel to Z x V, where Z is
!                               the third axis of V's base frame.
!
!                       Axis 3: normal direction N. This axis is
!                               parallel to R x T.
!
!                   The unit vectors R, T, N are, respectively, the
!                   first, second and third rows of M.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1) If the input vector V has X and Y components equal to zero,
!        the error SPICE(DEGENERATECASE) is signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     The RTN frame supported this routine is a generalization of
!     the frame defined by a solar system object's spin axis and
!     and a position vector (often the position of a spacecraft
!     relative to the center of the object).
!
!     If the base frame of the input vector V is the body-fixed,
!     body-centered planetocentric frame associated with a solar system
!     object such as a planet or satellite, then the R, T, N directions
!     correspond to the "up", "East," and "North" directions at the
!     location indicated by V.
!
! $ Examples
!
!     1) Get the RTN transformation matrix for the vector ( 1, 0, 1 ):
!
!
!           IMPLICIT NONE
!
!           DOUBLE PRECISION      V ( 3 )
!           DOUBLE PRECISION      M ( 3, 3 )
!           INTEGER               I
!           INTEGER               J
!
!           CALL VPACK    ( 1.D0, 0.D0, 1.D0, V )
!
!           CALL ZZRTNMAT ( V, M )
!
!           DO I = 1, 3
!              WRITE(*,'(3E15.7)') ( M(I,J), J = 1, 3 )
!           END DO
!
!           END
!
!        When this program was executed on a PC/Linux/g77 system, the
!        output was
!
!            0.7071068E+00  0.0000000E+00  0.7071068E+00
!            0.0000000E+00  0.1000000E+01  0.0000000E+00
!           -0.7071068E+00  0.0000000E+00  0.7071068E+00
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
!-    SPICELIB Version 1.0.0, 05-MAR-2009 (NJB)
!
!-&

! $ Index_Entries
!
!     transformation to radial, tangential, normal frame
!     transformation to rtn frame
!
!-&

!
!     SPICELIB functions
!
      logical               return

!
!     Local variables
!
!     Internally, we're going to use the more
!     descriptive names EAST for the "tangential"
!     direction and NORTH for the "normal" direction.
!
      double precision      east   ( 3 )
      double precision      lon
      double precision      north  ( 3 )
      double precision      rad    ( 3 )
      double precision      vlon   ( 3 )
      double precision      z ( 3 )

      integer               i

!
!     Saved variables
!
      save                  z

!
!     Initial values
!
      data                  z / 0.d0, 0.d0, 1.d0 /

!
!     Use discovery check-in. Just test the RETURN status.
!
      if ( return() ) then
         return
      end if


      if (  ( v(1) .eq. 0.d0 ) .and. ( v(2) .eq. 0.d0 )  ) then

         call cleard ( 9, m )

         call chkin  ( 'ZZRTNMAT'                              )
         call setmsg ( 'Input vector (# # #) lies on Z-axis; ' &
         //            'tangential and normal directions are ' &
         //            'undefined.'                            )
         call errdp  ( '#', v(1)                               )
         call errdp  ( '#', v(2)                               )
         call errdp  ( '#', v(3)                               )
         call sigerr ( 'SPICE(DEGENERATECASE)'                 )
         call chkout ( 'ZZRTNMAT'                              )
         return

      else
!
!        The two-argument arctangent function gives us a
!        robust way of determining the longitude of V, even
!        when the magnitude of V is very small.
!
         lon = datan2 ( v(2), v(1) )

!
!        Let VLON be a unit vector in the x-y plane whose
!        longitude is LON.
!
         vlon(1) = cos( lon )
         vlon(2) = sin( lon )
         vlon(3) = 0.d0

!
!        We can compute the East and North vectors
!        without much loss of precision, since VLON is
!        orthogonal to Z and EAST is orthogonal to V.
!
         call ucrss ( z, vlon, east  )
         call ucrss ( v, east, north )

         call vhat  ( v, rad )

!
!        The rows of M are the basis vectors of
!        the radial/East/North frame:
!
         do i = 1, 3

            m(1,i) = rad  (i)
            m(2,i) = east (i)
            m(3,i) = north(i)

         end do

      end if

      end

